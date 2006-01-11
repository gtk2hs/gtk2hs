-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Cairo.SVG
-- Copyright   :  (c) 2005 Duncan Coutts, Paolo Martini 
-- License     :  BSD-style (see cairo/COPYRIGHT)
--
-- Maintainer  :  gtk2hs-devel@lists.sourceforge.net
-- Stability   :  experimental
-- Portability :  portable
--
-- The SVG extension to the Cairo 2D graphics library.
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Cairo.SVG (
  -- * Convenience API

  -- | These operations render an SVG image directly in the current 'Render'
  -- contect. Because they operate in the cairo 'Render' monad they are
  -- affected by the current transformation matrix. So it is possible, for
  -- example, to scale or rotate an SVG image.
  --
  -- In the following example we scale an SVG image to a unit square:
  --
  -- > let (width, height) = svgGetSize in
  -- > do scale (1/width) (1/height)
  -- >    svgRender svg

  svgRenderFromFile,
  svgRenderFromHandle,
  svgRenderFromString,

  -- * Standard API

  -- | With this API there are seperate functions for loading the SVG and
  -- rendering it. This allows us to be more effecient in the case that an SVG
  -- image is used many times - since it can be loaded just once and rendered
  -- many times. With the convenience API above the SVG would be parsed and
  -- processed each time it is drawn.

  SVG,
  svgRender,
  svgGetSize,

  -- ** Block scoped versions

  -- | These versions of the SVG loading operations give temporary access
  -- to the 'SVG' object within the scope of the handler function. These
  -- operations guarantee that the resources for the SVG object are deallocated
  -- at the end of the handler block. If this form of resource allocation is
  -- too restrictive you can use the GC-managed versions below.
  --
  -- These versions are ofen used in the following style:
  --
  -- > withSvgFromFile "foo.svg" $ \svg -> do
  -- >   ...
  -- >   svgRender svg
  -- >   ...

  withSvgFromFile,
  withSvgFromHandle,
  withSvgFromString,

  -- ** GC-managed versions

  -- | These versions of the SVG loading operations use the standard Haskell
  -- garbage collector to manage the resources associated with the 'SVG' object.
  -- As such they are more convenient to use but the GC cannot give
  -- strong guarantees about when the resources associated with the 'SVG' object
  -- will be released. In most circumstances this is not a problem, especially
  -- if the SVG files being used are not very big.

  svgNewFromFile,
  svgNewFromHandle,
  svgNewFromString,
  ) where

import Control.Monad (liftM, when)
import Foreign hiding (rotate)
import CForeign
import Control.Exception (bracket)
import Control.Monad.Reader (ReaderT(..), runReaderT, ask, MonadIO, liftIO)
import System.IO

import Graphics.Rendering.Cairo.Internal hiding (Status(..))
{# import Graphics.Rendering.Cairo.Types #} hiding (Status(..))

{# context lib="svg-cairo" prefix="svg_cairo" #}

---------------------
-- Types
-- 

{# pointer *svg_cairo_t as SVG foreign newtype #}

{# enum status_t as Status {underscoreToCase} deriving(Eq, Show) #}

---------------------
-- Basic API
-- 

-- block scoped versions

withSvgFromFile :: FilePath -> (SVG -> Render a) -> Render a
withSvgFromFile file action =
  withSVG $ \svg -> do   
    liftIO $ svgParseFromFile file svg
    action svg

withSvgFromHandle :: Handle -> (SVG -> Render a) -> Render a
withSvgFromHandle hnd action =
  withSVG $ \svg -> do   
    liftIO $ svgParseFromHandle hnd svg
    action svg

withSvgFromString :: String -> (SVG -> Render a) -> Render a
withSvgFromString str action =
  withSVG $ \svg -> do
    liftIO $ svgParseFromString str svg
    action svg

withSVG :: (SVG -> Render a) -> Render a
withSVG =
  bracketR (alloca $ \svgPtrPtr -> do
             {# call unsafe create #} (castPtr svgPtrPtr)
             svgPtr <- peek (svgPtrPtr :: Ptr (Ptr SVG))
             svgPtr' <- newForeignPtr_ svgPtr
             return (SVG svgPtr'))
             
          {# call unsafe destroy #}

-- GC managed versions

svgNewFromFile :: FilePath -> IO SVG
svgNewFromFile file = do
  svg <- svgNew
  svgParseFromFile file svg
  return svg

svgNewFromHandle :: Handle -> IO SVG
svgNewFromHandle hnd = do
  svg <- svgNew
  svgParseFromHandle hnd svg
  return svg

svgNewFromString :: String -> IO SVG
svgNewFromString str = do
  svg <- svgNew
  svgParseFromString str svg
  return svg

svgNew :: IO SVG
svgNew =
  alloca $ \svgPtrPtr -> do
    {# call unsafe create #} (castPtr svgPtrPtr)
    svgPtr <- peek (svgPtrPtr :: Ptr (Ptr SVG))
    svgPtr' <- newForeignPtr svg_cairo_destroy_ptr svgPtr
    return (SVG svgPtr')

foreign import ccall unsafe "&svg_cairo_destroy"
  svg_cairo_destroy_ptr :: FinalizerPtr SVG

-- internal implementation

svgParseFromFile :: FilePath -> SVG -> IO ()
svgParseFromFile file svg =
  checkStatus $
  withCString file $ \filePtr -> 
    {# call parse #} svg filePtr

svgParseFromHandle :: Handle -> SVG -> IO ()
svgParseFromHandle hnd svg =
  allocaBytes 4096 $ \bufferPtr -> do
  checkStatus $ {# call parse_chunk_begin #} svg
  let loop = do
        count <- hGetBuf hnd bufferPtr 4096
        when (count > 0)
             (checkStatus $ {# call parse_chunk #}
                svg bufferPtr (fromIntegral count))
        when (count == 4096) loop
  loop
  checkStatus $ {# call parse_chunk_end #} svg

svgParseFromString :: String -> SVG -> IO ()
svgParseFromString str svg = do
  checkStatus $ {# call parse_chunk_begin #} svg
  let loop ""  = return ()
      loop str =
        case splitAt 4096 str of
          (chunk, str') -> do
            withCStringLen chunk $ \(chunkPtr, len) ->
              checkStatus $ {# call parse_chunk #}
                svg chunkPtr (fromIntegral len)
            loop str'
  loop str
  checkStatus $ {# call parse_chunk_end #} svg

-- actually render it

svgRender :: SVG -> Render ()
svgRender svg = do
  cr <- ask
  liftIO $ checkStatus $ {# call render #} svg cr

-- | Get the width and height of the SVG image.
--
svgGetSize :: 
    SVG
 -> (Int, Int) -- ^ @(width, height)@
svgGetSize svg = unsafePerformIO $
  alloca $ \widthPtr ->
  alloca $ \heightPtr -> do
  {# call unsafe get_size #} svg widthPtr heightPtr
  width <- peek widthPtr
  height <- peek heightPtr
  return (fromIntegral width, fromIntegral height)

---------------------
-- Convenience API
-- 

svgRenderFromFile :: FilePath -> Render ()
svgRenderFromFile file = withSvgFromFile file svgRender

svgRenderFromHandle :: Handle -> Render ()
svgRenderFromHandle hnd = withSvgFromHandle hnd svgRender

svgRenderFromString :: String -> Render ()
svgRenderFromString str = withSvgFromString str svgRender

---------------------
-- Utils
-- 

checkStatus :: IO CInt -> IO ()
checkStatus action = do
  status <- liftM (toEnum . fromIntegral) action
  if status == StatusSuccess
    then return ()
    else fail ("svg-cairo error: " ++ show status)
