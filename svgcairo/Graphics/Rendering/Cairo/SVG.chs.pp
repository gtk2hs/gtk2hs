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

import Control.Monad (when)
import Foreign
import Foreign.C
import Control.Monad.Reader (ask, liftIO)
import System.IO (Handle, openFile, IOMode(ReadMode), hGetBuf)

import System.Glib.GError (GError(GError), checkGError)
import System.Glib.GObject (GObjectClass(..), constructNewGObject, unGObject, mkGObject)

import Graphics.Rendering.Cairo.Internal (Render, bracketR)
{# import Graphics.Rendering.Cairo.Types #} (Cairo(Cairo))

{# context lib="librsvg" prefix="rsvg_handle" #}

---------------------
-- Types
-- 

{# pointer *RsvgHandle as SVG foreign newtype #}

mkSVG = SVG
unSVG (SVG o) = o

instance GObjectClass SVG where
  toGObject = mkGObject . castForeignPtr . unSVG
  unsafeCastGObject = mkSVG . castForeignPtr . unGObject

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
  bracketR (do
             {# call g_type_init #}
             svgPtr <- {# call unsafe new #}
             svgPtr' <- newForeignPtr_ svgPtr
             return (SVG svgPtr'))             
          (\(SVG fptr) -> withForeignPtr fptr $ \ptr ->
                            {# call unsafe g_object_unref #} (castPtr ptr))

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
svgNew = do
  {# call g_type_init #}
  constructNewGObject SVG {# call unsafe new #}


-- internal implementation

svgParseFromFile :: FilePath -> SVG -> IO ()
svgParseFromFile file svg = do
  hnd <- openFile file ReadMode
  svgParseFromHandle hnd svg

svgParseFromHandle :: Handle -> SVG -> IO ()
svgParseFromHandle hnd svg =
  allocaBytes 4096 $ \bufferPtr -> do
  let loop = do
        count <- hGetBuf hnd bufferPtr 4096
        when (count > 0)
             (checkStatus $ {# call unsafe rsvg_handle_write #}
                svg (castPtr bufferPtr) (fromIntegral count))
        when (count == 4096) loop
  loop
  checkStatus $ {# call unsafe rsvg_handle_close #} svg

svgParseFromString :: String -> SVG -> IO ()
svgParseFromString str svg = do
  let loop ""  = return ()
      loop str =
        case splitAt 4096 str of
          (chunk, str') -> do
            withCStringLen chunk $ \(chunkPtr, len) ->
              checkStatus $ {# call unsafe rsvg_handle_write #}
                svg (castPtr chunkPtr) (fromIntegral len)
            loop str'
  loop str
  checkStatus $ {# call unsafe rsvg_handle_close #} svg

-- actually render it

-- | render an SVG file
--
-- Returns @False@ if an error was detected.
-- On librsvg before 2.22.3, @svgRender@ always returns @True@.
svgRender :: SVG -> Render Bool
svgRender svg = do
  cr <- ask
  ret <- liftIO $ {# call unsafe render_cairo #} svg cr
#if ! LIBRSVG_CHECK_VERSION(2,22,3)
  return True
#else
  return (ret /= 0)
#endif

-- | Get the width and height of the SVG image.
--
svgGetSize :: 
    SVG
 -> (Int, Int) -- ^ @(width, height)@
svgGetSize svg = unsafePerformIO $
  allocaBytes {# sizeof RsvgDimensionData #} $ \dimentionsPtr -> do
  {# call unsafe get_dimensions #} svg dimentionsPtr
  width  <- {# get RsvgDimensionData->width  #} dimentionsPtr
  height <- {# get RsvgDimensionData->height #} dimentionsPtr
  return (fromIntegral width, fromIntegral height)

---------------------
-- Convenience API
-- 

svgRenderFromFile :: FilePath -> Render Bool
svgRenderFromFile file = withSvgFromFile file svgRender

svgRenderFromHandle :: Handle -> Render Bool
svgRenderFromHandle hnd = withSvgFromHandle hnd svgRender

svgRenderFromString :: String -> Render Bool
svgRenderFromString str = withSvgFromString str svgRender

---------------------
-- Utils
-- 

checkStatus :: (Ptr (Ptr ()) -> IO CInt) -> IO ()
checkStatus action =
  checkGError (\ptr -> action ptr >> return ())
    (\(GError domain code msg) -> fail ("svg cairo error: " ++ msg))
