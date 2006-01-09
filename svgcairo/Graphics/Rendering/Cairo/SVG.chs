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
  renderSVGFromFile,
  renderSVGFromString,
  
  -- * Basic API
  SVG,
  renderSVG,
  sizeSVG,

  -- ** Block scoped versions
  withSVGFromFile,
  withSVGFromHandle,
  withSVGFromString,

  -- ** GC-managed versions
  newSVGFromFile,
  newSVGFromHandle,
  newSVGFromString,
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

withSVGFromFile :: FilePath -> (SVG -> Render a) -> Render a
withSVGFromFile file action =
  withSVG $ \svg -> do   
    liftIO $ parseSVGFromFile file svg
    action svg

withSVGFromHandle :: Handle -> (SVG -> Render a) -> Render a
withSVGFromHandle hnd action =
  withSVG $ \svg -> do   
    liftIO $ parseSVGFromHandle hnd svg
    action svg

withSVGFromString :: String -> (SVG -> Render a) -> Render a
withSVGFromString str action =
  withSVG $ \svg -> do
    liftIO $ parseSVGFromString str svg
    action svg

withSVG :: (SVG -> Render a) -> Render a
withSVG action = Render $ ReaderT $ \r ->
  bracket (alloca $ \svgPtrPtr -> do
            {# call unsafe create #} (castPtr svgPtrPtr)
            svgPtr <- peek (svgPtrPtr :: Ptr (Ptr SVG))
            svgPtr' <- newForeignPtr_ svgPtr
            return (SVG svgPtr'))
             
          {# call unsafe destroy #}
	  (\s -> runReaderT (runRender $ action s) r)

-- GC managed versions

newSVGFromFile :: FilePath -> IO SVG
newSVGFromFile file = do
  svg <- newSVG
  parseSVGFromFile file svg
  return svg

newSVGFromHandle :: Handle -> IO SVG
newSVGFromHandle hnd = do
  svg <- newSVG
  parseSVGFromHandle hnd svg
  return svg

newSVGFromString :: String -> IO SVG
newSVGFromString str = do
  svg <- newSVG
  parseSVGFromString str svg
  return svg

newSVG :: IO SVG
newSVG =
  alloca $ \svgPtrPtr -> do
    {# call unsafe create #} (castPtr svgPtrPtr)
    svgPtr <- peek (svgPtrPtr :: Ptr (Ptr SVG))
    svgPtr' <- newForeignPtr svg_cairo_destroy_ptr svgPtr
    return (SVG svgPtr')

foreign import ccall unsafe "&svg_cairo_destroy"
  svg_cairo_destroy_ptr :: FinalizerPtr SVG

-- internal implementation

parseSVGFromFile :: FilePath -> SVG -> IO ()
parseSVGFromFile file svg =
  checkStatus $
  withCString file $ \filePtr -> 
    {# call parse #} svg filePtr

parseSVGFromHandle :: Handle -> SVG -> IO ()
parseSVGFromHandle hnd svg =
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

parseSVGFromString :: String -> SVG -> IO ()
parseSVGFromString str svg = do
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

renderSVG :: SVG -> Render ()
renderSVG svg = do
  cr <- ask
  liftIO $ checkStatus $ {# call render #} svg cr

-- find out how big the thing is supposed to be.
sizeSVG :: SVG -> (Int, Int)
sizeSVG svg = unsafePerformIO $
  alloca $ \widthPtr ->
  alloca $ \heightPtr -> do
  {# call unsafe get_size #} svg widthPtr heightPtr
  width <- peek widthPtr
  height <- peek heightPtr
  return (fromIntegral width, fromIntegral height)

---------------------
-- Convenience API
-- 

renderSVGFromFile :: FilePath -> Render ()
renderSVGFromFile file = withSVGFromFile file renderSVG

renderSVGFromString :: String -> Render ()
renderSVGFromString str = withSVGFromString str renderSVG


---------------------
-- Utils
-- 

checkStatus :: IO CInt -> IO ()
checkStatus action = do
  status <- liftM (toEnum . fromIntegral) action
  if status == StatusSuccess
    then return ()
    else fail ("svg-cairo error: " ++ show status)
