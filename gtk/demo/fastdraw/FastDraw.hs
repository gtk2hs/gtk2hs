{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -O #-}

-- Example of an drawing graphics onto a canvas.
import Graphics.UI.Gtk

import Control.Applicative
import Prelude
import Data.IORef
import Graphics.Rendering.Cairo
import Foreign (allocaArray)
import Graphics.Rendering.Cairo.Types (PixelData)
import Foreign.Storable (Storable(..))
import Foreign.C (CUChar)


main = do
  initGUI
  dia <- dialogNew
  dialogAddButton dia stockOk ResponseOk
  contain <- castToBox <$> dialogGetContentArea dia
  canvas <- drawingAreaNew
  let w = 256
      h = 256
      chan = 4
      row = w * chan
      stride = row
  widgetSetSizeRequest canvas 256 256

  -- create the Pixbuf
  allocaArray (w * h * chan) $ \ pbData -> do

  -- draw into the Pixbuf
  doFromTo 0 (h-1) $ \y ->
    doFromTo 0 (w-1) $ \x -> do
      pokeByteOff pbData (2+x*chan+y*row) (fromIntegral x :: CUChar)
      pokeByteOff pbData (1+x*chan+y*row) (fromIntegral y :: CUChar)
      pokeByteOff pbData (0+x*chan+y*row) (0 :: CUChar)

  -- a function to update the Pixbuf
  blueRef <- newIORef (0 :: CUChar)
  dirRef <- newIORef True
  let updateBlue = do
        blue <- readIORef blueRef
        -- print blue
        doFromTo 0 (h-1) $ \y ->
          doFromTo 0 (w-1) $ \x ->
            pokeByteOff pbData (0+x*chan+y*row) blue  -- unchecked indexing

        -- arrange for the canvas to be redrawn now that we've changed
        -- the Pixbuf
        widgetQueueDraw canvas

        -- update the blue state ready for next time
        dir <- readIORef dirRef
        let diff = 1
        let blue' = if dir then blue+diff else blue-diff
        if dir then
          if blue<=maxBound-diff then writeIORef blueRef blue' else
            writeIORef blueRef maxBound >> modifyIORef dirRef not
          else
          if blue>=minBound+diff then writeIORef blueRef blue' else
            writeIORef blueRef minBound >> modifyIORef dirRef not
        return True

  idleAdd updateBlue priorityLow
  canvas `on` draw $ updateCanvas pbData w h stride
  boxPackStart contain canvas PackGrow 0
  widgetShow canvas
  dialogRun dia
  return ()

updateCanvas :: PixelData -> Int -> Int -> Int -> Render ()
updateCanvas pb w h stride = do
  s <- liftIO $ createImageSurfaceForData pb FormatRGB24 w h stride
  setSourceSurface s 0 0
  paint

-- GHC is much better at opimising loops like this:
--
-- > doFromTo 0 255 $ \y ->
-- >   doFromTo 0 255 $ \x -> do ...
--
-- Than it is at optimising loops like this:
--
-- > sequence_ [ do ...
-- >           | x <- [0..255]
-- >           , y <- [0..255] ]
--
-- The first kind of loop runs significantly faster (with GHC 6.2 and 6.4)

{-# INLINE doFromTo #-}
-- do the action for [from..to], ie it's inclusive.
doFromTo :: Int -> Int -> (Int -> IO ()) -> IO ()
doFromTo from to action =
  let loop n | n > to   = return ()
             | otherwise = do action n
                              loop (n+1)
   in loop from
