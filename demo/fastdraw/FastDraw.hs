{-# OPTIONS -O #-}

-- Example of an drawing graphics onto a canvas.
import Graphics.UI.Gtk

import Data.Array.MArray
import Data.Word
import Data.IORef
import Control.Monad ( when )
import Data.Array.Base ( unsafeWrite ) 


main = do
  initGUI
  dia <- dialogNew
  dialogAddButton dia stockOk ResponseOk
  contain <- dialogGetUpper dia
  canvas <- drawingAreaNew
  canvas `onSizeRequest` return (Requisition 256 256)

  -- create the Pixbuf
  pb <- pixbufNew ColorspaceRgb False 8 256 256
  pbData <- (pixbufGetPixels pb :: IO (PixbufData Int Word8))
  row <- pixbufGetRowstride pb
  chan <- pixbufGetNChannels pb
  bits <- pixbufGetBitsPerSample pb
  putStrLn ("bytes per row: "++show row++", channels per pixel: "++show chan++
	    ", bits per sample: "++show bits)

  -- draw into the Pixbuf
  doFromTo 0 255 $ \y ->
    doFromTo 0 255 $ \x -> do
      writeArray pbData (x*chan+y*row) (fromIntegral x)
      writeArray pbData (1+x*chan+y*row) (fromIntegral y)
      writeArray pbData (2+x*chan+y*row) 0

  -- a function to update the Pixbuf
  blueRef <- newIORef 0
  dirRef <- newIORef True 
  let updateBlue = do
        blue <- readIORef blueRef
	--print blue
	doFromTo 0 255 $ \y ->
          doFromTo 0 255 $ \x ->
	-- Here, writeArray was replaced with unsafeWrite. The latter does
	-- not check that the index is within bounds which has a tremendous
	-- effect on performance.
        --  writeArray  pbData (2+x*chan+y*row) blue  -- safe checked indexing
            unsafeWrite pbData (2+x*chan+y*row) blue  -- unchecked indexing

        -- arrange for the canvas to be redrawn now that we've changed
        -- the Pixbuf
	widgetQueueDraw canvas

        -- update the blue state ready for next time
        dir <- readIORef dirRef
	let diff = 4
	let blue' = if dir then blue+diff else blue-diff
	if dir then
	  if blue<=maxBound-diff then writeIORef blueRef blue' else
	    writeIORef blueRef maxBound >> modifyIORef dirRef not 
	  else
	  if blue>=minBound+diff then writeIORef blueRef blue' else
	    writeIORef blueRef minBound >> modifyIORef dirRef not 
	return True 
 
  idleAdd updateBlue priorityLow
  canvas `onExpose` updateCanvas canvas pb
  boxPackStartDefaults contain canvas
  widgetShow canvas
  dialogRun dia
  return ()

instance Show Rectangle where
  show (Rectangle x y w h) = "x="++show x++", y="++show y++
			     ", w="++show w++", h="++show h++";"

updateCanvas :: DrawingArea -> Pixbuf -> Event -> IO Bool
updateCanvas canvas pb Expose { region = region } = do
  win <- drawingAreaGetDrawWindow canvas
  gc <- gcNew win
  (width,height) <- drawingAreaGetSize canvas
  rects <- regionGetRectangles region
--  putStrLn ("redrawing: "++show rects)
  (flip mapM_) rects $ \(Rectangle x y w h) -> do
    drawPixbuf win gc pb x y x y (-1) (-1) RgbDitherNone 0 0
  return True
 
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
