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
  pb <- pixbufNew ColorspaceRgb False 8 256 256
  pbData <- (pixbufGetPixels pb :: IO (PixbufData Int Word8))
  row <- pixbufGetRowstride pb
  chan <- pixbufGetNChannels pb
  bits <- pixbufGetBitsPerSample pb
  putStrLn ("bytes per row: "++show row++", channels per pixel: "++show chan++
	    ", bits per sample: "++show bits)
  sequence_ [writeArray pbData (x*chan+y*row) (fromIntegral x) >>
	     writeArray pbData (1+x*chan+y*row) (fromIntegral y) >>
	     writeArray pbData (2+x*chan+y*row) 0
	     | x <- [0..255], y <- [0..255] ]
  blueRef <- newIORef 0
  dirRef <- newIORef True 
  let updateBlue = do
        blue <- readIORef blueRef
	print blue
	let setBlue x i | x==256 = setBlue 0 (i-256*chan+row)
			| i>2+255*chan+255*row = return ()
			| otherwise =
			    unsafeWrite pbData i blue >> setBlue (x+1) (i+chan)
	-- Here, writeArray was replaced with unsafeWrite. The latter does
	-- not check that the index is within bounds which has a tremendous
	-- effect on performance.
	--sequence_ [ unsafeWrite pbData (x+y) blue
	--	   | x <- [2,(2+chan)..(2+255*chan)], y <- [0, row..(255*row)] ]
	setBlue 0 2
	widgetQueueDraw canvas
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
 
 
