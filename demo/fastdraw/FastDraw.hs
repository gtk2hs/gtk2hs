-- Example of an drawing graphics onto a canvas.
import Graphics.UI.Gtk

import Data.Array.MArray
import Data.Word

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
	     writeArray pbData (2+x*chan+y*row) (fromIntegral 0)
	     | x <- [0..255], y <- [0..255] ]
  canvas `onExposeRegion` updateCanvas canvas pb
  boxPackStartDefaults contain canvas
  widgetShow canvas
  dialogRun dia
  return ()

instance Show Rectangle where
  show (Rectangle x y w h) = "x="++show x++", y="++show y++
			     ", w="++show w++", h="++show h++";"

updateCanvas :: DrawingArea -> Pixbuf -> Region -> IO Bool
updateCanvas canvas pb region = do
  win <- drawingAreaGetDrawWindow canvas
  gc <- gcNew win
  (width,height) <- drawingAreaGetSize canvas
  rects <- regionGetRectangles region
  putStrLn ("redrawing: "++show rects)
  (flip mapM_) rects $ \(Rectangle x y w h) -> do
    drawPixbuf win gc pb x y x y w h RgbDitherNone 0 0
  return True
 
 
