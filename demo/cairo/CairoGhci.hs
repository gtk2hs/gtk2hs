-- Example of an drawing graphics onto a canvas.
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Control.Monad.Trans ( liftIO )
import Control.Monad
import Control.Concurrent

run :: Render () -> IO ()
run act = do
  initGUI
  dia <- dialogNew
  dialogAddButton dia stockClose ResponseClose
  contain <- dialogGetUpper dia
  canvas <- drawingAreaNew
  canvas `onSizeRequest` return (Requisition 250 250)
  canvas `onExpose` updateCanvas canvas act 
  boxPackStartDefaults contain canvas
  widgetShow canvas
  dialogRun dia
  widgetDestroy dia
  -- run the main loop until all pending events are processed; this
  -- ensures that the window is actually closed which is necessary to
  -- allow ghci to display the prompt again
  let finish = do
	pen <- eventsPending
	when (pen>0) $ do
	  mainIterationDo False
	  finish
  finish


updateCanvas :: DrawingArea -> Render () -> Event -> IO Bool
updateCanvas canvas act (Expose {}) = do
  win <- drawingAreaGetDrawWindow canvas
  renderWithDrawable win act
  return True
updateCanvas canvas act _ = return False

setRed :: Render ()
setRed = do
  setSourceRGB 1 0 0



setFat :: Render ()
setFat = do
  setLineWidth 20
  setLineCap LineCapRound



drawSquare :: Double -> Double -> Render ()
drawSquare width height = do
  (x,y) <- getCurrentPoint
  lineTo (x+width) y
  lineTo (x+width) (y+height)
  lineTo x (y+height)
  closePath
  stroke





drawHCirc :: Double -> Double -> Double -> Render () 
drawHCirc x y radius = do
  arc x y radius 0 pi
  stroke



drawStr :: String -> Render ()
drawStr txt = do
  lay <- createLayout txt
  showLayout lay


 
drawStr_ :: String -> Render ()
drawStr_ txt = do
  lay <- liftIO $ do
    ctxt <- cairoCreateContext Nothing
    descr <- contextGetFontDescription ctxt 
    descr `fontDescriptionSetSize` 20
    ctxt `contextSetFontDescription` descr
    layoutText ctxt txt
  showLayout lay 
