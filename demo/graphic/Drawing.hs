-- Example of an drawing graphics onto a canvas.
import Prelude 	hiding (init)
import Gtk	hiding (main)	

main = do
  init Nothing
  dia <- dialogNew
  dialogAddButton dia stockButtonOk responseOk
  contain <- dialogGetUpper dia
  canvas <- drawingAreaNew
  canvas `onSizeRequest` return (Requisition 40 40)
  canvas `onExpose` updateCanvas canvas
  boxPackStartDefaults contain canvas
  widgetShow canvas
  dialogRun dia

updateCanvas :: DrawingArea -> Event -> IO Bool
updateCanvas canvas (Expose { area=rect }) = do
  win <- drawingAreaGetWindow canvas
  (width,height) <- drawingAreaGetSize canvas
  gc <- gcNew win
  gcSetValues gc $ newGCValues {
    foreground = Color 65535 0 0,
    capStyle = CapRound,
    lineWidth  = 20,
    joinStyle = JoinRound
  }
  drawLines win gc [(30,30),(width-30,height-30),(width-30,30),(30,height-30)]
  gcSetValues gc $ newGCValues {
    foreground = Color 65535 65535 0,
    lineWidth = 4
  }
  drawArc win gc False 0 0 width height (135*64) (90*64)
  return True
 