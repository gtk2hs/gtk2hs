-- Example of an drawing graphics onto a canvas.
import Graphics.UI.Gtk

main = do
  initGUI
  dia <- dialogNew
  dialogAddButton dia stockOk ResponseOk
  contain <- dialogGetUpper dia
  canvas <- drawingAreaNew
  canvas `onSizeRequest` return (Requisition 40 40)
  text <- canvas `widgetCreateLayout` "Hello World."
  canvas `onExpose` updateCanvas canvas text
  boxPackStartDefaults contain canvas
  widgetShow canvas
  dialogRun dia
  return ()

updateCanvas :: DrawingArea -> PangoLayout -> Event -> IO Bool
updateCanvas canvas text (Expose { eventArea=rect }) = do
  win <- drawingAreaGetDrawWindow canvas
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

  drawLayoutWithColors win gc 30 (height `div` 2) text 
    (Just (Color 0 0 0)) Nothing

  return True
 
