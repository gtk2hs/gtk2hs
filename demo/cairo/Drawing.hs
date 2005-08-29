-- Example of an drawing graphics onto a canvas.
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Cairo
import qualified Graphics.Rendering.Cairo.Internal as Cairo

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
updateCanvas canvas text (Expose { area=rect }) = do
  win <- drawingAreaGetDrawWindow canvas
  (width,height) <- drawingAreaGetSize canvas

  cr <- cairoCreate win
  Cairo.setSourceRGB cr 1 0 0
  Cairo.setLineWidth cr 20
  Cairo.setLineCap cr Cairo.LineCapRound
  Cairo.setLineJoin cr Cairo.LineJoinRound

  Cairo.moveTo cr 30 30
  Cairo.lineTo cr (realToFrac width-30) (realToFrac height-30)
  Cairo.lineTo cr (realToFrac width-30) 30
  Cairo.lineTo cr 30 (realToFrac height-30)
  Cairo.stroke cr

  Cairo.setSourceRGB cr 1 1 0
  Cairo.setLineWidth cr 4
  
  Cairo.save cr
  Cairo.translate cr (realToFrac width / 2) (realToFrac height / 2)
  Cairo.scale cr (realToFrac width / 2) (realToFrac height / 2)
  Cairo.arc cr 0 0 1 (135 * pi/180) (225 * pi/180)
  Cairo.restore cr
  Cairo.stroke cr
  
  Cairo.setSourceRGB cr 0 0 0
  Cairo.moveTo cr 30 (realToFrac height / 2)
  Cairo.showText cr "Hello World."

  Cairo.destroy cr

  gc <- gcNew win
  drawLayoutWithColors win gc 30 (height `div` 2) text 
    (Just (Color 0 0 0)) Nothing

  return True
 
