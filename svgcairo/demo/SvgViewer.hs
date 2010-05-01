
import System.Environment (getArgs)

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.SVG

main :: IO ()
main = do

  (file:_) <- getArgs
  svg <- svgNewFromFile file
  let (width, height) = svgGetSize svg

  initGUI
  dia <- dialogNew
  dialogAddButton dia stockOk ResponseOk
  contain <- dialogGetUpper dia
  canvas <- drawingAreaNew
  onSizeRequest canvas $ return (Requisition width height)
  canvas `on` exposeEvent $ updateCanvas canvas svg
  boxPackStartDefaults contain canvas
  widgetShow canvas
  dialogRun dia
  return ()

updateCanvas :: DrawingArea -> SVG -> EventM EExpose Bool
updateCanvas canvas svg = do
  win <- eventWindow
  liftIO $ do
  let (width, height) = svgGetSize svg
  (width', height') <- widgetGetSize canvas
  renderWithDrawable win $ do
    scale (realToFrac width'  / realToFrac width)
          (realToFrac height' / realToFrac height)
    svgRender svg
  return True
