-- Example of an drawing graphics onto a canvas. Note that this example
-- uses the old-style Gdk drawing functions. New implementations should
-- use Cairo. See examples in that directory.
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.EventM
import Control.Monad.Trans ( liftIO )

main = do
  initGUI
  dia <- dialogNew
  dialogAddButton dia stockOk ResponseOk
  contain <- dialogGetUpper dia
  canvas <- drawingAreaNew
  canvas `on` sizeRequest $ return (Requisition 40 40)
  text <- canvas `widgetCreateLayout` "Hello World."
  canvas `on` exposeEvent $ updateCanvas text
  boxPackStartDefaults contain canvas
  widgetShow canvas
  dialogRun dia
  return ()

updateCanvas :: PangoLayout -> EventM EExpose Bool
updateCanvas text = do
  win <- eventWindow
  liftIO $ do
  (width,height) <- drawableGetSize win
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
 
