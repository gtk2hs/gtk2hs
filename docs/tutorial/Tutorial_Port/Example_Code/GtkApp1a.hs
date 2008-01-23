import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

main :: IO ()
main= do
     initGUI
     window <- windowNew
     set window [windowTitle := "Hello Cairo",
                 windowDefaultWidth := 300, windowDefaultHeight := 200,
                 containerBorderWidth := 30 ]

     frame <- frameNew
     containerAdd window frame
     canvas <- drawingAreaNew
     containerAdd frame canvas
     widgetModifyBg canvas StateNormal (Color 65535 65535 65535)

     widgetShowAll window 
     drawin <- widgetGetDrawWindow canvas
     onExpose canvas (\x -> do renderWithDrawable drawin myDraw
                               return (eventSent x))
    
     onDestroy window mainQuit
     mainGUI

myDraw :: Render ()
myDraw = do
    setSourceRGB 1 1 0
    setLineWidth 5

    moveTo 120 60
    lineTo 60 110
    lineTo 180 110
    closePath

    stroke
