import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

main :: IO ()
main= do
     initGUI
     window <- windowNew
     set window [windowTitle := "Hello Cairo with Resizing",
                 windowDefaultWidth := 300, windowDefaultHeight := 200,
                 containerBorderWidth := 30 ]

     frame <- frameNew
     containerAdd window frame
     canvas <- drawingAreaNew
     containerAdd frame canvas

     widgetShowAll window 
     onExpose canvas (\x -> do (w,h) <- widgetGetSize canvas
                               drw <- widgetGetDrawWindow canvas
                               renderWithDrawable drw 
                                   (myDraw (fromIntegral w) (fromIntegral h))
                               return (eventSent x))
    
     onDestroy window mainQuit
     mainGUI

myDraw :: Double -> Double -> Render ()
myDraw w h = do
    setSourceRGB 1 1 1
    paint

    setSourceRGB 1 1 0
    setLineWidth 5

    moveTo (0.5 * w) (0.43 * h)
    lineTo (0.33 * w) (0.71 * h)
    lineTo (0.66 * w) (0.71 * h)
    closePath
    stroke
