import Graphics.UI.Gtk  hiding (fill)
import Graphics.Rendering.Cairo


main :: IO ()
main= do
     initGUI
     window <- windowNew
     set window [windowTitle := "Hello Cairo Tutorial",
                 windowDefaultWidth := 300, windowDefaultHeight := 200,
                 containerBorderWidth := 15 ]

     frame <- frameNew
     containerAdd window frame
     canvas <- drawingAreaNew
     containerAdd frame canvas

     widgetShowAll window 
     onExpose canvas (\x ->  do (w,h) <- widgetGetSize canvas
                                drawin <- widgetGetDrawWindow canvas
                                renderWithDrawable drawin 
                                    (myDraw (fromIntegral w)(fromIntegral h))
                                return (eventSent x))
    
     onDestroy window mainQuit
     mainGUI

myDraw :: Double -> Double -> Render ()
myDraw w h = do
           setSourceRGB 1 1 1
           paint

           setSourceRGB 0 0 0
           moveTo 0 0
           lineTo w h
           moveTo w 0
           lineTo 0 h
           setLineWidth (0.1 * (h + w))
           stroke

           rectangle 0 0 (0.5 * w) (0.5 * h)
           setSourceRGBA 1 0 0 0.8
           fill

           rectangle 0 (0.5 * h) (0.5 * w) (0.5 * h)
           setSourceRGBA 0 1 0 0.6
           fill

           rectangle (0.5 * w) 0 (0.5 * w) (0.5 * h)
           setSourceRGBA 0 0 1 0.4
           fill
