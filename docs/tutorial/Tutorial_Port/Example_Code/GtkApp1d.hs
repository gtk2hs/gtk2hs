import Graphics.UI.Gtk  hiding (fill)
import Graphics.Rendering.Cairo

main :: IO ()
main= do
     initGUI
     window <- windowNew
     set window [windowTitle := "Save as...",
                 windowDefaultWidth := 300, windowDefaultHeight := 200] 

     let pnw = 300
         pnh = 200
     withImageSurface 
       FormatARGB32 pnw pnh (\srf -> do renderWith srf (myDraw (fromIntegral pnw) (fromIntegral pnh))
                                        surfaceWriteToPNG srf "myDraw.png")


     let pdw = 720
         pdh = 720
     withPDFSurface "myDraw.pdf" pdw pdh (\s ->  renderWith s $ do myDraw pdw pdh
                                                                   showPage )
     
     let psw = 360
         psh = 540
     withPSSurface 
        "myDraw.ps" psw psh (flip renderWith (myDraw psw psh >> showPage))

     let pgw = 180
         pgh = 360
     withSVGSurface 
        "myDraw.svg" pgw pgh (flip renderWith $ myDraw pgw pgh >> showPage)
         
     putStrLn "Press any key to quit..."
     onKeyPress window (\x -> do widgetDestroy window
                                 return (eventSent x))

     widgetShowAll window
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

