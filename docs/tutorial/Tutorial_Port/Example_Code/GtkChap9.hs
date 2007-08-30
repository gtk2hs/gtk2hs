import Graphics.UI.Gtk

main :: IO ()
main = do
     initGUI
     window <- windowNew
     set window [windowTitle := "Arrow Tips", 
                 windowDefaultWidth := 200,
                 windowDefaultHeight := 200, containerBorderWidth := 20 ]

     table <- tableNew 5 5 True
     containerAdd window table
     
     button1 <- buttonNew
     button2 <- buttonNew
     button3 <- buttonNew
     button4 <- buttonNew

     tableAttachDefaults table button1 0 1 2 3
     tableAttachDefaults table button2 2 3 0 1
     tableAttachDefaults table button3 4 5 2 3
     tableAttachDefaults table button4 2 3 4 5

     tlt <- tooltipsNew

     arrow1 <- arrowNew ArrowLeft ShadowEtchedIn 
     containerAdd button1 arrow1
     tooltipsSetTip tlt button1 "West" "T1"

     arrow2 <- arrowNew ArrowUp ShadowEtchedOut
     containerAdd button2 arrow2
     tooltipsSetTip tlt button2 "North" "T2"

     arrow3 <- arrowNew ArrowRight ShadowEtchedIn
     containerAdd button3 arrow3
     tooltipsSetTip tlt button3 "East" "T3"

     arrow4 <- arrowNew ArrowDown ShadowEtchedOut
     containerAdd button4 arrow4
     tooltipsSetTip tlt button4 "South" "T4"

     tooltipsEnable tlt
     widgetShowAll window
     onDestroy window mainQuit
     mainGUI


   
