import Graphics.UI.Gtk

main :: IO ()
main = do
   initGUI
   window <- windowNew
   hbox <- hBoxNew True 10
   button1 <- buttonNewWithLabel "Button 1"
   button2 <- buttonNewWithLabel "Button 2"
   set window [windowDefaultWidth := 200, windowDefaultHeight := 200,
               containerBorderWidth := 10, containerChild := hbox,
               windowTitle := "Packing" ]
   boxPackStart hbox button1 PackGrow 0
   boxPackStart hbox button2 PackGrow 0
   widgetShowAll window
   onDestroy window mainQuit
   mainGUI
