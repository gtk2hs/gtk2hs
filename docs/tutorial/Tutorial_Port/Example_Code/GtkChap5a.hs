-- GTK 2.0 Tutorial: Chapter 5a, Packing Widgets

import Graphics.UI.Gtk

main :: IO ()
main = do
   initGUI
   window <- windowNew
   vbox <- vBoxNew False 0
   set window [containerBorderWidth := 10, 
               windowTitle := "Packing Demonstration",
               containerChild := vbox ]
   label1 <- labelNew (Just "hBoxNew False 0")
   miscSetAlignment label1 0 0
   boxPackStart vbox label1 PackNatural 0
   box1 <- makeBox False 0 PackNatural 0
   boxPackStart vbox box1 PackNatural 0
   box2 <- makeBox False 0 PackRepel 0
   boxPackStart vbox box2 PackNatural 0 
   box3 <- makeBox False 0 PackGrow 0
   boxPackStart vbox box3 PackNatural 0
   sep1 <- hSeparatorNew
   boxPackStart vbox sep1 PackNatural 10  
   label2 <- labelNew (Just "hBoxNew True 0")
   miscSetAlignment label2 0 0
   boxPackStart vbox label2 PackNatural 0
   box4 <- makeBox True 0 PackNatural 0
   boxPackStart vbox box4 PackNatural 0
   box5 <- makeBox True 0 PackRepel 0
   boxPackStart vbox box5 PackNatural 0 
   box6 <- makeBox False 0 PackGrow 0
   boxPackStart vbox box6 PackNatural 0
   sep <- hSeparatorNew
   boxPackStart vbox sep PackNatural 10 
   quitbox <- hBoxNew False 0
   boxPackStart vbox quitbox PackNatural 0
   quitbutton <- buttonNewWithLabel "Quit"
   boxPackStart quitbox quitbutton PackRepel 0
   onClicked quitbutton mainQuit 
   onDestroy window mainQuit  
   widgetShowAll window
   mainGUI


makeBox :: Bool -> Int -> Packing -> Int -> IO HBox
makeBox homogeneous spacing packing padding = do
    box <- hBoxNew homogeneous spacing
    button1 <- buttonNewWithLabel "boxPackStart"
    boxPackStart box button1 packing padding
    button2 <- buttonNewWithLabel "box"
    boxPackStart box button2 packing padding
    button3 <- buttonNewWithLabel "button"
    boxPackStart box button3 packing padding
    button4 <- case packing of 
                   PackNatural -> buttonNewWithLabel "PackNatural"
                   PackRepel   -> buttonNewWithLabel "PackRepel"
                   PackGrow    -> buttonNewWithLabel "PackGrow"
    boxPackStart box button4 packing padding 
    button5 <- buttonNewWithLabel (show padding)
    boxPackStart box button5 packing padding  
    return box
