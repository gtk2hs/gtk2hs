-- GTK 2.0 Tutorial: Chapter 5b, Table

import Graphics.UI.Gtk

main :: IO ()
main = do
   initGUI
   window <- windowNew
   set window [windowTitle := "Table", containerBorderWidth := 20,
               windowDefaultWidth := 150, windowDefaultHeight := 100 ]
   table <- tableNew 2 2 True
   containerAdd window table
   button1 <- buttonNewWithLabel "On"
   onClicked button1 (buttonSwitch button1)
   tableAttachDefaults table button1 0 1 0 1
   button2 <- buttonNewWithLabel "Off"
   onClicked button2 (buttonSwitch button2)
   tableAttachDefaults table button2 1 2 0 1
   button3 <- buttonNewWithLabel "Quit"
   onClicked button3 mainQuit
   tableAttachDefaults table button3 0 2 1 2
   onDestroy window mainQuit  
   widgetShowAll window
   mainGUI

buttonSwitch :: Button -> IO ()
buttonSwitch b = do
         txt <- buttonGetLabel b
         let newtxt = case txt of
                              "Off" -> "On"
                              "On"  -> "Off"
         buttonSetLabel b newtxt 


