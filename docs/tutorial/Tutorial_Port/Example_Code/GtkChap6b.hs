-- Chapter 7, Radio Button

-- packeds not good!

import Graphics.UI.Gtk

main :: IO ()
main = do
  initGUI
  window <- windowNew
  set window [windowTitle := "Radio Button", containerBorderWidth := 5 ,
              windowDefaultWidth := 200, windowDefaultHeight := 150 ]
  box1 <- vBoxNew False 0
  containerAdd window box1
  box2 <- vBoxNew False 10
  containerSetBorderWidth box2 10
  boxPackStart box1 box2 PackNatural 0
  button1 <- radioButtonNewWithLabel "button 1"
  boxPackStart box2 button1 PackNatural 0
  button2 <- radioButtonNewWithLabelFromWidget button1 "button 2"
  boxPackStart box2 button2 PackNatural 0
  button3 <- radioButtonNewWithLabelFromWidget button2 "button 3"
  boxPackStart box2 button3 PackNatural 0
  toggleButtonSetActive button2 True
  onToggled button1 (setRadioState button1)
  onToggled button2 (setRadioState button2)
  onToggled button3 (setRadioState button3)
  sep <- hSeparatorNew
  boxPackStart box1 sep PackNatural 0
  box3 <- vBoxNew False 10
  containerSetBorderWidth box3 10
  boxPackStart box1 box3 PackNatural 0
  closeb <- buttonNewWithLabel "close"
  boxPackStart box3 closeb PackNatural 0
  onClicked closeb mainQuit
  widgetShowAll window
  onDestroy window mainQuit
  mainGUI

setRadioState :: RadioButton -> IO ()
setRadioState b = do
  state <- toggleButtonGetActive b
  label <- get b buttonLabel
  putStrLn ("State " ++ label ++ " now is " ++ (show state))


