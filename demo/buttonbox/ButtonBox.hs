module Main (Main.main) where

import Gtk

main :: IO ()
main = do
  initGUI

  -- Create a new window
  window <- windowNew

  -- Here we connect the "destroy" event to a signal handler.
  -- This event occurs when we call widgetDestroy on the window,
  -- or if we return FALSE in the "delete_event" callback.
  window `onDestroy` mainQuit

  -- Sets the border width of the window.
  containerSetBorderWidth window 10
  
  hbuttonbox <- hButtonBoxNew

  containerAdd window hbuttonbox

  button1 <- buttonNewWithLabel "One"
  button2 <- buttonNewWithLabel "Two"

  boxPackStart hbuttonbox button1 PackNatural 0
  boxPackStart hbuttonbox button2 PackNatural 0

  -- The final step is to display this newly created widgets
  widgetShowAll window
  -- and the window

  -- All GTK applications must have a Gtk.main. Control ends here and
  -- waits for an event to occur (like a key press or mouse event).
  mainGUI

