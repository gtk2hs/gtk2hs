-- A simple program to demonstrate Gtk2Hs.
module Main (Main.main) where

import Graphics.UI.Gtk

main :: IO ()
main = do
  initGUI
  -- Create a new window
  window <- windowNew
  -- Here we connect the "destroy" event to a signal handler.
  -- This event occurs when we call widgetDestroy on the window
  -- or if the user closes the window.
  onDestroy window mainQuit
  -- Sets the border width of the window. Note that this method is in
  -- 'Container' from which 'Window' is derived.
  set window [containerBorderWidth := 10]
  -- Creates a new button with the label "Hello World".
  buttonHW <- buttonNewWithLabel "Hello World"
  -- When the button receives the "clicked" signal, it will call the
  -- function given as the second argument.
  onClicked buttonHW (putStr "Hello World\n")
  -- Gtk allows several callbacks for the same event.
  -- This one will cause the window to be destroyed by calling
  -- widgetDestroy. The callbacks are called in the sequence they were added.
  onClicked buttonHW $ do
    putStrLn "A \"clicked\"-handler to say \"destroy\""
    widgetDestroy window
  -- Insert the hello-world button into the window.
  containerAdd window buttonHW
  -- The final step is to display this newly created widget. Note that this
  -- also allocates the right amount of space to the windows and the button.
  widgetShowAll window
  -- All GTK applications must have a main loop. Control ends here
  -- and waits for an event to occur (like a key press or mouse event).
  -- This function returns if the program should finish.
  mainGUI 

