{-# LANGUAGE OverloadedStrings #-}
-- A simple program to demonstrate Gtk2Hs.
module Main (Main.main) where

import qualified GI.Gtk as Gtk (main, init)
import GI.Gtk
       (widgetShowAll, setContainerChild, widgetDestroy, onButtonClicked,
        setButtonLabel, buttonNew, setWindowTitle, setContainerBorderWidth,
        mainQuit, onWidgetDestroy, windowNew)
import GI.Gtk.Enums (WindowType(..))

main :: IO ()
main = do
  Gtk.init Nothing
  -- Create a new window
  window <- windowNew WindowTypeToplevel
  -- Here we connect the "destroy" event to a signal handler.
  -- This event occurs when we call widgetDestroy on the window
  -- or if the user closes the window.
  onWidgetDestroy window mainQuit
  -- Sets the border width and tile of the window. Note that border width
  -- attribute is in 'Container' from which 'Window' is derived.
  setContainerBorderWidth window 10
  setWindowTitle window "Hello World"
  -- Creates a new button with the label "Hello World".
  button <- buttonNew
  setButtonLabel button "Hello World"
  -- When the button receives the "clicked" signal, it will call the
  -- function given as the second argument.
  onButtonClicked button (putStrLn "Hello World")
  -- Gtk+ allows several callbacks for the same event.
  -- This one will cause the window to be destroyed by calling
  -- widgetDestroy. The callbacks are called in the sequence they were added.
  onButtonClicked button $ do
    putStrLn "A \"clicked\"-handler to say \"destroy\""
    widgetDestroy window
  -- Insert the hello-world button into the window.
  setContainerChild window button
  -- The final step is to display this newly created widget. Note that this
  -- also allocates the right amount of space to the windows and the button.
  widgetShowAll window
  -- All Gtk+ applications must have a main loop. Control ends here
  -- and waits for an event to occur (like a key press or mouse event).
  -- This function returns if the program should finish.
  Gtk.main
