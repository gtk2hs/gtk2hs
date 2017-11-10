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
  on window objectDestroy mainQuit
  -- Sets the border width and tile of the window. Note that border width
  -- attribute is in 'Container' from which 'Window' is derived.
  set window [ containerBorderWidth := 10, windowTitle := "Switch" ]
  -- Creates a new button with the label "Hello World".

  switch <- switchNew
  switchSetActive switch True

  set window [ containerChild := switch ]

  on switch stateSet $ \new_state -> do
    set window [ windowTitle := if new_state then "Switch on" else "Switch off" ]
    switchSetState switch new_state
    return True

  -- The final step is to display this newly created widget. Note that this
  -- also allocates the right amount of space to the windows and the button.
  widgetShowAll window
  -- All Gtk+ applications must have a main loop. Control ends here
  -- and waits for an event to occur (like a key press or mouse event).
  -- This function returns if the program should finish.
  mainGUI
