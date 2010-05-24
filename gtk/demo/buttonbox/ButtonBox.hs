module Main (main) where

import Graphics.UI.Gtk

main :: IO ()
main = do
  initGUI

  -- Create a new window
  window <- windowNew

  -- Here we connect the "destroy" event to a signal handler.
  -- This event occurs when we call widgetDestroy on the window,
  -- or if the user closes the window.
  window `onDestroy` mainQuit

  -- Sets the border width of the window.
  set window [ containerBorderWidth := 10 ]
  
  hbuttonbox <- hButtonBoxNew

  set window [ containerChild := hbuttonbox ]

  button1 <- buttonNewWithLabel "One"
  button2 <- buttonNewWithLabel "Two"
  button3 <- buttonNewWithLabel "Three"

  -- Add each button to the button box with the default packing and padding
  set hbuttonbox [ containerChild := button
                 | button <- [button1, button2, button3] ]
  
  -- This sets button3 to be a so called 'secondary child'. When the layout
  -- stlye is ButtonboxStart or ButtonboxEnd, the secondary children are
  -- grouped seperately from the others. Resize the window to see the effect.
  --
  -- This is not interesting in itself but shows how to set child attributes.
  -- Note that the child attribute 'buttonBoxChildSecondary' takes the
  -- button box container child 'button3' as a parameter.
  set hbuttonbox [ buttonBoxLayoutStyle := ButtonboxStart
		 , buttonBoxChildSecondary button3 := True ]

  -- The final step is to display everything (the window and all the widgets
  -- contained within it)
  widgetShowAll window

  -- All Gtk+ applications must run the main event loop. Control ends here and
  -- waits for an event to occur (like a key press or mouse event).
  mainGUI

