{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified GI.Gtk as GI (main, init)
import GI.Gtk
       (buttonBoxNew, buttonBoxSetChildSecondary, widgetShowAll,
        setButtonBoxLayoutStyle, buttonNewWithLabel, setContainerChild,
        setContainerBorderWidth, mainQuit, onWidgetDestroy, windowNew)
import GI.Gtk.Enums
       (Orientation(..), WindowType(..), ButtonBoxStyle(..))

main :: IO ()
main = do
  GI.init Nothing

  -- Create a new window
  window <- windowNew WindowTypeToplevel

  -- Here we connect the "destroy" event to a signal handler.
  onWidgetDestroy window mainQuit

  -- Sets the border width of the window.
  setContainerBorderWidth window 10

  hbuttonbox <- buttonBoxNew OrientationHorizontal

  setContainerChild window hbuttonbox

  button1 <- buttonNewWithLabel "One"
  button2 <- buttonNewWithLabel "Two"
  button3 <- buttonNewWithLabel "Three"

  -- Add each button to the button box with the default packing and padding
  mapM_ (setContainerChild hbuttonbox) [button1, button2, button3]

  -- This sets button3 to be a so called 'secondary child'. When the layout
  -- stlye is ButtonboxStart or ButtonboxEnd, the secondary children are
  -- grouped seperately from the others. Resize the window to see the effect.
  --
  -- This is not interesting in itself but shows how to set child attributes.
  -- Note that the child attribute 'buttonBoxChildSecondary' takes the
  -- button box container child 'button3' as a parameter.
  setButtonBoxLayoutStyle hbuttonbox ButtonBoxStyleStart
  buttonBoxSetChildSecondary hbuttonbox button3 True

  -- The final step is to display everything (the window and all the widgets
  -- contained within it)
  widgetShowAll window

  -- All Gtk+ applications must run the main event loop. Control ends here and
  -- waits for an event to occur (like a key press or mouse event).
  GI.main
