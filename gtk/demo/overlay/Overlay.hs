{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import GI.Gtk
       (overlayAddOverlay, widgetShowAll, setButtonBoxLayoutStyle,
        containerAdd, hButtonBoxNew, setWidgetMarginBottom,
        setWidgetMarginLeft, boxPackStart, buttonNewWithLabel, labelNew,
        vBoxNew, setContainerChild, overlayNew, setContainerBorderWidth,
        mainQuit, onWidgetDestroy, windowNew)
import qualified GI.Gtk as Gtk (main, init)
import GI.Gtk.Enums (ButtonBoxStyle(..), WindowType(..))

main :: IO ()
main = do
  Gtk.init Nothing

  -- Create a new window
  window <- windowNew WindowTypeToplevel

  -- Here we connect the "destroy" event to a signal handler.
  onWidgetDestroy window mainQuit

  -- Sets the border width of the window.
  setContainerBorderWidth window 10

  overlay <- overlayNew

  setContainerChild window overlay

  vbox <- vBoxNew True 3
  label1 <- labelNew (Just "This is an overlayed label")
  button <- buttonNewWithLabel "another one"
  label3 <- labelNew (Just "and a final one")
  boxPackStart vbox label1 False False 3
  boxPackStart vbox button False False 3
  boxPackStart vbox label3 False False 3

  setWidgetMarginLeft vbox 150
  setWidgetMarginBottom vbox 50

  overlayAddOverlay overlay vbox

  hbuttonbox <- hButtonBoxNew

  containerAdd overlay hbuttonbox

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

  -- The final step is to display everything (the window and all the widgets
  -- contained within it)
  widgetShowAll window

  -- All Gtk+ applications must run the main event loop. Control ends here and
  -- waits for an event to occur (like a key press or mouse event).
  Gtk.main
