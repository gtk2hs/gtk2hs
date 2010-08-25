module Main where

import Graphics.UI.Gtk

main = do
        initGUI

        -- Create the builder, and load the UI file
        builder <- builderNew
        builderAddFromFile builder "simple.ui"

        -- Retrieve some objects from the UI
        window <- builderGetObject builder castToWindow "window1"
        button <- builderGetObject builder castToButton "button1"

        -- Basic user interation
        button `onClicked` putStrLn "button pressed!"
        window `onDestroy` mainQuit

        -- Display the window
        widgetShowAll window
        mainGUI
