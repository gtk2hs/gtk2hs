module Main where

import Graphics.UI.Gtk
import Control.Monad.IO.Class (MonadIO(..))

main = do
        initGUI

        -- Create the builder, and load the UI file
        builder <- builderNew
        builderAddFromFile builder "simple.ui"

        -- Retrieve some objects from the UI
        window <- builderGetObject builder castToWindow "window1"
        button <- builderGetObject builder castToButton "button1"

        -- Basic user interation
        on button buttonActivated $ putStrLn "button pressed!"
        on window objectDestroy mainQuit

        -- Display the window
        widgetShowAll window
        mainGUI
