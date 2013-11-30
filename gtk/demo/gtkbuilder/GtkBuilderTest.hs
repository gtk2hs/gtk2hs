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
        on window deleteEvent $ liftIO mainQuit >> return False

        -- Display the window
        widgetShowAll window
        mainGUI
