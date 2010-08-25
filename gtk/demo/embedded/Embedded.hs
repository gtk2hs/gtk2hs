-- Use GtkSocket and GtkPlug for cross-process embedded.
-- Just startup program, press 'm' to create tab with new button.
-- Click button for hang to simulate plug hanging process,
-- but socket process still running, can switch to other tab.

module Main where

import System.Process
import System.Environment
import System.Directory
import System.FilePath ((</>))
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.DrawWindow
import Graphics.UI.Gtk.Gdk.EventM

-- | Main.
main :: IO ()
main = do
  -- Get program arguments.
  args <- getArgs

  case args of
    -- Entry plug main when have two arguments.
    [id] -> plugMain (toNativeWindowId $ read id :: NativeWindowId) -- get GtkSocket id
    -- Othersise entry socket main when no arguments.
    _ -> socketMain

-- | GtkSocekt main.
socketMain :: IO ()
socketMain = do
  initGUI

  -- Create top-level window.
  window <- windowNew
  windowSetPosition window WinPosCenter
  windowSetDefaultSize window 600 400
  windowSetTitle window "Press `m` to new tab, press `q` exit."
  window `onDestroy` mainQuit

  -- Create notebook to contain GtkSocekt.
  notebook <- notebookNew
  window `containerAdd` notebook

  -- Handle key press.
  window `on` keyPressEvent $ tryEvent $ do
    keyName <- eventKeyName
    liftIO $
      case keyName of
        "m" -> do
               -- Create new GtkSocket.
               socket <- socketNew
               widgetShow socket                          -- must show before add GtkSocekt to container
               notebookAppendPage notebook socket "Tab"   -- add to GtkSocekt notebook
               id <- socketGetId socket                    -- get GtkSocket id

               -- Fork process to add GtkPlug into GtkSocekt.
               path <- liftM2 (</>) getCurrentDirectory getProgName -- get program full path
               runCommand $ path ++ " " ++ (show $ fromNativeWindowId id) -- don't use `forkProcess`
               return ()
        "q" -> mainQuit          -- quit

  widgetShowAll window

  mainGUI

-- | GtkPlug main.
plugMain :: NativeWindowId -> IO ()
plugMain id = do
  initGUI

  plug <- plugNew $ Just id
  plug `onDestroy` mainQuit

  button <- buttonNewWithLabel "Click me to hang."
  plug `containerAdd` button

  -- Simulate a plugin hanging to see if it blocks the outer process.
  button `onClicked` threadDelay 5000000

  widgetShowAll plug

  mainGUI
