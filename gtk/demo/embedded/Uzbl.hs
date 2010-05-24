-- | This is program use uzbl embedded in window to render webpage.
-- Just simple model demo for view, haven't handle event or else. 
--
-- You need install uzbl (git clone git://github.com/Dieterbe/uzbl.git) first.
-- 
-- How to use: 
-- ./Uzbl       default open Google page.
-- ./Uzbl url   will open url you input
--
module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.DrawWindow
import System.Process
import System.Environment 

main :: IO ()
main = do
  -- Init.
  initGUI
  
  -- Get program arguments.
  args <- getArgs
  let url = case args of
              [arg] -> arg                      -- get user input url
              _     ->  "http://www.google.com" -- set default url

  -- Create window.
  window <- windowNew
  windowSetDefaultSize window 900 600
  windowSetPosition window WinPosCenter
  windowSetOpacity window 0.8   -- this function need window-manager support Alpha channel in X11
  
  -- Create socket.
  socket <- socketNew
  widgetShow socket             -- must show before add to parent
  window `containerAdd` socket

  -- Get socket id.
  socketId <- fmap (show . fromNativeWindowId) $ socketGetId socket

  -- Start uzbl-core process.
  runCommand $ "uzbl-core -s " ++ socketId ++ " -u " ++ url

  -- Show.
  window `onDestroy` mainQuit
  widgetShowAll window

  mainGUI
