-- | WebKit browser demo.
--  Author      :  Andy Stewart
--  Copyright   :  (c) 2010 Andy Stewart <lazycat.manatee@gmail.com>

-- | This simple browser base on WebKit API.
-- For simple, i just make all link open in current window.
-- Of course, you can integrate signal `createWebView` with `notebook`
-- to build multi-tab browser.
--
-- You can click right-button for forward or backward page.
--
-- Usage:
--      webkit [uri]
--
module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.Structs
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.WebFrame

import System.Process
import System.Environment 

-- | Main entry.
main :: IO ()
main = do
  -- Get program arguments.
  args <- getArgs
  case args of
    -- Display help
    ["--help"] -> do
       putStrLn $ "Welcome to Gtk2hs WebKit demo. :)\n\n" ++ 
                  "Usage: webkit [uri]\n\n" ++
                  "  -- Gtk2hs Team"
    -- Start program.
    [arg]    -> browser arg                     -- entry user input url
    _        -> browser "http://www.google.com" -- entry default url

-- | Internal browser fucntion.
browser :: String -> IO ()
browser url = do
  -- Init.
  initGUI
  
  -- Create window.
  window <- windowNew
  windowSetDefaultSize window 900 600
  windowSetPosition window WinPosCenter
  windowSetOpacity window 0.8   -- this function need window-manager support Alpha channel in X11

  -- Create WebKit view.
  webView <- webViewNew
  
  -- Create window box.
  winBox <- vBoxNew False 0
  
  -- Create address bar.
  addressBar <- entryNew

  -- Create scroll window.
  scrollWin <- scrolledWindowNew Nothing Nothing

  -- Load uri.
  webViewLoadUri webView url
  entrySetText addressBar url

  -- Open uri when user press `return` at address bar.
  onEntryActivate addressBar $ do
    uri <- entryGetText addressBar -- get uri from address bar
    webViewLoadUri webView uri    -- load new uri

  -- Add current uri to address bar when load start.
  webView `on` loadStarted $ \frame -> do
    currentUri <- webFrameGetUri frame
    case currentUri of
         Just uri -> entrySetText addressBar uri
         Nothing  -> return ()

  -- Open all link in current window.
  webView `on` createWebView $ \frame -> do
    newUri <- webFrameGetUri frame
    case newUri of
      Just uri -> webViewLoadUri webView uri
      Nothing  -> return ()
    return webView

  -- Connect and show.
  boxPackStart winBox addressBar PackNatural 0
  boxPackStart winBox scrollWin PackGrow 0
  window `containerAdd` winBox
  scrollWin `containerAdd` webView
  window `onDestroy` mainQuit
  widgetShowAll window

  mainGUI
