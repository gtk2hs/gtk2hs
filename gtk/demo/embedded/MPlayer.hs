-- | MPlayer client demo
--  Author      :  Andy Stewart
--  Copyright   :  (c) 2010 Andy Stewart <lazycat.manatee@gmail.com>

module Main where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans
import Graphics.UI.Gtk
import System.Exit
import System.IO
import System.Process
import System.Environment 
import Text.Printf

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filepath] -> do
        initGUI
        
        mainWindow <- windowNew
        windowSetDefaultSize mainWindow 800 450
        windowSetPosition mainWindow WinPosCenter
        
        mplayer <- mplayerNew 
        mplayerStick mplayer (toContainer mainWindow) 
        
        mainWindow `afterShow` do 
          mplayerRun mplayer filepath
                     
          mainWindow `onDestroy` do
            mplayerQuit mplayer
            mainQuit
        
          return ()
        
        widgetShowAll mainWindow
        
        mainGUI
    _ -> putStrLn "Usage : mplayer file"

data MPlayer =
    MPlayer {mplayerWidget      :: DrawingArea
            ,mplayerHandle      :: TVar (Maybe (Handle, Handle, Handle, ProcessHandle))}
                                                     
mplayerNew :: IO MPlayer
mplayerNew =
  MPlayer <$> drawingAreaNew
          <*> newTVarIO Nothing

mplayerStick :: MPlayer -> Container -> IO ()
mplayerStick (MPlayer {mplayerWidget = mWidget}) container = do
  widgetShowAll mWidget
  container `containerAdd` mWidget
  
mplayerRun :: MPlayer -> FilePath -> IO ()
mplayerRun (MPlayer {mplayerWidget = mWidget
                    ,mplayerHandle = mHandle}) filepath = do
  drawWindow <- widgetGetDrawWindow mWidget -- you just can get DrawWindow after widget realized
  wid <- liftM fromNativeWindowId $ drawableGetID drawWindow
  handle <- runInteractiveCommand $ printf "mplayer %s -slave -wid %d" filepath (wid :: Int)
  writeTVarIO mHandle (Just handle)
    
mplayerQuit :: MPlayer -> IO ()
mplayerQuit MPlayer {mplayerHandle = mHandle} = do
  handle <- readTVarIO mHandle
  case handle of
    Just (inp, _, _, _) -> hPutStrLn inp "quit"
    Nothing -> return ()

-- | The IO version of `writeTVar`.
writeTVarIO :: TVar a -> a -> IO ()
writeTVarIO a b = atomically $ writeTVar a b
