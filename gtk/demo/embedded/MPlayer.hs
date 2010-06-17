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
  initGUI

  putStrLn "Please entry path of multimedia file : "
  filepath <- getLine
  
  mainWindow <- windowNew
  windowSetDefaultSize mainWindow 800 600
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

data MPlayer =
    MPlayer {mplayerSocket      :: Socket
            ,mplayerHandle      :: TVar (Maybe (Handle, Handle, Handle, ProcessHandle))}
                                                     
mplayerNew :: IO MPlayer
mplayerNew =
  MPlayer <$> socketNew
          <*> newTVarIO Nothing

mplayerStick :: MPlayer -> Container -> IO ()
mplayerStick (MPlayer {mplayerSocket = socket}) container = do
  widgetShowAll socket
  container `containerAdd` socket
  
mplayerRun :: MPlayer -> FilePath -> IO ()
mplayerRun (MPlayer {mplayerSocket = socket
                    ,mplayerHandle = mHandle}) filepath = do
  socketId <- liftM fromNativeWindowId $ socketGetId socket
  handle   <- runInteractiveCommand $ printf "mplayer %s -slave -wid %d" filepath (socketId :: Int)
  writeTVarIO mHandle (Just handle)
    
mplayerQuit :: MPlayer -> IO ()
mplayerQuit MPlayer {mplayerHandle = mHandle} = do
  handle <- readTVarIO mHandle
  case handle of
    Just (inp, _, _, _) -> hPutStr inp "quit\n"
    Nothing -> return ()

-- | The IO version of `writeTVar`.
writeTVarIO :: TVar a -> a -> IO ()
writeTVarIO a b = atomically $ writeTVar a b
