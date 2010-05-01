module Main where

import qualified System.Gnome.VFS as VFS
import Control.Exception ( handleJust )
import Control.Monad     ( when
                         , liftM )
import Data.Maybe        ( fromMaybe )
import Text.Printf       ( printf )
import System.Glib.MainLoop ( mainLoopNew
                            , mainLoopRun )
import System.IO
import System.Exit
import System.Environment

main :: IO ()
main =
    do VFS.init >>= (\success ->
                     when (not success) $
                          do hPutStrLn stderr $ "could not initialize GnomeVFS"
                             exitFailure)
       
       mainLoop <- mainLoopNew Nothing True
       
       putStrLn "Waiting for Volume mount/unmount events..."
       VFS.onVolumeMonitorVolumeMounted VFS.volumeMonitor $ \volume ->
           do VFS.volumeGetDisplayName volume >>= printf "volume-mounted: %s\n"
              return ()
       VFS.onVolumeMonitorVolumePreUnmount VFS.volumeMonitor $ \volume ->
           do VFS.volumeGetDisplayName volume >>= printf "volume-pre-unmount: %s\n"
              return ()
       VFS.onVolumeMonitorVolumeUnmounted VFS.volumeMonitor $ \volume ->
           do VFS.volumeGetDisplayName volume >>= printf "volume-unmounted: %s\n"
              return ()
       
       mainLoopRun mainLoop
       
       return ()
