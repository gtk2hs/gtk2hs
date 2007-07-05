module Main where

import qualified System.Gnome.VFS as VFS
import Control.Exception ( handleJust )
import Control.Monad     ( when
                         , liftM )
import Data.Maybe        ( fromMaybe )
import Text.Printf       ( printf )
import System.IO
import System.Exit

handleVFSError vfsError =
    let VFS.Error result = vfsError
    in do hPutStrLn stderr $ "VFS error: " ++ show result
          exitFailure

main :: IO ()
main =
    handleJust VFS.errors handleVFSError $
        do VFS.init >>= (\success ->
                         when (not success) $
                             do hPutStrLn stderr $ "could not initialize GnomeVFS"
                                exitFailure)
           
	   drives <- VFS.volumeMonitorGetConnectedDrives VFS.volumeMonitor
	   flip mapM_ drives $ \drive ->
	   	do VFS.driveGetDisplayName drive >>= printf "Drive %s:\n"
                   VFS.driveGetDeviceType drive >>= (printf "\tDevice Type: %s\n") . show
                   VFS.driveGetDevicePath drive >>=  (printf "\tDevice Path: %s\n") . show
                   volumes <- VFS.driveGetMountedVolumes drive
                   flip mapM_ volumes $ \volume ->
                       do VFS.volumeGetDisplayName volume >>= printf "\tVolume %s:\n"
                          VFS.volumeGetDevicePath volume >>=  printf "\t\tDevice Path: %s\n"
                          VFS.volumeGetFilesystemType volume >>= printf "\t\tFilesystem Type: %s\n"
           
           return ()
