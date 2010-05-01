module Main where

import qualified System.Gnome.VFS as VFS
import Control.Exception ( handleJust )
import Control.Monad     ( when
                         , liftM )
import Data.Maybe        ( fromMaybe )
import Text.Printf       ( printf )
import System.Time       ( ClockTime(..)
                         , calendarTimeToString
                         , toCalendarTime )
import System.IO
import System.Exit
import System.Environment

handleVFSError vfsError =
    let VFS.Error result = vfsError
    in do hPutStrLn stderr $ "VFS error: " ++ show result
          exitFailure

directoryVisitCallback :: String
                       -> VFS.FileInfo
                       -> Bool
                       -> IO VFS.DirectoryVisitResult
directoryVisitCallback name fileInfo recursingWillLoop =
    do mTimeStr <- case VFS.fileInfoMTime fileInfo of
                     Just mTime -> liftM calendarTimeToString $
                                       toCalendarTime $ TOD (fromIntegral $ fromEnum mTime) 0
                     Nothing    -> return "unknown"
       let name = fromMaybe "unknown" (VFS.fileInfoName fileInfo)
           size = VFS.formatFileSizeForDisplay (fromMaybe 0 (VFS.fileInfoSize fileInfo))
       
       printf "%20s  %20s  %s\n" size mTimeStr name
       return VFS.DirectoryVisitContinue

main :: IO ()
main =
     handleJust VFS.errors handleVFSError $
         do progName <- getProgName
            args <- getArgs
            
            when (length args /= 1) $
                 do hPutStrLn stderr $ "Usage: " ++ progName ++ " <uri>"
                    exitFailure
           
            VFS.init >>= (\success ->
                          when (not success) $
                               do hPutStrLn stderr $ "could not initialize GnomeVFS"
                                  exitFailure)
            
            let textURI = head args
            uri <- case VFS.uriFromString textURI of
                     Nothing -> do hPutStrLn stderr $ "Invalid URI: " ++ textURI
                                   exitFailure
                     Just uri -> return uri
            
            VFS.directoryVisit textURI [] [] directoryVisitCallback
