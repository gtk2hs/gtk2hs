module Main where

import qualified System.Gnome.VFS as VFS
import Control.Exception ( handleJust )
import Control.Monad     ( when
                         , liftM )
import Data.Maybe        ( fromMaybe )
import Text.Printf       ( printf )
import System.IO
import System.Exit
import System.Environment

handleVFSError vfsError =
    let VFS.Error result = vfsError
    in do hPutStrLn stderr $ "VFS error: " ++ show result
          exitFailure

xferProgressCallback :: VFS.XferProgressCallback
xferProgressCallback info =
    do printf "Status: %s\tPhase: %s\n"
           (show $ VFS.xferProgressInfoVFSStatus info)
           (show $ VFS.xferProgressInfoPhase info)
       printf "\tSource: %s\n\tTarget: %s\n"
           (show $ VFS.xferProgressInfoSourceName info)
           (show $ VFS.xferProgressInfoTargetName info)
       printf "\t%d of %d files\n"
           (toInteger $ VFS.xferProgressInfoFileIndex info)
           (toInteger $ VFS.xferProgressInfoFilesTotal info)
       printf "\t%s of %s\n"
           (VFS.formatFileSizeForDisplay $ VFS.xferProgressInfoBytesCopied info)
           (VFS.formatFileSizeForDisplay $ VFS.xferProgressInfoFileSize info)
       printf "\t%s of %s total\n"
           (VFS.formatFileSizeForDisplay $ VFS.xferProgressInfoTotalBytesCopied info)
           (VFS.formatFileSizeForDisplay $ VFS.xferProgressInfoBytesTotal info)
       return True

xferErrorCallback :: VFS.XferErrorCallback
xferErrorCallback info =
    do printf "error: %s; aborting transfer\n" $ show $ VFS.xferProgressInfoVFSStatus info
       return VFS.XferErrorActionAbort

xferOverwriteCallback :: VFS.XferOverwriteCallback
xferOverwriteCallback info =
    do printf "skipping file %s as it already exists\n" $ fromMaybe "unknown" $ VFS.xferProgressInfoSourceName info
       return VFS.XferOverwriteActionSkip

main :: IO ()
main =
    handleJust VFS.errors handleVFSError $
        do progName <- getProgName
           args <- getArgs
           
           when (length args /= 2) $
                do hPutStrLn stderr $ "Usage: " ++ progName ++ " source target"
                   exitFailure
           
           VFS.init >>= (\success ->
                         when (not success) $
                             do hPutStrLn stderr $ "could not initialize GnomeVFS"
                                exitFailure)
           
           hPutStrLn stderr "vfs initialized"
           
           let [source, target] = args
           
           hPutStrLn stderr "parsing source URI"
           
           sourceURI <- case VFS.uriFromString source of
                          Just sourceURI -> return sourceURI
                          Nothing        -> do hPutStrLn stderr $ "invalid source URI"
                                               exitFailure
           
           hPutStrLn stderr "parsing target URI"
           
           targetURI <- case VFS.uriFromString target of
                          Just targetURI -> return targetURI
                          Nothing        -> do hPutStrLn stderr $ "invalid target URI"
                                               exitFailure
           
           hPutStrLn stderr "executing transfer"
           
           VFS.xferURI sourceURI targetURI []
                       VFS.XferErrorModeQuery VFS.XferOverwriteModeQuery
                       (Just xferProgressCallback) (Just xferErrorCallback)
                       (Just xferOverwriteCallback) Nothing
           
           return ()
