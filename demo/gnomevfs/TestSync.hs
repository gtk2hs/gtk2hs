module Main where

import qualified System.Gnome.VFS as VFS
import Control.Exception
import Control.Monad (when)
import Data.Maybe (fromMaybe)
import System.IO
import System.Exit
import System.Environment
import qualified Data.ByteString as BS

handleVFSError vfsError =
    let VFS.Error result = vfsError
    in do hPutStrLn stderr $ "VFS error: " ++ show result
          exitFailure

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
           
           handle <- VFS.openURI uri VFS.OpenRead
           fileInfo <- VFS.getFileInfoFromHandle handle []
           let blockSize = fromMaybe 4096 $ VFS.fileInfoIOBlockSize fileInfo
           
           let loop = handleJust VFS.errors
                          (\(VFS.Error result) ->
                           case result of
                             VFS.ErrorEof -> return ()
                             _ -> handleVFSError $ VFS.Error result) $
                          do bytes <- VFS.read handle blockSize
                             BS.putStr bytes
                             loop
           loop
           
           VFS.close handle
