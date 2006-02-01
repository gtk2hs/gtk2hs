-- Test file for the ListView widget.
module Main(main) where

import Graphics.UI.Gtk hiding (listStoreNew)
import System.Directory
import System.Posix.Files
import Graphics.UI.Gtk.TreeList.ListStoreNew

data FileInfo = FileInfo {
  fName :: String,
  fSize :: Integer
}

main = do
  initGUI
  win <- windowNew
  win `onDestroy` mainQuit

  curDir <- getCurrentDirectory
  files <- getDirectoryContents curDir
  fInfos <- (flip mapM) files $ \f -> do
    status <- getFileStatus f
    return FileInfo { fName = f,
		      fSize = fromIntegral (fileSize status) }

  store <- listStoreNew fInfos

  tv <- treeViewNewWithModel store
  win `containerAdd` tv

  tvc <- treeViewColumnNew
  treeViewAppendColumn tv tvc

  text <- cellRendererTextNew
  treeViewColumnPackStart tvc text True
  cellLayoutSetAttributes tvc text store
    (\FileInfo { fName = name } -> [cellText := Just name])

  size <- cellRendererTextNew
  treeViewColumnPackStart tvc size True
  cellLayoutSetAttributes tvc size store
    (\FileInfo { fSize = size } -> [cellText := Just (show size)])

  widgetShowAll win
  mainGUI
