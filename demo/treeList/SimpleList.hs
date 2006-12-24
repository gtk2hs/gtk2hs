-- Test file for the ListView widget.
module Main(main) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as New

import System.Directory
import System.Posix.Files

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

  store <- New.listStoreNew fInfos

  tv <- New.treeViewNewWithModel store
  win `containerAdd` tv

  tvc <- New.treeViewColumnNew
  New.treeViewAppendColumn tv tvc

  text <- New.cellRendererTextNew
  New.treeViewColumnPackStart tvc text True
  New.cellLayoutSetAttributes tvc text store
    (\FileInfo { fName = name } -> [New.cellText := name])

  size <- New.cellRendererTextNew
  New.treeViewColumnPackStart tvc size True
  New.cellLayoutSetAttributes tvc size store
    (\FileInfo { fSize = size } -> [New.cellText := show size])

  widgetShowAll win
  mainGUI
