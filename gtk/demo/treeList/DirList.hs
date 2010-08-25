{-# OPTIONS -cpp #-}
-- Test file for the ListView widget.
module Main(main) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as New

import Control.Exception
import System.Directory
import System.IO
import System.Locale
import System.Time

data FileInfo = FileInfo {
  fName :: String,
  fSize :: Integer,
  fTime :: ClockTime
}

main = do
  initGUI
  win <- windowNew
  win `onDestroy` mainQuit

  curDir <- getCurrentDirectory
  files <- getDirectoryContents curDir
  fInfos <- (flip mapM) files $ \f -> do
    s <- handle (\e ->
#if __GLASGOW_HASKELL__>=610
                 case e :: SomeException of
                   e ->
#endif
                        return 0) $ do
              h <- openFile f ReadMode
              s <- hFileSize h
              hClose h
              return s
    t <- getModificationTime f
    return FileInfo { fName = f
                    , fSize = s
                    , fTime = t }

  store <- New.listStoreNew fInfos

  tv <- New.treeViewNewWithModel store
  containerAdd win tv

  tvc <- New.treeViewColumnNew
  set tvc [ New.treeViewColumnTitle := "File name"
          , New.treeViewColumnResizable := True ]
  New.treeViewAppendColumn tv tvc

  name <- New.cellRendererTextNew
  New.treeViewColumnPackStart tvc name True
  New.cellLayoutSetAttributes tvc name store $ \FileInfo { fName = name } ->
    [ New.cellText := name ]

  tvc <- New.treeViewColumnNew
  set tvc [ New.treeViewColumnTitle := "Size"
          , New.treeViewColumnResizable := True ]
  New.treeViewAppendColumn tv tvc

  size <- New.cellRendererTextNew
  New.treeViewColumnPackStart tvc size True
  New.cellLayoutSetAttributes tvc size store $ \FileInfo { fSize = size } ->
    [ New.cellText := show size ]

  tvc <- New.treeViewColumnNew
  set tvc [ New.treeViewColumnTitle := "Modification time"
          , New.treeViewColumnResizable := True ]
  New.treeViewAppendColumn tv tvc

  time <- New.cellRendererTextNew
  New.treeViewColumnPackStart tvc time True
  New.cellLayoutSetAttributes tvc time store $ \FileInfo { fTime = time } ->
    [ New.cellText :=> do
        calTime <- toCalendarTime time
        return (formatCalendarTime defaultTimeLocale "%D %T" calTime)
    ]

  widgetShowAll win
  mainGUI
