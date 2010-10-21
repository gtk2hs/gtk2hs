-- | File Manager demo.
--  Author      :  Andy Stewart
--  Copyright   :  (c) 2010 Andy Stewart <lazycat.manatee@gmail.com>

-- | This simple file-manager base on gio.
--
module Main where

import Control.Monad
import Data.Maybe
import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.IconTheme
import Graphics.UI.Gtk.ModelView
import System.GIO
import System.Glib.GDateTime
import System.Glib.GError
import System.Locale
import System.Time
import Text.Printf

import qualified Data.ByteString.UTF8 as UTF8

data FMInfo = FMInfo {
  fIcon :: Pixbuf,               -- icon
  fName :: String,               -- file name
  fDesc :: String,               -- mime type description
  fSize :: Integer,              -- file size
  fTime :: ClockTime             -- modified time
}

-- | Main.
main :: IO ()
main = do
  -- Init.
  initGUI

  -- Create window.
  window <- windowNew
  windowSetDefaultSize window 900 600
  windowSetPosition window WinPosCenter
  scrolledWindow <- scrolledWindowNew Nothing Nothing
  window `containerAdd` scrolledWindow

  -- Get file infos under specify directory.
  infos <- directoryGetFileInfos "/"

  -- Get FMInfo.
  fInfos <- mapM (\info -> do
                   -- Get Icon.
                   icon <- fileInfoGetIcon info
                   iconTheme <- iconThemeGetDefault
                   iconInfo <- iconThemeLookupByGIcon iconTheme icon 24 IconLookupUseBuiltin
                   pixbuf <- case iconInfo of
                              Just ii -> iconInfoLoadIcon ii
                              Nothing -> liftM fromJust $ iconThemeLoadIcon iconTheme "unknown" 24 IconLookupUseBuiltin

                   let
                       -- Get file name.
                       name = fromJust $ fileInfoGetName info
                       -- File size.
                       size = toInteger $ fileInfoGetSize info
                       -- File modified time.
                       time = gTimeValToClockTime $ fileInfoGetModificationTime info
                       -- File mime description.
                       Just contentType = fileInfoGetContentType info
                       desc = contentTypeGetDescription contentType

                   return $ FMInfo pixbuf (UTF8.toString name) desc size time
                ) infos

  -- Initialize tree view.
  store <- listStoreNew fInfos
  tv <- treeViewNewWithModel store
  scrolledWindow `containerAdd` tv

  -- List Icons.
  tvc <- treeViewColumnNew
  set tvc [ treeViewColumnTitle := "Icon"
          , treeViewColumnResizable := True ]
  treeViewAppendColumn tv tvc

  icon <- cellRendererPixbufNew
  treeViewColumnPackStart tvc icon True
  cellLayoutSetAttributes tvc icon store $ \FMInfo { fIcon = icon } ->
    [ cellPixbuf := icon ]

  -- List Name.
  tvc <- treeViewColumnNew
  set tvc [ treeViewColumnTitle := "Name"
          , treeViewColumnResizable := True ]
  treeViewAppendColumn tv tvc

  name <- cellRendererTextNew
  treeViewColumnPackStart tvc name True
  cellLayoutSetAttributes tvc name store $ \FMInfo { fName = name } ->
    [ cellText := name ]

  -- List file mime description.
  tvc <- treeViewColumnNew
  set tvc [ treeViewColumnTitle := "Description"
          , treeViewColumnResizable := True ]
  treeViewAppendColumn tv tvc

  desc <- cellRendererTextNew
  treeViewColumnPackStart tvc desc True
  cellLayoutSetAttributes tvc desc store $ \FMInfo { fDesc = desc } ->
    [ cellText := desc ]

  -- List file size.
  tvc <- treeViewColumnNew
  set tvc [ treeViewColumnTitle := "Size"
          , treeViewColumnResizable := True ]
  treeViewAppendColumn tv tvc

  size <- cellRendererTextNew
  treeViewColumnPackStart tvc size True
  cellLayoutSetAttributes tvc size store $ \FMInfo { fSize = size } ->
    [ cellText := formatFileSizeForDisplay size
    , cellXAlign := 1.0]

  -- List modified time.
  tvc <- treeViewColumnNew
  set tvc [ treeViewColumnTitle := "Modified"
          , treeViewColumnResizable := True ]
  treeViewAppendColumn tv tvc

  time <- cellRendererTextNew
  treeViewColumnPackStart tvc time True
  cellLayoutSetAttributes tvc time store $ \FMInfo { fTime = time } ->
    [ cellText :=> do
        calTime <- toCalendarTime time
        return (formatCalendarTime defaultTimeLocale "%Y/%m/%d %T" calTime)]

  -- Show window.
  window `onDestroy` mainQuit
  widgetShowAll window

  mainGUI

directoryGetFileInfos :: FilePath -> IO [FileInfo]
directoryGetFileInfos directory = do
  let dir = fileFromPath (UTF8.fromString directory)
  enumerator <- fileEnumerateChildren dir "*" [] Nothing
  fileEnumeratorGetFileInfos enumerator

fileEnumeratorGetFileInfos :: FileEnumeratorClass enumerator => enumerator -> IO [FileInfo]
fileEnumeratorGetFileInfos enum = do
  fileInfo <- fileEnumeratorNextFile enum Nothing
  case fileInfo of
    Just info -> do
      infos <- fileEnumeratorGetFileInfos enum
      return $ info : infos
    Nothing -> return []

formatFileSizeForDisplay :: Integer -> String
formatFileSizeForDisplay size
    | size < 2 ^ 10  = humanSize 1 ++ " bytes"
    | size < 2 ^ 20  = humanSize (2 ^ 10)  ++ " KB"
    | size < 2 ^ 30  = humanSize (2 ^ 20)  ++ " MB"
    | size < 2 ^ 40  = humanSize (2 ^ 30)  ++ " GB"
    | size < 2 ^ 50  = humanSize (2 ^ 40)  ++ " TB"
    | size < 2 ^ 60  = humanSize (2 ^ 50)  ++ " PB"
    | size < 2 ^ 70  = humanSize (2 ^ 60)  ++ " EB"
    | size < 2 ^ 80  = humanSize (2 ^ 70)  ++ " ZB"
    | size < 2 ^ 90  = humanSize (2 ^ 80)  ++ " YB"
    | size < 2 ^ 100 = humanSize (2 ^ 90)  ++ " NB"
    | size < 2 ^ 110 = humanSize (2 ^ 100) ++ " DB"
    where humanSize base = printf "%.1f" (integralToDouble size / base) :: String

integralToDouble :: Integral a => a -> Double
integralToDouble v = fromIntegral v :: Double

gTimeValToClockTime :: GTimeVal -> ClockTime
gTimeValToClockTime GTimeVal {gTimeValSec  = seconds
                             ,gTimeValUSec = microseconds} =
  TOD (toInteger seconds) (toInteger microseconds * 1000)
