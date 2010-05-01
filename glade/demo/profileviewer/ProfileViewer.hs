-- Copyright (c) 2004 Duncan Coutts
-- This program is liscenced under the GNU General Public License version 2
-- or (at your option) any later version.

-- This is a slightly larger demo that combines use of glade, the file chooser
-- dialog, program state (IORefs) and use of the mogul tree view wrapper
-- interface. 

-- The program is a simple viewer for the log files that ghc produces when you
-- do time profiling. The parser is not very clever so loading large files can
-- take several seconds.

-- TODO: The gui will appear to hang when loading files. We should use threads
-- to keep the gui responsive.

module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.ModelView as New

import ParseProfile

import Data.Maybe         (isJust, fromJust)
import Control.Monad      (when)
import Data.List          (unfoldr, intersperse)
import qualified Data.Tree as Tree
import System.Environment (getArgs)
import Data.IORef

main :: IO ()
main = do
  -- our global state
  thresholdVar <- newIORef 0        --current cuttoff/threshhold value
  profileVar <- newIORef Nothing    --holds the current profile data structure
  
  -- initialisation stuff
  initGUI

  Just dialogXml <- xmlNew "ProfileViewer.glade"

  -- get a handle on a various objects from the glade file
  mainWindow <- xmlGetWidget dialogXml castToWindow "mainWindow"
  onDestroy mainWindow mainQuit

  mainView <- xmlGetWidget dialogXml castToTreeView "mainView"

  titleLabel <- xmlGetWidget dialogXml castToLabel "titleLabel"
  commandLabel <- xmlGetWidget dialogXml castToLabel "commandLabel"
  totalTimeLabel <- xmlGetWidget dialogXml castToLabel "totalTimeLabel"
  totalAllocLabel <- xmlGetWidget dialogXml castToLabel "totalAllocLabel"
  
  -- create the tree model
  store <- New.treeStoreNew []
  New.treeViewSetModel mainView store

  let createTextColumn name field = do
        column <- New.treeViewColumnNew
        New.treeViewAppendColumn mainView column
        New.treeViewColumnSetTitle column name
        cell <- New.cellRendererTextNew
        New.treeViewColumnPackStart column cell True
        New.cellLayoutSetAttributes column cell store
          (\record -> [New.cellText := field record])

  -- create the various columns in both the model and view
  createTextColumn "Cost Centre"       costCentre
  createTextColumn "Module"            moduleName
  createTextColumn "Entries"           (show.entries)
  createTextColumn "Individual %time"  (show.(/10).fromIntegral.individualTime)
  createTextColumn "Individual %alloc" (show.(/10).fromIntegral.individualAlloc)
  createTextColumn "Inherited %time"   (show.(/10).fromIntegral.inheritedTime)
  createTextColumn "Inherited %alloc"  (show.(/10).fromIntegral.inheritedAlloc)

  -- this action clears the tree model and then populates it with the
  -- profile contained in the profileVar, taking into account the current
  -- threshold value kept in the thresholdVar 
  let repopulateTreeStore = do
        profile <- readIORef profileVar
        maybe (return ()) repopulateTreeStore' profile

      repopulateTreeStore' profile = do
        New.treeStoreClear store

        titleLabel `labelSetText` (title profile)
        commandLabel `labelSetText` (command profile)
        totalTimeLabel `labelSetText` (show (totalTime profile) ++ " sec")
        totalAllocLabel `labelSetText` (formatNumber (totalAlloc profile) ++ " bytes")
        
	threshold <- readIORef thresholdVar
        let node = if threshold > 0
                     then pruneOnThreshold threshold (breakdown profile)
                     else Just (breakdown profile)
            toTree :: ProfileNode -> Tree.Tree ProfileNode
            toTree = Tree.unfoldTree (\node -> (node, children node))
        case node of
          Nothing -> return ()
          Just node -> New.treeStoreInsertTree store [] 0 (toTree node)

  -- associate actions with the menus
  
  -- the open menu item, opens a file dialog and then loads and displays
  -- the the profile (unless the user cancleled the dialog)
  openMenuItem <- xmlGetWidget dialogXml castToMenuItem "openMenuItem"
  openMenuItem `onActivateLeaf` do
    filename <- openFileDialog mainWindow
    when (isJust filename)
         (do profile <- parseProfileFile (fromJust filename)
	     writeIORef profileVar (Just profile)
             repopulateTreeStore)

  quitMenuItem <- xmlGetWidget dialogXml castToMenuItem "quitMenuItem"
  quitMenuItem `onActivateLeaf` mainQuit
  
  aboutMenuItem <- xmlGetWidget dialogXml castToMenuItem "aboutMenuItem"
  aboutMenuItem `onActivateLeaf` showAboutDialog mainWindow
  
  -- each menu item in the "View" menu sets the thresholdVar and re-displays
  -- the current profile
  let doThresholdMenuItem threshold itemName = do
        menuItem <- xmlGetWidget dialogXml castToMenuItem itemName
        menuItem `onActivateLeaf` do writeIORef thresholdVar threshold
	                             repopulateTreeStore
  mapM_ (uncurry doThresholdMenuItem)
    [(0, "allEntries"), (1, "0.1%Entries"), (5, "0.5%Entries"), (10, "1%Entries"),
     (50, "5%Entries"), (100, "10%Entries"), (500, "50%Entries")]

  -- Check the command line to see if a profile file was given
  commands <- getArgs
  when (not (null commands))
       (do profile <- parseProfileFile (head commands)
           writeIORef profileVar (Just profile)
           repopulateTreeStore)

  -- The final step is to display the main window and run the main loop
  widgetShowAll mainWindow
  mainGUI


-- display a standard file open dialog
openFileDialog :: Window -> IO (Maybe String)
openFileDialog parentWindow = do
  dialog <- fileChooserDialogNew
              (Just "Open Profile... ")
              (Just parentWindow)
	      FileChooserActionOpen
	      [("gtk-cancel", ResponseCancel)
	      ,("gtk-open", ResponseAccept)]
  widgetShow dialog
  response <- dialogRun dialog
  widgetHide dialog
  case response of
      ResponseAccept -> fileChooserGetFilename dialog
      _ -> return Nothing

-- just to display a number using thousand seperators
-- eg "3,456,235,596"
formatNumber :: Integer -> String
formatNumber =
  reverse . concat . intersperse ","
  . unfoldr (\l -> case splitAt 3 l of
                     ([], _) -> Nothing
                     p       -> Just p)
  . reverse . show

showAboutDialog :: Window -> IO ()
showAboutDialog parent = do
  -- create the about dialog
  aboutDialog <- aboutDialogNew

  -- set some attributes
  set aboutDialog [
      aboutDialogName      := "profileviewer",
      aboutDialogVersion   := "0.2",
      aboutDialogCopyright := "Duncan Coutts",
      aboutDialogComments  := "A viewer for GHC time profiles.",
      aboutDialogWebsite   := "http://haskell.org/gtk2hs/"
    ]

  -- make the about dialog appear above the main window
  windowSetTransientFor aboutDialog parent

  -- make the dialog non-modal. When the user closes the dialog destroy it.
  afterResponse aboutDialog $ \_ -> widgetDestroy aboutDialog
  widgetShow aboutDialog
