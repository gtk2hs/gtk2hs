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

import Gtk
import Mogul
import Glade

import ParseProfile

import Maybe (isJust, fromJust)
import Monad (when)
import List  (unfoldr, intersperse)
import Data.IORef

main :: IO ()
main = do
  -- our global state
  thresholdVar <- newIORef 0        --current cuttoff/threshhold value
  profileVar <- newIORef Nothing    --holds the current profile data structure
  
  -- initialisation stuff
  initGUI

  dialogXmlM <- xmlNew "ProfileViewer.glade"
  let dialogXml = case dialogXmlM of
        (Just dialogXml) -> dialogXml
        Nothing -> error $ "can't find the glade file \"ProfileViewer.glade\""
                        ++ "in the current directory"

  -- get a handle on a various objects from the glade file
  mainWindow <- xmlGetWidget dialogXml castToWindow "mainWindow"
  mainWindow `onDestroy` mainQuit

  mainView <- xmlGetWidget dialogXml castToTreeView "mainView"

  titleLabel <- xmlGetWidget dialogXml castToLabel "titleLabel"
  commandLabel <- xmlGetWidget dialogXml castToLabel "commandLabel"
  totalTimeLabel <- xmlGetWidget dialogXml castToLabel "totalTimeLabel"
  totalAllocLabel <- xmlGetWidget dialogXml castToLabel "totalAllocLabel"
  
  -- create the tree model
  skel <- emptyTreeSkel
  
  let createTextColumn name = do
        (attr, _, set) <- treeSkelAddAttribute skel cellText
        column <- newTreeViewColumn
        column `treeViewColumnSetTitle` name
        mainView `treeViewAppendColumn` column
        renderer <- treeViewColumnNewText column True True
        renderer `treeViewColumnAssociate` [attr]
        return set

  -- create the various columns in both the model and view
  setCostCentre <- createTextColumn "Cost Centre"
  setModule     <- createTextColumn "Module"
  setEntries    <- createTextColumn "Entries"
  setIndiTime   <- createTextColumn "Individual %time"
  setIndiAlloc  <- createTextColumn "Individual %alloc"
  setInhTime    <- createTextColumn "Inherited %time"
  setInhAlloc   <- createTextColumn "Inherited %alloc"

  store <- newTreeStore skel
  mainView `treeViewSetModel` store

  -- this action clears the tree model and then populates it with the
  -- profile contained in the profileVar, taking into account the current
  -- threshold value kept in the thresholdVar 
  let repopulateTreeStore = do
        profile <- readIORef profileVar
        when (isJust profile)
             (repopulateTreeStore' $ fromJust profile)
      repopulateTreeStore' profile = do
        treeStoreClear store
        titleLabel `labelSetText` (title profile)
        commandLabel `labelSetText` (command profile)
        totalTimeLabel `labelSetText` (show (totalTime profile) ++ " sec")
        totalAllocLabel `labelSetText` (formatNumber (totalAlloc profile) ++ " bytes")
        
	threshold <- readIORef thresholdVar
        let node = if threshold > 0
                     then pruneOnThreshold threshold (breakdown profile)
                     else Just (breakdown profile)
        when (isJust node)
             (addProfileNode Nothing (fromJust node))

      addProfileNode :: Maybe TreeIter -> ProfileNode -> IO ()
      addProfileNode parentNode profile = do
        iter <- treeStoreAppend store parentNode
        setCostCentre iter (costCentre profile)
        setModule     iter (moduleName profile)
        setEntries    iter (show $ entries profile)
        setIndiTime   iter (show $ fromIntegral (individualTime profile) / 10)
        setIndiAlloc  iter (show $ fromIntegral (individualAlloc profile)/ 10)
        setInhTime    iter (show $ fromIntegral (inheritedTime profile)  / 10)
        setInhAlloc   iter (show $ fromIntegral (inheritedAlloc profile) / 10)
	return iter
        mapM_ (addProfileNode (Just iter)) (children profile)

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
  
  -- each menu item in the "View" menu sets the thresholdVar and re-displays
  -- the current profile
  let doThresholdMenuItem threshold itemName = do
        menuItem <- xmlGetWidget dialogXml castToMenuItem itemName
        menuItem `onActivateLeaf` do writeIORef thresholdVar threshold
	                             repopulateTreeStore
  mapM_ (uncurry doThresholdMenuItem)
    [(0, "allEntries"), (1, "0.1%Entries"), (5, "0.5%Entries"), (10, "1%Entries"),
     (50, "5%Entries"), (100, "10%Entries"), (500, "50%Entries")]

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
  dialogRun dialog
  widgetHide dialog
  fileChooserGetFilename dialog
 
-- just to display a number using thousand seperators
-- eg "3,456,235,596"
formatNumber :: Integer -> String
formatNumber =
  reverse . concat . intersperse ","
  . unfoldr (\l -> case splitAt 3 l of
                     ([], _) -> Nothing
                     p       -> Just p)
  . reverse . show
