module Main where

import Gtk
import Glade

main :: IO ()
main = do
  initGUI

  -- load up our main window
  dialogXmlM <- xmlNew "FileChooserDemo.glade"
  let dialogXml = case dialogXmlM of
        (Just dialogXml) -> dialogXml
        Nothing -> error $ "can't find the glade file \"FileChooserDemo.glade\""
                        ++ "in the current directory"

  -- get a handle on a various objects from the glade file
  mainWindow <- xmlGetWidget dialogXml castToWindow "mainWindow"
  mainWindow `onDestroy` mainQuit
  
  -- and associate actions with the buttons
  selectFolderButton <- xmlGetWidget dialogXml castToButton "selectFolderButton"
  selectFolderButton `onClicked` openSelectFolderDialog mainWindow

  createFolderButton <- xmlGetWidget dialogXml castToButton "createFolderButton"
  createFolderButton `onClicked` openCreateFolderDialog mainWindow

  openFileButton <- xmlGetWidget dialogXml castToButton "openFileButton"
  openFileButton `onClicked` openOpenFileDialog mainWindow

  saveFileButton <- xmlGetWidget dialogXml castToButton "saveFileButton"
  saveFileButton `onClicked` openSaveFileDialog mainWindow

  openFilePreviewButton <- xmlGetWidget dialogXml castToButton "openFilePreviewButton"
  openFilePreviewButton `onClicked` openFilePreviewDialog mainWindow

  quitButton <- xmlGetWidget dialogXml castToButton "quitButton"
  quitButton `onClicked` mainQuit

  -- The final step is to display the main window and run the main loop
  widgetShowAll mainWindow
  mainGUI


openSelectFolderDialog :: Window -> IO ()
openSelectFolderDialog parentWindow = do
  dialog <- fileChooserDialogNew
              (Just $ "Demo of the standard dialog "
	           ++ "to select an existing folder")  --dialog title
              (Just parentWindow)                      --the parent window
	      FileChooserActionSelectFolder            --the kind of dialog we want
	      [("Yes, this new dialog looks nice"      --The buttons to display
	       , ResponseAccept)
	      ,("Eugh! Take me away!"
	       ,ResponseCancel)]
  widgetShow dialog
  response <- dialogRun dialog
  case response of 
    ResponseAccept -> do Just fileName <- fileChooserGetFilename dialog
                         putStrLn $ "you selected the folder " ++ show fileName
    ResponseCancel -> putStrLn "dialog canceled"
    ResponseDeleteEvent -> putStrLn "dialog closed"
  widgetHide dialog

openCreateFolderDialog :: Window -> IO ()
openCreateFolderDialog parentWindow = do
  dialog <- fileChooserDialogNew
              (Just $ "Demo of the standard dialog to select "
	           ++ "a new folder (or existing) folder")  --dialog title
              (Just parentWindow)                     --the parent window
	      FileChooserActionCreateFolder                 --the kind of dialog we want
	      [("I want this new folder"                    --The buttons to display
	       , ResponseAccept)
	      ,("Bored now."
	       ,ResponseCancel)]
  widgetShow dialog
  response <- dialogRun dialog
  case response of 
    ResponseAccept -> do Just fileName <- fileChooserGetFilename dialog
                         putStrLn $ "you selected the folder " ++ show fileName
    ResponseCancel -> putStrLn "Getting bored?"
    ResponseDeleteEvent -> putStrLn "dialog closed"
  widgetHide dialog

openOpenFileDialog :: Window -> IO ()
openOpenFileDialog parentWindow = do
  dialog <- fileChooserDialogNew
              (Just $ "Demo of the standard dialog to select "
	                 ++ "an existing file")             --dialog title
              (Just parentWindow)                     --the parent window
	      FileChooserActionOpen                         --the kind of dialog we want
	      [("gtk-cancel"                                --The buttons to display
	       ,ResponseCancel)
	      ,("gtk-open"                                  
	       , ResponseAccept)]

  widgetShow dialog
  response <- dialogRun dialog
  case response of 
    ResponseAccept -> do Just fileName <- fileChooserGetFilename dialog
                         putStrLn $ "you selected the file " ++ show fileName
    ResponseCancel -> putStrLn "dialog canceled"
    ResponseDeleteEvent -> putStrLn "dialog closed"
  widgetHide dialog

openSaveFileDialog :: Window -> IO ()
openSaveFileDialog parentWindow = do
  dialog <- fileChooserDialogNew
              (Just $ "Demo of the standard dialog to select "
	                 ++ "a new file")                   --dialog title
              (Just parentWindow)                     --the parent window
	      FileChooserActionSave                         --the kind of dialog we want
	      [("gtk-cancel"                                --The buttons to display
	       ,ResponseCancel)                             --you can use stock buttons
	      ,("gtk-save"
	       , ResponseAccept)]
  widgetShow dialog
  response <- dialogRun dialog
  case response of 
    ResponseAccept -> do Just fileName <- fileChooserGetFilename dialog
                         putStrLn $ "you called the new file " ++ show fileName
    ResponseCancel -> putStrLn "dialog canceled"
    ResponseDeleteEvent -> putStrLn "dialog closed"
  widgetHide dialog

openFilePreviewDialog :: Window -> IO ()
openFilePreviewDialog parentWindow = do
  dialog <- fileChooserDialogNew
              (Just $ "Demo of the standard dialog to select "
	           ++ "a new file - with a preview widget") --dialog title
              (Just parentWindow)                     --the parent window
	      FileChooserActionOpen                         --the kind of dialog we want
	      [("_Yes, yes that's very clever"              --The buttons to display
	       , ResponseAccept)
	      ,("_No, I'm not impressed"
	       ,ResponseCancel)]

  --create and set an extra widget
  checkButton <- checkButtonNewWithLabel "frobnicate this file"
  dialog `fileChooserSetExtraWidget` checkButton
  
  --create and set a preview widget
  previewLabel <- labelNew $ Just "Preview appears here"
  previewLabel `labelSetLineWrap` True
  dialog `fileChooserSetPreviewWidget` previewLabel
  dialog `onUpdatePreview` do
    previewFile <- fileChooserGetPreviewFilename dialog
    previewLabel `labelSetText` case previewFile of
      Nothing -> "Preview appears here"
      (Just filename) -> "Just pretend this is a preview of the file:\n" ++
                          show filename

  widgetShow dialog
  response <- dialogRun dialog
  case response of 
    ResponseAccept -> do fileName <- fileChooserGetFilename dialog
                         putStrLn $ "you selected the new file " ++ show fileName
                         
                         --check the state of the extra widget
                         frobnicate <- toggleButtonGetActive checkButton
                         putStrLn $ if frobnicate
                                      then "you foolishly decided to frobnicate the file"
                                      else "you wisely decided not to frobnicate the file"
    ResponseCancel -> putStrLn "you were not impressed"
    ResponseDeleteEvent -> putStrLn "dialog closed"
  widgetHide dialog

