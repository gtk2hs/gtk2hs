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
  openFilePreviewButton `onClicked` openOpenFileDialog mainWindow

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
    ResponseAccept -> do fileName <- fileChooserGetFilename dialog
                         putStrLn $ "you selected the folder " ++ show fileName
    ResponseCancel -> putStrLn "dialog canceled"
    ResponseDeleteEvent -> putStrLn "dialog closed"
  widgetHide dialog

openCreateFolderDialog :: Window -> IO ()
openCreateFolderDialog parentWindow = putStrLn "openCreateFolderDialog not yet implemented"

openOpenFileDialog :: Window -> IO ()
openOpenFileDialog parentWindow = putStrLn "openOpenFileDialog not yet implemented"

openSaveFileDialog :: Window -> IO ()
openSaveFileDialog parentWindow = putStrLn "openSaveFileDialog not yet implemented"

