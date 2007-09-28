import Graphics.UI.Gtk

main :: IO ()
main = do
     initGUI
     fchdal <- fileChooserDialogNew (Just "Save As...Dialog") Nothing
                                     FileChooserActionSave
                                     [("Cancel", ResponseCancel),
                                      ("Save", ResponseAccept),
                                      ("Backup", ResponseUser 100)]
 
     fileChooserSetDoOverwriteConfirmation fchdal True
     widgetShow fchdal
     response <- dialogRun fchdal
     case response of
          ResponseCancel -> putStrLn "You cancelled..."
          ResponseAccept -> do nwf <- fileChooserGetFilename fchdal
                               case nwf of
                                    Nothing -> putStrLn "Nothing"
                                    Just path -> putStrLn ("New file path is:\n" ++ path)
          ResponseUser 100 -> putStrLn "You pressed the backup button"
          ResponseDeleteEvent -> putStrLn "You closed the dialog window..."

     widgetDestroy fchdal
     onDestroy fchdal mainQuit
     mainGUI
