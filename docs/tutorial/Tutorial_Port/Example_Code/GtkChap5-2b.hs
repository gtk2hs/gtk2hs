import Graphics.UI.Gtk

main :: IO ()
main = do
     initGUI
     window <- windowNew
     set window [windowTitle := "File Chooser Button", windowDefaultWidth := 250, windowDefaultHeight := 75 ]
     fchd <- fileChooserButtonNew "Select Folder" FileChooserActionSelectFolder
     containerAdd window fchd

     onCurrentFolderChanged fchd $
          do dir <- fileChooserGetCurrentFolder fchd   
             case dir of
                  Nothing -> putStrLn "Nothing"
                  Just dpath -> putStrLn ("You selected:\n" ++ dpath)

     widgetShowAll window
     onDestroy window mainQuit
     mainGUI
