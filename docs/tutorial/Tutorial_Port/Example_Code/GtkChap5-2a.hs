import Graphics.UI.Gtk

main :: IO ()
main = do
   initGUI
   window <- windowNew
   set window [windowTitle := "File Chooser Widget", 
               windowDefaultWidth := 500,
               windowDefaultHeight := 400 ]

   fch <- fileChooserWidgetNew FileChooserActionOpen
   containerAdd window fch 

   selopt <- checkButtonNewWithLabel "Multiple File Selection"
   fileChooserSetExtraWidget fch selopt

   hsfilt <- fileFilterNew
   fileFilterAddPattern hsfilt "*.hs"
   fileFilterSetName hsfilt "Haskell Source"   
   fileChooserAddFilter fch hsfilt

   nofilt <- fileFilterNew
   fileFilterAddPattern nofilt "*.*"
   fileFilterSetName nofilt "All Files"
   fileChooserAddFilter fch nofilt

   img <- imageNew
   fileChooserSetPreviewWidget fch img


   onUpdatePreview fch $ 
        do file <- fileChooserGetPreviewFilename fch
           case file of
                Nothing -> putStrLn "No File Selected"
                Just fpath -> imageSetFromFile img fpath

                           
   onFileActivated fch $ 
        do dir <- fileChooserGetCurrentFolder fch
           case dir of 
                Just dpath -> putStrLn 
                               ("The current directory is: " ++ dpath)
                Nothing -> putStrLn "Nothing" 
           mul <- fileChooserGetSelectMultiple fch 
           if mul 
              then do
                fls <- fileChooserGetFilenames fch
                putStrLn 
                  ("You selected " ++ (show (length fls)) ++" files:")
                sequence_ (map putStrLn fls)
              else do
                file <- fileChooserGetFilename fch
                case file of
                     Just fpath -> putStrLn ("You selected: " ++ fpath)
                     Nothing -> putStrLn "Nothing"

   onToggled selopt $ do state <- toggleButtonGetActive selopt
                         fileChooserSetSelectMultiple fch state

   widgetShowAll window
   onDestroy window mainQuit
   mainGUI
