import Graphics.UI.Gtk

main :: IO ()
main= do
     initGUI
     window <- windowNew
     set window [windowTitle := "Click Right Popup",
                 windowDefaultWidth := 250,
                 windowDefaultHeight := 150 ]

     eda <- actionNew "EDA" "Edit" Nothing Nothing
     pra <- actionNew "PRA" "Process" Nothing Nothing
     rma <- actionNew "RMA" "Remove" Nothing Nothing
     saa <- actionNew "SAA" "Save" Nothing Nothing

     agr <- actionGroupNew "AGR1" 
     mapM_ (actionGroupAddAction agr) [eda,pra,rma,saa]

     uiman <- uiManagerNew
     uiManagerAddUiFromString uiman uiDecl
     uiManagerInsertActionGroup uiman agr 0

     maybePopup <- uiManagerGetWidget uiman "/ui/popup"
     let pop = case maybePopup of 
                    (Just x) -> x
                    Nothing -> error "Cannot get popup from string"

     onButtonPress window (\x -> if (eventButton x) == RightButton
                                    then do menuPopup (castToMenu pop) Nothing
                                            return (eventSent x)
                                    else return (eventSent x))

     mapM_ prAct [eda,pra,rma,saa]

     widgetShowAll window
     onDestroy window mainQuit
     mainGUI

uiDecl = "<ui> \
\          <popup>\
\            <menuitem action=\"EDA\" />\
\            <menuitem action=\"PRA\" />\
\            <menuitem action=\"RMA\" />\
\            <separator />\
\            <menuitem action=\"SAA\" />\
\          </popup>\
\        </ui>"

prAct :: ActionClass self => self -> IO (ConnectId self)
prAct a = onActionActivate a $ do name <- actionGetName a
                                  putStrLn ("Action Name: " ++ name)
