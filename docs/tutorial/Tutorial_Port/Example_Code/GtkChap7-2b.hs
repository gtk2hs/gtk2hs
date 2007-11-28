import Graphics.UI.Gtk

main :: IO ()
main= do
     initGUI
     window <- windowNew
     set window [windowTitle := "Radio and Toggle Actions",
                 windowDefaultWidth := 400,
                 windowDefaultHeight := 200 ]
 
     mhma <- actionNew "MHMA" "Highlight\nMode" Nothing Nothing
     msma <- actionNew "MSMA" "Source"         Nothing Nothing
     mmma <- actionNew "MMMA" "Markup"         Nothing Nothing     

     agr1 <- actionGroupNew "AGR1"
     mapM_ (actionGroupAddAction agr1) [mhma,msma,mmma]
     actionGroupAddRadioActions agr1 hlmods 0 myOnChange

     vima <- actionNew "VIMA" "View" Nothing Nothing                       

     agr2 <- actionGroupNew "AGR2"
     actionGroupAddAction agr2 vima
     actionGroupAddToggleActions agr2 togls

     uiman <- uiManagerNew
     uiManagerAddUiFromString uiman uiDef1
     uiManagerInsertActionGroup uiman agr1 0

     uiManagerAddUiFromString uiman uiDef2
     uiManagerInsertActionGroup uiman agr2 1


     mayMenubar <- uiManagerGetWidget uiman "/ui/menubar"
     let mb = case mayMenubar of 
                    (Just x) -> x
                    Nothing -> error "Cannot get menu bar."

     mayToolbar <- uiManagerGetWidget uiman "/ui/toolbar"
     let tb = case mayToolbar of 
                    (Just x) -> x
                    Nothing -> error "Cannot get tool bar."

     box <- vBoxNew False 0
     containerAdd window box
     boxPackStart box mb PackNatural 0
     boxPackStart box tb PackNatural 0

     widgetShowAll window
     onDestroy window mainQuit
     mainGUI

hlmods :: [RadioActionEntry]
hlmods = [
     RadioActionEntry "NOA" "None"    Nothing Nothing Nothing 0,   
     RadioActionEntry "SHA" "Haskell" (Just stockHome)  Nothing Nothing 1, 
     RadioActionEntry "SCA" "C"       Nothing Nothing Nothing 2,
     RadioActionEntry "SJA" "Java"    Nothing Nothing Nothing 3,
     RadioActionEntry "MHA" "HTML"    Nothing Nothing Nothing 4,
     RadioActionEntry "MXA" "XML"     Nothing Nothing Nothing 5]

myOnChange :: RadioAction -> IO ()
myOnChange ra = do val <- radioActionGetCurrentValue ra
                   putStrLn ("RadioAction " ++ (show val) ++ " now active.")

uiDef1 = " <ui> \
\           <menubar>\
\              <menu action=\"MHMA\">\
\                 <menuitem action=\"NOA\" />\
\                 <separator />\
\                 <menu action=\"MSMA\">\
\                    <menuitem action= \"SHA\" /> \
\                    <menuitem action= \"SCA\" /> \
\                    <menuitem action= \"SJA\" /> \
\                 </menu>\
\                 <menu action=\"MMMA\">\
\                    <menuitem action= \"MHA\" /> \
\                    <menuitem action= \"MXA\" /> \
\                 </menu>\
\              </menu>\
\           </menubar>\
\            <toolbar>\
\              <toolitem action=\"SHA\" />\
\            </toolbar>\
\           </ui> "
            

togls :: [ToggleActionEntry]
togls = let mste = ToggleActionEntry "MST" "Messages"   Nothing Nothing Nothing
                                    (myTog mste)  False   
            ttte = ToggleActionEntry "ATT" "Attributes" Nothing Nothing Nothing
                                    (myTog ttte)  False 
            erte = ToggleActionEntry "ERT" "Errors"     (Just stockInfo) Nothing Nothing
                                    (myTog erte)  True 
        in [mste,ttte,erte]

myTog :: ToggleActionEntry -> IO ()
myTog te = putStrLn ("The state of " ++ (toggleActionName te) 
                      ++ " (" ++ (toggleActionLabel te) ++ ") " 
                      ++ " is now " ++ (show $ not (toggleActionIsActive te)))

uiDef2 = "<ui>\
\          <menubar>\
\            <menu action=\"VIMA\">\
\             <menuitem action=\"MST\" />\
\             <menuitem action=\"ATT\" />\
\             <menuitem action=\"ERT\" />\
\            </menu>\
\          </menubar>\
\            <toolbar>\
\              <toolitem action=\"MST\" />\
\              <toolitem action=\"ERT\" />\
\            </toolbar>\
\         </ui>"
