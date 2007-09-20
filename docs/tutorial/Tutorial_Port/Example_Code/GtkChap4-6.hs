import Graphics.UI.Gtk

main :: IO ()
main= do
     initGUI
     window <- windowNew
     set window [windowTitle := "Text Entry", containerBorderWidth := 10]

     vb <- vBoxNew False 0
     containerAdd window vb

     hb <- hBoxNew False 0
     boxPackStart vb hb PackNatural 0

     txtfield <- entryNew
     boxPackStart hb txtfield PackNatural 5
     button <- buttonNewFromStock stockInfo
     boxPackStart hb button PackNatural 0

     txtstack <- statusbarNew
     boxPackStart vb txtstack PackNatural 0
     id <- statusbarGetContextId txtstack "Line"

     widgetShowAll window
     widgetSetSensitivity button False

     onEntryActivate txtfield (saveText txtfield button txtstack id)
     onPressed button (statusbarPop txtstack id)
     onDestroy window mainQuit
     mainGUI

saveText :: Entry -> Button -> Statusbar -> ContextId -> IO ()
saveText fld b stk id = do
         txt <- entryGetText fld
         let mesg | txt == reverse txt = "\"" ++ txt ++ "\""  ++
                                         " is equal to its reverse"
                  | otherwise =  "\"" ++ txt ++ "\""  ++
                                 " is not equal to its reverse"
         widgetSetSensitivity b True
         msgid <- statusbarPush stk id mesg
         return ()
              





