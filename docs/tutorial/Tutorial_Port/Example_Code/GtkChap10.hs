import Graphics.UI.Gtk

main :: IO ()
main = do
     initGUI
     
     dia <- dialogNew
     set dia [windowTitle := "Time Flies"]
     dialogAddButton dia stockApply  ResponseApply
     dialogAddButton dia stockCancel ResponseCancel
     dialogAddButton dia stockClose  ResponseClose

     pr <- progressBarNew
     progressBarSetPulseStep pr 1.0

     upbox <- dialogGetUpper dia
     boxPackStart upbox pr PackGrow 10
     widgetShowAll upbox
     
     answer <- dialogRun dia
     if answer == ResponseApply 
           then do tmhandle <- timeoutAdd (showPulse pr) 500
                   return ()
           else widgetDestroy dia
 
     onDestroy dia mainQuit
     mainGUI

showPulse :: ProgressBar -> IO Bool
showPulse b = do progressBarPulse b
                 return True
