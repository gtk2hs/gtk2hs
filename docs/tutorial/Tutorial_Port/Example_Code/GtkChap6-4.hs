import Graphics.UI.Gtk

main :: IO ()
main = do
     initGUI
     window <- windowNew
     set window [windowTitle := "Paned Window", containerBorderWidth := 10,
                 windowDefaultWidth := 400, windowDefaultHeight := 400 ]

     pw <- vPanedNew
     panedSetPosition pw 250
     containerAdd window pw
     af <- aspectFrameNew 0.5 0.5 (Just 3.0)
     frameSetLabel af "Aspect Ratio: 3.0"
     frameSetLabelAlign af 1.0 0.0
     panedAdd1 pw af

     da <- drawingAreaNew
     containerAdd af da
     widgetModifyBg da StateNormal (Color 65535 0 0)
   
     tv <- textViewNew
     panedAdd2 pw tv
     buf <- textViewGetBuffer tv

     onBufferChanged buf $ do cn <- textBufferGetCharCount buf
                              putStrLn (show cn)   

     widgetShowAll window 
     onDestroy window mainQuit
     mainGUI
