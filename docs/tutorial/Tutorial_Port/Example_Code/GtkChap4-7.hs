import Graphics.UI.Gtk

main:: IO ()
main = do
     initGUI
     window <- windowNew
     mainbox <- vBoxNew False 0
     set window [windowTitle := "Spin Buttons", containerBorderWidth := 10,
                                 windowDefaultWidth := 250,
                                 windowDefaultHeight := 200,
                                 containerChild := mainbox ]
     hbox1 <- hBoxNew False 0 
     frame1 <- frameNew
     set frame1 [frameLabel := "Simple SpinButtons", containerChild := hbox1,
                 frameLabelYAlign := 0.8, frameShadowType := ShadowOut]              
     boxPackStart mainbox frame1 PackNatural 5
 
     spinD <- myAddSpinButton hbox1 "Day:" 1.0 31.0
     spinM <- myAddSpinButton hbox1 "Month:" 1.0 12.0
     spinY <- myAddSpinButton hbox1 "Year:" 2000.0 2100.0
     set spinY [spinButtonValue := 2007]

     vbox1 <- vBoxNew False 5
     frame2 <- frameNew
     set frame2 [frameLabel := "More Features", containerChild := vbox1,
                  frameLabelYAlign := 0.8, frameShadowType:= ShadowOut ]
     boxPackStart mainbox frame2 PackNatural 5
  
     hbox2 <- hBoxNew False 0
     boxPackStart vbox1 hbox2 PackNatural 0     

     spinLarge <- myAddSpinButton hbox2 "Value:" (-1000.0) 1000.0
     adj <- adjustmentNew 0.0 (-100.0) 100.0 0.25 10.0 0.0
     spinButtonConfigure spinLarge adj 0.0 2
     spnctl <- myAddSpinButton hbox2 "Decimal:" 0.0 10.0    
     set spnctl [spinButtonValue := 2.0]
   
     tsnap <- checkButtonNewWithLabel "Snap to 0.25-ticks"
     boxPackStart vbox1 tsnap PackNatural 0

     tnumr <- checkButtonNewWithLabel "Numeric only input mode"   
     boxPackStart vbox1 tnumr PackNatural 0

     twrap <- checkButtonNewWithLabel "Wraparound at limits"   
     boxPackStart vbox1 twrap PackNatural 0

     widgetShowAll window

     onValueSpinned spnctl $ do newdig <- get spnctl spinButtonValue
                                set spinLarge [spinButtonDigits := (round newdig)]

     onToggled tsnap $ do st <- get tsnap toggleButtonActive
                          set spinLarge [spinButtonSnapToTicks := st]

     onToggled tnumr $ do st <- get tnumr toggleButtonActive
                          set spinLarge [spinButtonNumeric := st]

     onToggled twrap $ do st <- get twrap toggleButtonActive
                          set spinLarge [spinButtonWrap := st]

     onDestroy window mainQuit
     mainGUI

myAddSpinButton :: HBox -> String -> Double -> Double -> IO SpinButton
myAddSpinButton box name min max = do
        vbox <- vBoxNew False 0
        boxPackStart box vbox PackRepel 0
        label <- labelNew (Just name)
        miscSetAlignment label 0.0 0.5
        boxPackStart vbox label PackNatural 0
        spinb <- spinButtonNewWithRange min max 1.0
        boxPackStart vbox spinb PackNatural 0
        return spinb



