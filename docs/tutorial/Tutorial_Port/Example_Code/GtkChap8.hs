import Graphics.UI.Gtk

main:: IO ()
main = do
     initGUI
     window <- windowNew
     set window [windowTitle := "Labels", containerBorderWidth := 10 ]
     mainbox <- vBoxNew False 10
     containerAdd window mainbox 
     hbox <- hBoxNew True 5
     boxPackStart mainbox hbox PackNatural 0
     vbox1 <- vBoxNew False 10
     vbox2 <- vBoxNew False 0
     boxPackStart hbox vbox1 PackNatural 0 
     boxPackStart hbox vbox2 PackNatural 0

     (label1,frame1) <- myLabelWithFrameNew
     boxPackStart vbox1 frame1 PackNatural 0
     labelSetText label1 "Penny Harter"

     (label2,frame2) <- myLabelWithFrameNew
     boxPackStart vbox1 frame2 PackNatural 0
     labelSetText label2 "broken bowl\nthe pieces\nstill rocking"
     miscSetAlignment label2 0.0 0.0
     hsep1 <- hSeparatorNew
     boxPackStart vbox1 hsep1 PackNatural 10

     (label3,frame3) <- myLabelWithFrameNew
     boxPackStart vbox1 frame3 PackNatural 0
     labelSetText label3 "Gary Snyder"

     (label4,frame4) <- myLabelWithFrameNew
     boxPackStart vbox1 frame4 PackNatural 0
     labelSetText label4 "After weeks of watching the roof leak\nI fixed it tonight\nby moving a single board"
     labelSetJustify label4 JustifyCenter

     (label5,frame5) <- myLabelWithFrameNew
     boxPackStart vbox2 frame5 PackNatural 0
     labelSetText label5 "Kobayashi Issa"
    
     (label7,frame7) <- myLabelWithFrameNew
     boxPackEnd vbox2 frame7 PackNatural 0
     labelSetText label7 "only one guy and\nonly one fly trying to\nmake the guest room do"
     labelSetJustify label7 JustifyRight

     (label6,frame6) <- myLabelWithFrameNew
     boxPackEnd vbox2 frame6 PackNatural 10
     labelSetText label6 "One Guy"
     frameSetLabel frame6 "Title:"
     labelSetPattern label6 [3,1,3]

     button <- buttonNew
     boxPackEnd mainbox button PackNatural 20
     buttonlabel <- labelNewWithMnemonic "Haiku _Clicked"
     containerAdd button buttonlabel

     widgetShowAll window
     onClicked button (putStrLn "button clicked...")
     onDestroy window mainQuit
     mainGUI


myLabelWithFrameNew :: IO (Label,Frame)
myLabelWithFrameNew = do
           label <- labelNew Nothing
           frame <- frameNew
           containerAdd frame label
           frameSetShadowType frame ShadowOut
           return (label, frame)

-- Haikus quoted from X.J. Kennedy, Dana Gioia, Introduction to Poetry, Longman, 1997
