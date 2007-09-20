import Graphics.UI.Gtk

main :: IO ()
main= do
     initGUI
     window <- windowNew
     set window [windowTitle := "Calendar",
                 windowDefaultWidth:= 200,
                 windowDefaultHeight:= 100]
     mainbox <- vBoxNew True 0
     containerAdd window mainbox

     hbox1 <- hBoxNew True 0
     boxPackStart mainbox hbox1 PackGrow 0

     cal <-calendarNew
     boxPackStart hbox1 cal PackGrow 0   

     vbox1 <- vBoxNew True 0
     frame1 <- frameNew
     set frame1 [frameLabel := "Display Options",
                 containerBorderWidth := 10,
                 frameLabelYAlign := 0.5, 
                 frameLabelXAlign := 0.5,
                 containerChild := vbox1 ]
     boxPackStart hbox1 frame1 PackGrow 0
     headingopt <- addDisplayOpt vbox1 "Show Heading"
     daynameopt <- addDisplayOpt vbox1 "Show Day Names"
     monchngopt <- addDisplayOpt vbox1 "No Month Change"
     weeknumopt <- addDisplayOpt vbox1 "Show Week Numbers"

     set headingopt [toggleButtonActive := True]
     set daynameopt [toggleButtonActive := True]

     reslabel <- labelNew Nothing
     showMess cal reslabel "Nothing Done Yet"
     frame2 <- frameNew
     set frame2 [frameLabel := "Last Action:",
                 containerBorderWidth := 10, 
                 containerChild := reslabel]
     boxPackStart mainbox frame2 PackGrow 0     

     mySetOnToggled headingopt cal calendarShowHeading
     mySetOnToggled daynameopt cal calendarShowDayNames
     mySetOnToggled monchngopt cal calendarNoMonthChange
     mySetOnToggled weeknumopt cal calendarShowWeekNumbers

     onDaySelected cal (showMess cal reslabel "Day Selected")
     onDaySelectedDoubleClick cal 
            (showMess cal reslabel "Double Click Day Selected")

     widgetShowAll window
     onDestroy window mainQuit
     mainGUI


addDisplayOpt :: VBox -> String -> IO CheckButton
addDisplayOpt box lbl = do
         cb <- checkButtonNewWithLabel lbl
         boxPackStart box cb PackGrow 5
         return cb

mySetOnToggled :: CheckButton -> Calendar -> 
               Attr Calendar Bool -> 
               IO (ConnectId CheckButton)
mySetOnToggled cb cl att = onToggled cb $ do
         cbstate <- get cb toggleButtonActive
         set cl [att := cbstate]

showMess :: Calendar -> Label -> String -> IO ()
showMess cal lbl str = do  
         (year, month, day) <- calendarGetDate cal
         labelSetText lbl $ str ++ "\n" ++ "Date = " ++
                      (show year) ++ "//" ++ 
                      (myshow (month +1))  -- month is 0 to 11
                       ++ "//" ++ (myshow day) 
                            where myshow n | n <= 9 = '0':(show n)
                                           | otherwise = show n

{- Commented out for platform specific testing:
These signals all seem to be implemented as onDaySelected.
The platform was: Gtk2Hs 0.9.12 on Fedora Core 6

     onMonthChanged cal (showMess cal reslabel "Month Changed")
     onNextMonth cal (showMess cal reslabel "Next Month Selected")
     onNextYear cal (showMess cal reslabel "Next Year Selected")
     onPrevMonth cal (showMess cal reslabel "Previous Month Selected")
     onPrevYear cal (showMess cal reslabel "Previous Year Selected")
-}
