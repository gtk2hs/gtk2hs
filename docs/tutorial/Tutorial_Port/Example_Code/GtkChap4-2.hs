import Graphics.UI.Gtk

main :: IO ()
main = do
  initGUI
  window <- windowNew
  set window [windowTitle := "Range Controls",
              windowDefaultWidth := 250 ]
  mainbox <- vBoxNew False 10
  containerAdd window mainbox
  containerSetBorderWidth mainbox 10

  box1 <- hBoxNew False 0
  boxPackStart mainbox box1 PackGrow 0
  adj1 <- adjustmentNew 0.0 0.0 101.0 0.1 1.0 1.0
  vsc <- vScaleNew adj1
  boxPackStart box1 vsc PackGrow 0

  box2 <- vBoxNew False 0
  boxPackStart box1 box2 PackGrow 0 

  hsc1 <- hScaleNew adj1
  boxPackStart box2 hsc1 PackGrow 0
  hsc2 <- hScaleNew adj1
  boxPackStart box2 hsc2 PackGrow 0

  chb <- checkButtonNewWithLabel "Display Value on Scale Widgets"
  boxPackStart mainbox chb PackNatural 10
  toggleButtonSetActive chb True

  box3 <- hBoxNew False 10
  boxPackStart mainbox box3 PackNatural 0
  label1 <- labelNew (Just "Scale Value Position:")
  boxPackStart box3 label1 PackNatural 0
  opt1 <- makeOpt1
  boxPackStart box3 opt1 PackNatural 0

  box4 <- hBoxNew False 10
  boxPackStart mainbox box4 PackNatural 0
  label2 <- labelNew (Just "Scale Update Policy:")
  boxPackStart box4 label2 PackNatural 0
  opt2 <- makeOpt2
  boxPackStart box4 opt2 PackNatural 0

  adj2 <- adjustmentNew 1.0 0.0 5.0 1.0 1.0 0.0

  box5 <- hBoxNew False 0
  containerSetBorderWidth box5 10
  boxPackStart mainbox box5 PackGrow 0
  label3 <- labelNew (Just "Scale Digits:")
  boxPackStart box5 label3 PackNatural 10
  dsc <- hScaleNew adj2
  boxPackStart box5 dsc PackGrow 0
  scaleSetDigits dsc 0

  adj3 <- adjustmentNew 1.0 1.0 101.0 1.0 1.0 0.0

  box6 <- hBoxNew False 0
  containerSetBorderWidth box6 10
  boxPackStart mainbox box6 PackGrow 0
  label4 <- labelNew (Just "Scrollbar Page Size:")
  boxPackStart box6 label4 PackNatural 10
  psc <- hScaleNew adj3
  boxPackStart box6 psc PackGrow 0
  scaleSetDigits psc 0

  onToggled chb $ do toggleDisplay chb [hsc1,hsc2]
                     toggleDisplay chb [vsc]

  onChanged opt1 $ do setScalePos opt1 hsc1 
                      setScalePos opt1 hsc2
                      setScalePos opt1 vsc

  onChanged opt2 $ do setUpdatePol opt2 hsc1 
                      setUpdatePol opt2 hsc2
                      setUpdatePol opt2 vsc

  onValueChanged adj2 $ do setDigits hsc1 adj2
                           setDigits hsc2 adj2
                           setDigits vsc  adj2

  onValueChanged adj3 $ do val <- adjustmentGetValue adj3
                           adjustmentSetPageSize adj1 val

  widgetShowAll window
  onDestroy window mainQuit
  mainGUI

makeOpt1 :: IO ComboBox
makeOpt1 = do
  cb <- comboBoxNewText
  comboBoxAppendText cb "TOP" 
  comboBoxAppendText cb "BOTTOM"
  comboBoxAppendText cb "LEFT" 
  comboBoxAppendText cb "RIGHT"  
  comboBoxSetActive cb 0
  return cb

setScalePos :: ScaleClass self => ComboBox -> self -> IO ()
setScalePos cb sc = do
    ntxt <- comboBoxGetActiveText cb
    let pos = case ntxt of
                (Just "TOP") -> PosTop
                (Just "BOTTOM") -> PosBottom
                (Just "LEFT") -> PosLeft
                (Just "RIGHT") -> PosRight
                Nothing -> error "GtkChap9.hs setScalePos: no position set"
    scaleSetValuePos sc pos 

    
makeOpt2 :: IO ComboBox
makeOpt2 = do
  cb <- comboBoxNewText
  comboBoxAppendText cb "Continuous" 
  comboBoxAppendText cb "Discontinuous"
  comboBoxAppendText cb "Delayed" 
  comboBoxSetActive cb 0
  return cb

setUpdatePol :: RangeClass self => ComboBox -> self -> IO ()
setUpdatePol cb sc = do
    ntxt <- comboBoxGetActiveText cb
    let pol = case ntxt of
                (Just "Continuous") -> UpdateContinuous
                (Just "Discontinuous") -> UpdateDiscontinuous
                (Just "Delayed") -> UpdateDelayed
                Nothing -> error "GtkChap9.hs setUpdatePol: no policy set"
    rangeSetUpdatePolicy sc pol 

toggleDisplay :: ScaleClass self => CheckButton -> [self] -> IO ()
toggleDisplay b scls = sequence_ (map change scls) where 
                         change sc = do st <- toggleButtonGetActive b
                                        scaleSetDrawValue sc st

setDigits :: ScaleClass self => self -> Adjustment -> IO ()
setDigits sc adj = do val <- get adj adjustmentValue
                      set sc [scaleDigits := (round val) ]

 
