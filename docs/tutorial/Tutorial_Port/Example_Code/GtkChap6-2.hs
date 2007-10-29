import Graphics.UI.Gtk
import System.Random (randomRIO)

main :: IO ()
main= do
     initGUI
     window <- windowNew
     set window [windowTitle := "Slot Machine",
                 containerBorderWidth := 10,
                 windowDefaultWidth := 350, 
                 windowDefaultHeight := 400]                 
     hb1 <- hBoxNew False 0
     containerAdd window hb1
     vb1 <- vBoxNew False 0
     boxPackStart hb1 vb1 PackGrow 0
     vbb <- vButtonBoxNew
     boxPackStart hb1 vbb PackGrow 0
     resetb <- buttonNewWithLabel "Reset"
     containerAdd vbb resetb
     quitb <- buttonNewWithLabel "Quit"
     containerAdd vbb quitb
     playb <- buttonNewWithMnemonic "_Play"
     containerAdd vbb playb
     set vbb [buttonBoxLayoutStyle := ButtonboxStart, 
              (buttonBoxChildSecondary playb) := True ]

     let picfiles = ["./jacunda.gif", "./pacu.gif", "./tucunaream.gif"]
     evimls <- sequence (map (initEvent vb1) picfiles)
     tips <- tooltipsNew
     sequence_ $ map ((myTooltip tips) . fst) evimls

     onClicked playb (play evimls picfiles)
 
     onClicked resetb $ sequence_ (zipWith reSet evimls picfiles)

     onClicked quitb (widgetDestroy window)
     widgetShowAll window
     onDestroy window mainQuit
     mainGUI

initEvent :: VBox -> FilePath -> IO (EventBox, Image)
initEvent vb picfile = do
              eb <- eventBoxNew
              boxPackStart vb eb PackGrow 0
              slot <- imageNewFromFile picfile
              set eb[containerChild := slot, containerBorderWidth := 10 ]
              widgetModifyBg eb StateNormal (Color 0 35000 0)
              widgetModifyBg eb StateInsensitive (Color 50000 50000 50000)
              onButtonPress eb 
                 (\x -> if (eventButton x) == LeftButton 
                           then do widgetSetSensitivity eb False 
                                   return (eventSent x)
                           else return (eventSent x))
              return (eb, slot)

reSet :: (EventBox, Image) -> FilePath -> IO ()
reSet (eb, im) pf = do widgetSetSensitivity eb True                 
                       imageSetFromFile im pf
                 
play :: [(EventBox, Image)] -> [FilePath] -> IO ()
play eilist fplist = 
   do let n = length fplist
      rands <- sequence $ replicate n (randomRIO (0::Int,(n-1)))
      sequence_ (zipWith display eilist rands) where
                     display (eb, im) rn = do
                                  state <- widgetGetState eb
                                  if state == StateInsensitive 
                                     then return ()
                                     else imageSetFromFile im (fplist !! rn)   

myTooltip :: Tooltips -> EventBox -> IO ()
myTooltip ttp eb = tooltipsSetTip ttp eb "Click Left Mouse Button to Freeze" ""
