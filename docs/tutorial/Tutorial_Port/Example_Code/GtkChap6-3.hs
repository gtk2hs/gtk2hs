import Graphics.UI.Gtk

main :: IO ()
main = do
     initGUI
     window <- windowNew
     set window [windowTitle := "Alphabet" , windowDefaultWidth := 350,
             windowDefaultHeight := 350 , containerBorderWidth := 10]
     sw <- scrolledWindowNew Nothing Nothing
     set sw [scrolledWindowPlacement := CornerBottomRight, 
             scrolledWindowShadowType := ShadowEtchedIn,
             scrolledWindowHscrollbarPolicy := PolicyAutomatic,
             scrolledWindowVscrollbarPolicy := PolicyAutomatic ]
     containerAdd window sw

     layt <- layoutNew Nothing Nothing
     layoutSetSize layt myLayoutWidth myLayoutHeight
     widgetModifyBg layt StateNormal (Color 65535 65535 65535)
     containerAdd sw layt     
 
     upleft  <- labelNew (Just "‡(0,0)")
     layoutPut layt upleft 0 0
     upright <- labelNew (Just ("‡(" ++ (show (myLayoutWidth - 50)) ++",0)"))
     layoutPut layt upright (myLayoutWidth -50)  0
     dwnright <- labelNew (Just ("‡(0," ++ (show (myLayoutHeight -20)) ++ ")"))
     layoutPut layt dwnright 0 (myLayoutHeight -20)
     dwnleft <- labelNew (Just ("‡(" ++ (show(myLayoutWidth -70)) ++ "," ++
                                  (show (myLayoutHeight -20)) ++ ")"))
     layoutPut layt dwnleft (myLayoutWidth -70) (myLayoutHeight - 20)
     
     labels <- sequence $ map (labelNew . Just) txtls
     sequence_ $ map (\x -> widgetModifyFg x StateNormal (Color 0 0 45000)) labels
     
     let wnums = zip labels [0..]
     sequence_ $ map (myLayoutPut layt) wnums     

     widgetShowAll window
     onDestroy window mainQuit
     mainGUI

-- parameters
myLayoutWidth :: Int
myLayoutWidth = 800

myLayoutHeight :: Int
myLayoutHeight = 800

txtls :: [String]
txtls = map (\x -> x:[]) ['A'..'Z']
-- end parameters

step :: Double
step = (2 * pi)/(fromIntegral (length txtls))

ox :: Int
ox =  myLayoutWidth `div` 2

oy :: Int
oy = myLayoutHeight `div` 2

radius :: Double
radius = 0.25 * (fromIntegral ox)

angle :: Int -> Double
angle num = 1.5 * pi + (fromIntegral num) * step

num2x :: Int -> Int
num2x n = ox + relx where 
              relx = round $ radius * (cos (angle n))

num2y :: Int -> Int
num2y n = oy + rely where
              rely = round $ radius * (sin (angle n))

myLayoutPut :: Layout -> (Label, Int) -> IO ()
myLayoutPut lt (lb, n) = do 
         layoutPut lt lb (num2x n) (num2y n) 
         labelSetAngle lb (letterAngle n)

letterAngle :: Int -> Double
letterAngle n = (270 - degree) where
                    degree = (angle n) * (180.0 /pi)
