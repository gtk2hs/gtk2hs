-- program: S.A.R.A.H. road simulator
-- author: Maurício C. Antunes
-- e-mail: mauricio.antunes@gmail.com
-- license: public domain

module Main (Main.main) where
import Complex
import Data.Maybe
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Abstract.Widget
import Graphics.Rendering.Cairo hiding (translate)
import Graphics.Rendering.Cairo.Matrix
import Control.Monad
import Data.IORef
import Data.List
import Data.Char
import System.Time

mod1 :: Double -> Double
mod1 x = (x -) $ (fromIntegral.floor) x

-- car limits
acceleration = 0.9*carSize :: Double
desacceleration = 10*acceleration:: Double
carSize = 0.015 :: Double
-- time one takes to react to a change in the road
responseTime = 0.24 :: Double
halfMaxCars = 20 :: Integer

-- in a car list the last car should try to keep its position
-- before the first car position plus 1.0.  The actual position of
-- a car in the road is (mod1 position), i.e., 0 <= actual position
-- < 1
data Car = Car {position,speed::Double}
carPositionCompare c1 c2 = compare (position c1) (position c2)
carSpeedCompare c1 c2 = compare (speed c1) (speed c2)
newCarList n = map (((flip Car) 0).(1.0/(fromIntegral n) *).fromIntegral) [1..n]
changeCarListSize :: Int -> [Car] -> [Car]
changeCarListSize _ [] = []
changeCarListSize n carList = sortBy carPositionCompare $
        take n $ cycle $ sortBy carSpeedCompare carList

-- safe speed according to distance from next car
distance2speed distance = sqrt (b^2 + 2*d) - b
  where
    d = max 0 (distance*desacceleration)
    b = desacceleration*responseTime

-- update cars position and speed with a timestep
-- and maybe a congestion
updateCarList :: Maybe Double -> Double -> [Car] -> [Car]
updateCarList _ _ [] = []
updateCarsList congestion timestep carList = newList
  where
    positions = map position carList
    speeds = map speed carList
   
    -- distances considered to calculate speed are always 'responseTime'
    -- in the past, since human brain takes that time to react
    oldDistances = map (subtract carSize) $ zipWith (-) rotatedOldPositions oldPositions
      where
        oldPositions = zipWith (-) positions (map (responseTime *) speeds)
        rotatedOldPositions = (tail oldPositions) ++ [1 + (head oldPositions)]
    distancesToCongestion = congestion >>= \c -> Just $ map (mod1.(c - carSize/2 -)) positions

    speedFromDistances = map distance2speed oldDistances
    speedFromCongestion = distancesToCongestion >>= \d -> Just $ map distance2speed d
    desiredSpeed = case speedFromCongestion of
      Nothing -> speedFromDistances
      Just d -> zipWith min d speedFromDistances

    -- never change speeds more than given limits
    upperSpeed = map (+ timestep*acceleration) speeds
    lowerSpeed = map (subtract (timestep*desacceleration)) speeds
    finalSpeed = zipWith3 between lowerSpeed desiredSpeed upperSpeed
      where between x y z = max x (min y z)

    newList = zipWith updateSpeed carList finalSpeed
      where
        updateSpeed (Car p _) s = Car (p+s*timestep-base) s
        -- base is just to ensure that car positions
        -- do not get too big, since just (mod1 position)
        -- is what actually matter
        base = (fromIntegral . floor . head) positions

-- matrix to transform a coordinate space
-- so that a a circle with radius 1.0 can
-- fit inside window area
drawingMatrix :: DrawWindow -> IO Matrix
drawingMatrix dw = do
  (w_,h_) <- drawableGetSize dw
  (w,h) <- return (fromIntegral w_,fromIntegral h_)
  if (w*h)>0
    then do
      s <- return $ 0.85 * (min (w/2) (h/2))
      return $ (translate (w/2) (h/2)) . (scalarMultiply s) $ identity
    else
      return identity

main :: IO ()
main = do
  -- GTK stuff
  initGUI
  window <- windowNew
  set window [ containerBorderWidth := 10, windowTitle := "S.A.R.A.H.",
      windowWindowPosition := WinPosCenter]
  onDestroy window mainQuit
  hBox <- hBoxNew False 5
  hSeparator <- hSeparatorNew
  vBox <- vBoxNew False 0
  hButtonBox <- hButtonBoxNew
  scaleCarAmount <- vScaleNewWithRange 1 (fromIntegral (2*halfMaxCars)) 1
  mapM ($ scaleCarAmount) [(`scaleSetDigits` 0),(`scaleSetValuePos` PosTop),
          (`rangeSetUpdatePolicy` UpdateDelayed),(`rangeSetInverted` True)]
  scaleAdjustment <- rangeGetAdjustment scaleCarAmount
  scaleAdjustment `adjustmentSetValue` (fromIntegral halfMaxCars)
  [buttonReset,buttonAbout,buttonQuit] <- mapM buttonNewWithLabel ["Reset","About","Quit"]
  widgetSetCanFocus scaleCarAmount False
  mapM (`widgetSetCanFocus` False) [buttonReset,buttonAbout,buttonQuit]
  desenho <- drawingAreaNew
  desenho `onSizeRequest` return (Requisition 300 300)

  -- layout
  window `containerAdd` hBox
  boxPackStart hBox scaleCarAmount PackNatural 0
  boxPackStart hBox vBox PackGrow 0
  boxPackStart vBox desenho PackGrow 0
  boxPackStart vBox hSeparator PackNatural 0
  boxPackStart vBox hButtonBox PackNatural 0
  buttonBoxSetLayout hButtonBox ButtonboxSpread
  mapM (boxPackStartDefaults hButtonBox) [buttonReset,buttonAbout,buttonQuit]

  aboutDialog <- aboutDialogNew
  set aboutDialog [aboutDialogName := "S.A.R.A.H.", aboutDialogVersion := "0.95",
      aboutDialogLicense := Just "This small program is public domain. You can do \
      \whatever you want with it.", aboutDialogAuthors :=
      ["Maur"++[chr 237]++"cio C. Antunes (mauricio.antunes@gmail.com)"], aboutDialogComments :=
      "Software Automation of Road Automobile Headache"]

  -- all variables. 'last_time' is the last time 'cars' has
  -- been updated; used to calculate timestep
  cars <- newIORef (newCarList (fromIntegral halfMaxCars))
  last_time <- (newIORef =<< getClockTime)

  onClicked buttonReset $ do
    nCars <- adjustmentGetValue scaleAdjustment
    writeIORef cars (newCarList (round nCars))
  onClicked buttonAbout $ do
    dialogRun aboutDialog
    return ()
  onClicked buttonQuit $ do
    widgetDestroy window

  afterValueChanged scaleAdjustment $ do
    nCars <- adjustmentGetValue scaleAdjustment
    modifyIORef cars (changeCarListSize (round nCars))

  -- every 33 milliseconds...
  (flip timeoutAdd) 33 $ do
    (TOD s1 ps1) <- readIORef last_time
    (TOD s2 ps2) <- getClockTime
    writeIORef last_time (TOD s2 ps2)
    -- how much time since last update?
    timestep <- return $ 1e-12 * fromInteger(10^12*(s2-s1)+ps2-ps1)

    drawWindow <- widgetGetDrawWindow desenho
    coordinateTransformation <- drawingMatrix drawWindow

    -- do we have a congestion, i.e., is mouse
    -- close to the road?
    (mouseFromOrigin,congestionPosition) <- do
      (xI,yI) <- widgetGetPointer desenho
      (xD,yD) <- return (fromIntegral xI, fromIntegral yI)
      (x,y) <- return $ transformPoint (invert coordinateTransformation) (xD,yD)
      return (sqrt(x^2+y^2),(atan2 y x)/(2*pi))
    congestion <- return $ if mouseFromOrigin<0.85 || mouseFromOrigin>1.15
      then Nothing
      else Just congestionPosition

    modifyIORef cars (updateCarsList congestion timestep)

    -- paint
    c <- readIORef cars
    (w,h) <- drawableGetSize drawWindow
    drawWindowBeginPaintRect drawWindow (Rectangle 0 0 w h)
    renderWithDrawable drawWindow $ do
      setMatrix coordinateTransformation
      road2render congestion c
    drawWindowEndPaint drawWindow
    return True

  widgetShowAll window
  mainGUI

road2render :: Maybe Double -> [Car] -> Render ()
road2render congestion cars = do
  newPath
  -- road
  setSourceRGB 0.0 0.0 0.0
  s <- return (2*pi/30.0)
  setDash [s,s] 0.0
  setLineWidth 0.01
  arc 0.0 0.0 1.0 0.0 (2*pi)
  stroke
  setDash [0.08,0.02] 0.0
  -- congestion
  case congestion of
    Nothing -> return ()
    Just c -> do
      moveTo 0 0
      lineTo (1.2*(cos(c*2*pi))) (1.2*(sin(c*2*pi)))
      stroke
  -- cars
  setSourceRGBA 0.0 0.0 0.0 0.55
  (flip mapM_) cars $ \(Car p _) -> do
    (x,y) <- return (cos(2*pi*p),sin(2*pi*p))
    arc x y (0.5*carSize*2*pi) 0.0 (2*pi)
    Graphics.Rendering.Cairo.fill

