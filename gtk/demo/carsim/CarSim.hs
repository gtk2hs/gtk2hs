-- program: S.A.R.A.H. jam simulator
-- author: Maurício C. Antunes
-- e-mail: mauricio.antunes@gmail.com
-- license: public domain

module Main where
import Data.Maybe
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.Rendering.Cairo
import Control.Monad
import Data.IORef
import Data.List
import Data.Time
import Data.Complex

-- Constants

accelerator = 0.7*carSize :: Double
brake = 10*accelerator:: Double
carSize = 2*pi/59 :: Double
responseTime = 0.24 :: Double
drawSide = 5/2 :: Double

-- A few conveniences

eventWindowSize = do
    dr <- eventWindow
    (w,h) <- liftIO $ drawableGetSize dr
    return $ if w*h > 1
        then (fromIntegral w, fromIntegral h)
        else (1,1)

eventPolarCoordinates = do
    (w,h) <- eventWindowSize
    (x,y) <- eventCoordinates
    let (origX, origY) = (w/2, h/2)
    let (scaleX, scaleY) = (drawSide/w, drawSide/h)
    let (x',y') = (scaleX*(x-origX), scaleY*(y-origY))
    let (radius,theta) = polar $ x' :+ y'
    return $ (radius,theta)

getAndSet :: a -> IO (IO a, a -> IO ())
getAndSet a = do
    ior <- newIORef a
    let get = readIORef ior
    let set = writeIORef ior
    return (get,set)

diffTime :: UTCTime -> UTCTime -> Double
diffTime = (realToFrac .) . diffUTCTime

moveToLineTo :: Double -> Double
 -> Double -> Double -> Render ()
moveToLineTo a b c d = moveTo a b >> lineTo c d

-- Car list handling

-- Each car is represented by a pair of Doubles. The first
-- Double is its position in a circular road, represented by
-- an angle. The second is its angular velocity. The general
-- idea behind the simulation is that in a list of cars each
-- one will try to keep a safe speed to avoid a crash in the
-- event of a sudden brake of the next car.

newCarList nCars = take nCars $ zip [0,2*pi/nCars'..] (repeat 0)
    where nCars' = fromIntegral nCars

-- This resizes car lists by copying or keeping those
-- at lower speeds.

newCarListFromList nCars [] = newCarListFromList nCars [(0,0)]
newCarListFromList nCars list = sortBy ((. fst).(compare . fst)) $
    take nCars $ cycle $ sortBy ((. snd).(compare . snd)) list

-- Safe speed for car, given data from itself and the next
-- and, possibly, a forced (by the user) jam. Speed changes
-- are limited by accelerator and brake maxima.

newSpeed dt jam (p1,s1) (p2,s2) = min cv $ max bv $ ds - br
    where
        pd = (p2-p1-carSize) - responseTime*(s2-s1)
        pj = maybe pd ((subtract $ carSize/2)
         . (until (>0) (+2*pi)) . (subtract p1)) jam
        dd = brake*(max 0 $ min pd pj)
        br = brake*responseTime
        ds = sqrt $ br^2 + 2*dd
        cv = s1 + accelerator*dt
        bv = s1 - brake*dt

-- Update positions and speeds based on a timestep and maybe
-- taking a forced congestion into account

updateCarList _ _ [] = []
updateCarList timestep jam list = zip newPositions' newSpeeds
    where
        fakeCar = (p+2*pi,s) where (p,s) = head list
        newSpeeds = zipWith ns list (tail list ++ [fakeCar])
            where ns = newSpeed timestep jam
        newPositions = zipWith3 mean fsts snds newSpeeds
            where
                mean a b c = a + timestep*(b+c)/2
                fsts = map fst list
                snds = map snd list
        newPositions' = map (subtract base) newPositions
        base = (*(2*pi)) $ fromIntegral $ floor $ (/ (2*pi)) $
            head newPositions

about = do
    ad <- aboutDialogNew
    aboutDialogSetName ad "S.A.R.A.H."
    aboutDialogSetVersion ad "1.0"
    aboutDialogSetAuthors ad $ ["Maurício C. Antunes "
        ++ "<mauricio.antunes@gmail.com>"]
    aboutDialogSetComments ad $ "Software Automation of "
        ++ "Road Automobile Headache"
    dialogRun ad
    widgetDestroy ad

main :: IO ()
main = do

    initGUI

    mainWindow <- windowNew
    drawingArea <- drawingAreaNew

    (getTimeStamp,setTimeStamp) <- getCurrentTime >>= getAndSet
    (getCars,setCars) <- getAndSet $ newCarList 20
    (getJam,setJam) <- getAndSet Nothing
    (getTimeoutId,setTimeoutId) <- getAndSet Nothing

    -- If 'resume' is called, 'step' will be called at small
    -- timesteps to update car data. If 'pause' is called, 'step'
    -- calls are stoped.  'resume' is called at program startup,
    -- and then the pause button alternates 'resume' and 'pause'.

    let step = do
         time <- getCurrentTime
         dt <- getTimeStamp >>= return . (diffTime time)
         setTimeStamp time
         liftM2 (updateCarList dt) getJam getCars >>= setCars
    let pause = do
         maybe (return ()) timeoutRemove =<< getTimeoutId
         setTimeoutId Nothing
    let resume = do
         setTimeoutId . Just =<< flip timeoutAdd 33
          (step >> widgetQueueDraw drawingArea >> return True)
         getCurrentTime >>= setTimeStamp

    -- The elements of the graphic interface are the set of
    -- buttons, the scale to set the number of cars and the
    -- car track. They are named as 'buttons', 'howMany' and
    -- 'track'. Each of them contains other widgets inside, but
    -- there's no reason to expose their names to the main IO.

    buttons <- do

        qr <- buttonNewFromStock stockClear
        onClicked qr $ do
            (liftM length) getCars >>= setCars . newCarList
            getCurrentTime >>= setTimeStamp
            widgetQueueDraw drawingArea

        qp <- toggleButtonNewWithLabel stockMediaPause
        buttonSetUseStock qp True
        onToggled qp $ do
            p <- toggleButtonGetActive qp
            case p of
                True -> pause
                False -> resume

        qa <- buttonNewFromStock stockAbout
        onClicked qa $ about

        qq <- buttonNewFromStock stockQuit
        onClicked qq (do
                       widgetDestroy mainWindow
                       mainQuit)

        bb <- hButtonBoxNew
        containerAdd bb qr
        containerAdd bb qp
        containerAdd bb qa
        containerAdd bb qq
        return bb
  
    howMany <- do

        sc <- vScaleNewWithRange 1 40 1
        afterRangeValueChanged sc $ do
            v <- liftM floor $ rangeGetValue sc
            c <- getCars
            setCars $ newCarListFromList v c
            widgetQueueDraw drawingArea

        scaleSetValuePos sc PosTop
        scaleSetDigits sc 0
        rangeSetUpdatePolicy sc UpdateDiscontinuous
        rangeSetValue sc =<< liftM (fromIntegral . length) getCars

        al <- alignmentNew 0.5 0.5 0 1
        alignmentSetPadding al 15 15 15 15
        containerAdd al sc
        return al
  
    track <- do

        let dr = drawingArea
        widgetAddEvents dr [PointerMotionMask]

        on dr motionNotifyEvent $ do
            (r,t) <- eventPolarCoordinates
            liftIO $ if (0.8<r && r<1.2)
                then setJam (Just t)
                else setJam Nothing
            liftIO $ widgetQueueDraw dr
            return True

        on dr leaveNotifyEvent $ liftIO $
            setJam Nothing >> return True

        on dr exposeEvent $ do
            (w,h) <- eventWindowSize
            dw <- eventWindow
            liftIO $ do
                jam <- getJam
                cars <- getCars
                renderWithDrawable dw $ do
                    translate (w/2) (h/2)
                    scale (w/drawSide) (h/drawSide)
                    road2render jam cars
            return True

        af <- aspectFrameNew 0.5 0.5 (Just 1)
        frameSetShadowType af ShadowNone
        containerAdd af dr
        return af
  
    -- 'layout' is a widget that contains all interface elements
    -- properly arranged.

    layout <- do
        vb <- vBoxNew False 0
        hb <- hBoxNew False 0
        boxPackStart vb track PackGrow 0
        boxPackStart vb buttons PackNatural 0
        boxPackStart hb howMany PackNatural 0
        boxPackStart hb vb PackGrow 0
        return hb

    windowSetTitle mainWindow "S.A.R.A.H."
    windowSetDefaultSize mainWindow 400 400
    on mainWindow objectDestroy mainQuit
    containerAdd mainWindow layout
    widgetShowAll mainWindow

    resume

    mainGUI

-- As the name says, this takes road info, in the form of a
-- possible jam and a list of cars, and make it into a Cairo
-- render.  Road will have radius 1.

road2render :: Maybe Double -> [(Double,Double)] -> Render ()
road2render jam cars = do
    newPath
    setSourceRGB 0 0 0
    drawRoad
    when (isJust jam) drawJam
    setSourceRGBA 0 0 0 0.55
    let cars' = map fst cars
    let rotations = zipWith subtract (0:cars') cars'
    sequence_ $ map ((>> drawCar) . rotate) rotations
 where
    drawRoad = setLineWidth 0.01 >> setDash [2*pi/34,2*pi/34]
     (pi/34) >> arc 0.0 0.0 1.0 0.0 (2*pi) >> stroke
    drawJam = setLineWidth 0.005 >> setDash [0.03,0.02] 0.04 >>
     save >> rotate (fromJust jam) >> moveToLineTo 0.8 0 1.2
     0 >> stroke >> setDash [] 0 >> moveToLineTo 0.8 (-0.015)
     0.8 0.015 >> moveToLineTo 1.2 (-0.015) 1.2 0.015 >> stroke
     >> restore
    drawCar = arc 1 0 (carSize/2) 0 (2*pi) >> fill
