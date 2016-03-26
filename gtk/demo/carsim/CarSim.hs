{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- program: S.A.R.A.H. jam simulator
-- author: Maurício C. Antunes
-- e-mail: mauricio.antunes@gmail.com
-- license: public domain

module Main where

import Control.Applicative
import Prelude

import Data.Maybe
import GI.Gtk
       (widgetShowAll, onWidgetDestroy, setWindowDefaultHeight,
        setWindowDefaultWidth, setWindowTitle, boxPackStart, hBoxNew,
        vBoxNew, frameSetShadowType, aspectFrameNew,
        widgetGetAllocatedHeight, widgetGetAllocatedWidth, onWidgetDraw,
        onWidgetLeaveNotifyEvent, onWidgetMotionNotifyEvent,
        widgetAddEvents, alignmentSetPadding, alignmentNew, rangeSetValue,
        scaleSetDigits, scaleSetValuePos, rangeGetValue,
        afterScaleButtonValueChanged, vScaleNewWithRange, containerAdd,
        hButtonBoxNew, mainQuit, onButtonActivate, pattern STOCK_QUIT, pattern STOCK_ABOUT,
        toggleButtonGetActive, onToggleButtonToggled, buttonSetUseStock,
        pattern STOCK_MEDIA_PAUSE, toggleButtonNewWithLabel, onButtonClicked,
        pattern STOCK_CLEAR, buttonNewFromStock, widgetQueueDraw, drawingAreaNew,
        windowNew, widgetDestroy, dialogRun, setAboutDialogComments,
        setAboutDialogAuthors, setAboutDialogVersion,
        setAboutDialogProgramName, aboutDialogNew)
import GI.Cairo
import Control.Monad
import Data.IORef
import Data.List
import Data.Time
import Data.Complex
import Control.Monad.IO.Class (MonadIO(..))
import Graphics.Rendering.Cairo
       (fill, restore, save, stroke, arc, setDash, setLineWidth, rotate,
        setSourceRGBA, setSourceRGB, newPath, scale, translate, lineTo,
        moveTo, Render)
import qualified GI.Gtk as GI (init, main)
import GI.GLib (pattern PRIORITY_DEFAULT, sourceRemove, timeoutAdd)
import GI.Gdk
       (eventMotionReadY, eventMotionReadX, windowGetHeight,
        windowGetWidth, eventMotionReadWindow)
import GI.Gdk.Flags (EventMask(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import GI.Gtk.Enums
       (WindowType(..), ShadowType(..), PositionType(..))
import Data.Monoid ((<>))
import Data.GI.Base.BasicConversions (gflagsToWord)
import Graphics.Rendering.Cairo.Types (Cairo(..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (castPtr)
import Graphics.Rendering.Cairo.Internal (Render(..))

-- Constants

accelerator = 0.7*carSize :: Double
brake = 10*accelerator:: Double
carSize = 2*pi/59 :: Double
responseTime = 0.24 :: Double
drawSide = 5/2 :: Double

-- A few conveniences
eventWindowSize e = do
    dr <- eventMotionReadWindow e
    w <- windowGetWidth dr
    h <- windowGetHeight dr
    return $ if w*h > 1
        then (fromIntegral w, fromIntegral h)
        else (1,1)

eventPolarCoordinates e = do
    (w,h) <- eventWindowSize e
    x <- eventMotionReadX e
    y <- eventMotionReadY e
    let (origX, origY) = (w/2, h/2)
    let (scaleX, scaleY) = (drawSide/w, drawSide/h)
    let (x',y') = (scaleX*(x-origX), scaleY*(y-origY))
    let (radius,theta) = polar $ x' :+ y'
    return (radius,theta)

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
    setAboutDialogProgramName ad "S.A.R.A.H."
    setAboutDialogVersion ad "1.0"
    setAboutDialogAuthors ad ["Maurício C. Antunes "
                           <> "<mauricio.antunes@gmail.com>"]
    setAboutDialogComments ad ("Software Automation of "
                            <> "Road Automobile Headache")
    dialogRun ad
    widgetDestroy ad

main :: IO ()
main = do

    GI.init Nothing

    mainWindow <- windowNew WindowTypeToplevel
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
         maybe (return ()) (void . sourceRemove) =<< getTimeoutId
         setTimeoutId Nothing
    let resume = do
         setTimeoutId . Just =<< timeoutAdd PRIORITY_DEFAULT 33
          (step >> widgetQueueDraw drawingArea >> return True)
         getCurrentTime >>= setTimeStamp

    -- The elements of the graphic interface are the set of
    -- buttons, the scale to set the number of cars and the
    -- car track. They are named as 'buttons', 'howMany' and
    -- 'track'. Each of them contains other widgets inside, but
    -- there's no reason to expose their names to the main IO.

    buttons <- do

        qr <- buttonNewFromStock STOCK_CLEAR
        onButtonClicked qr $ do
            (liftM length) getCars >>= setCars . newCarList
            getCurrentTime >>= setTimeStamp
            widgetQueueDraw drawingArea

        qp <- toggleButtonNewWithLabel STOCK_MEDIA_PAUSE
        buttonSetUseStock qp True
        onToggleButtonToggled qp $ do
            p <- toggleButtonGetActive qp
            if p
                then pause
                else resume

        qa <- buttonNewFromStock STOCK_ABOUT
        onButtonClicked qa about

        qq <- buttonNewFromStock STOCK_QUIT
        onButtonActivate qq (do
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
        afterScaleButtonValueChanged sc $ \_ -> do
            v <- floor <$> rangeGetValue sc
            c <- getCars
            setCars $ newCarListFromList v c
            widgetQueueDraw drawingArea

        scaleSetValuePos sc PositionTypeTop
        scaleSetDigits sc 0
--        rangeSetUpdatePolicy sc UpdateDiscontinuous
        rangeSetValue sc =<< liftM (fromIntegral . length) getCars

        al <- alignmentNew 0.5 0.5 0 1
        alignmentSetPadding al 15 15 15 15
        containerAdd al sc
        return al

    track <- do

        let dr = drawingArea
        widgetAddEvents dr (gflagsToWord [EventMaskPointerMotionMask])

        onWidgetMotionNotifyEvent dr $ \e -> do
            (r,t) <- eventPolarCoordinates e
            if 0.8<r && r<1.2
                then setJam (Just t)
                else setJam Nothing
            widgetQueueDraw dr
            return True

        onWidgetLeaveNotifyEvent dr $ \e ->
            setJam Nothing >> return True

        onWidgetDraw dr $ \(Context fp) -> withForeignPtr fp $ \p -> (`runReaderT` Cairo (castPtr p)) $ runRender $ do
            w <- liftIO $ fromIntegral <$> widgetGetAllocatedWidth dr
            h <- liftIO $ fromIntegral <$> widgetGetAllocatedHeight dr
            jam <- liftIO getJam
            cars <- liftIO getCars
            translate (w/2) (h/2)
            scale (w/drawSide) (h/drawSide)
            road2render jam cars
            return True

        af <- aspectFrameNew Nothing 0.5 0.5 1 False
        frameSetShadowType af ShadowTypeNone
        containerAdd af dr
        return af

    -- 'layout' is a widget that contains all interface elements
    -- properly arranged.

    layout <- do
        vb <- vBoxNew False 0
        hb <- hBoxNew False 0
        boxPackStart vb track True True 0
        boxPackStart vb buttons False False 0
        boxPackStart hb howMany False False 0
        boxPackStart hb vb True True 0
        return hb

    setWindowTitle mainWindow "S.A.R.A.H."
    setWindowDefaultWidth mainWindow 400
    setWindowDefaultHeight mainWindow 400
    onWidgetDestroy mainWindow mainQuit
    containerAdd mainWindow layout
    widgetShowAll mainWindow

    resume

    GI.main

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
