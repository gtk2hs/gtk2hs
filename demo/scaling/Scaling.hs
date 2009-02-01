{-# OPTIONS -O #-}
 ---  {-# OPTIONS_GHC -XFlexibleContexts #-} see Makefile
-- Author: Pawel Bulkowski (pawelb16@gmail.com)
-- Thanks to Michal Palka for teaching me Haskell
-- Photos by: Magdalena Niedziela
-- based on other gtk2hs example applications
-- the code is public domain
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.EventM

import Data.Array.MArray
import Data.Array.IO
--import Data.Array.IO.Internals
import Data.Array.Storable
import Data.Bits
import Data.Word
import Data.Maybe
import Data.IORef
import Data.Ord
import Control.Monad ( when, unless, liftM )
import Control.Monad.Trans ( liftIO )
import Control.Monad.ST
import Data.Array.Base ( unsafeWrite, unsafeRead ) 
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.ModelView as New 
import CPUTime
import System.Environment ( getArgs )
import System.Directory ( doesFileExist )
type ArrayType = IOUArray
--type ArrayType = StorableArray

-- The state and GUI

data ImageState = Empty|NonEmpty
data State = State {
  pb :: Pixbuf,
  is :: ImageState
}

      
main = do
  args <- getArgs
  case args of
    [fName] -> do
      exists <- doesFileExist fName
      if exists then runGUI fName else
        putStrLn ("File "++fName++" not found.")
    _ -> putStrLn "Usage: scaling <image.jpg>"
    
runGUI fName = do 
  initGUI

  window <- windowNew
  window `onDestroy` mainQuit
  set window [ windowTitle := "Scaling"
             , windowResizable := True ]
  label <- labelNew (Just "Content Aware Image Scaling")
  vboxOuter <- vBoxNew False 0
  vboxInner <- vBoxNew False 5
  
  (mb,miOpen,miSave,miScale, miGradient, miSeamCarve, miQuit) <- makeMenuBar
  canvas <- drawingAreaNew
  containerAdd vboxInner canvas
  
  
  -- Assemble the bits
  set vboxOuter [ containerChild := mb
                , containerChild := vboxInner ]
  set vboxInner [ containerChild := label
                , containerBorderWidth := 10 ]
  set window [ containerChild := vboxOuter ]
 
  -- create the Pixbuf
  pb <- pixbufNew ColorspaceRgb False 8 256 256
  -- Initialize the state
  state <- newIORef State { pb = pb, is = Empty }
  let modifyState f = readIORef state >>= f >>= writeIORef state

  canvas `onSizeRequest` return (Requisition 256 256)
  

  -- Add action handlers
  onActivateLeaf miQuit mainQuit
--  onActivateLeaf miOpen $ modifyState $ reset gui
  onActivateLeaf miOpen $ modifyState $ loadImageDlg canvas window
  onActivateLeaf miSave $ modifyState $ saveImageDlg canvas window
  onActivateLeaf miScale $ modifyState $ scaleImageDlg canvas window
  onActivateLeaf miGradient $ modifyState $ gradientImageDlg canvas window
  onActivateLeaf miSeamCarve $ modifyState $ seamCarveImageDlg canvas window

  modifyState (loadImage canvas window fName)
  
  canvas `on` exposeEvent $ updateCanvas state
  boxPackStartDefaults vboxInner canvas
  widgetShowAll window
  mainGUI

  return ()

 --uncomment for ghc < 6.8.3
--instance Show Rectangle where
--  show (Rectangle x y w h) = "x="++show x++", y="++show y++
--			     ", w="++show w++", h="++show h++";"

updateCanvas :: IORef State -> EventM EExpose Bool
updateCanvas rstate = do
  region <- eventRegion
  win <- eventWindow
  liftIO $ do
  state <- readIORef rstate
  let (State pb is) = state
  gc <- gcNew win
  width  <- pixbufGetWidth pb
  height <- pixbufGetHeight pb
  pbregion <- regionRectangle (Rectangle 0 0 width height)
  regionIntersect region pbregion
  rects <- regionGetRectangles region
  putStrLn ("redrawing: "++show rects)
  (flip mapM_) rects $ \(Rectangle x y w h) -> do
    drawPixbuf win gc pb x y x y w h RgbDitherNone 0 0
  return True

{-# INLINE doFromTo #-}
-- do the action for [from..to], ie it's inclusive.
doFromTo :: Int -> Int -> (Int -> IO ()) -> IO ()
doFromTo from to action =
  let loop n | n > to   = return ()
             | otherwise = do action n
                              loop (n+1)
   in loop from

-- do the action for [to..from], ie it's inclusive.
{-# INLINE doFromToDown #-}
doFromToDown :: Int -> Int -> (Int -> IO ()) -> IO ()
doFromToDown from to action =
  let loop n | n < to   = return ()
             | otherwise = do action n
                              loop (n-1)
   in loop from

-- do the action for [from..to] with step, ie it's inclusive.
{-# INLINE doFromToStep #-}
doFromToStep :: Int -> Int -> Int -> (Int -> IO ()) -> IO ()
doFromToStep from to step action =
  let loop n | n > to   = return ()
             | otherwise = do action n
                              loop (n+step)
   in loop from
   
--forM = flip mapM
   
makeMenuBar = do
  mb <- menuBarNew
  fileMenu <- menuNew
  open <- menuItemNewWithMnemonic "_Open"
  save <- menuItemNewWithMnemonic "_Save"
  scale <- menuItemNewWithMnemonic "_Scale"
  gradient <- menuItemNewWithMnemonic "_Gradient"
  seamCarve <- menuItemNewWithMnemonic "Seam _Carve"
  quit <- menuItemNewWithMnemonic "_Quit"
  file <- menuItemNewWithMnemonic "_File"
  menuShellAppend fileMenu open
  menuShellAppend fileMenu save
  menuShellAppend fileMenu scale
  menuShellAppend fileMenu gradient
  menuShellAppend fileMenu seamCarve
  menuShellAppend fileMenu quit
  menuItemSetSubmenu file fileMenu
  containerAdd mb file
  return (mb,open,save,scale,gradient,seamCarve,quit)

loadImageDlg canvas window (State pb is) = do
  putStrLn ("loadImage")
  ret <- openFileDialog window
  case ret of
    Just (filename) -> (loadImage canvas window filename (State pb is))
    Nothing -> return (State pb is)


loadImage canvas window filename (State pb is) = do
  putStrLn ("loadImage")
  pxb <- pixbufNewFromFile filename
  width  <- pixbufGetWidth pxb
  height <- pixbufGetHeight pxb
  widgetSetSizeRequest canvas width height
  widgetQueueDraw canvas
--	updateCanvas canvas pxb
  return (State pxb NonEmpty)

  
saveImageDlg canvas window (State pb is) = do
  putStrLn ("saveImage")
  ret <- openFileDialog window
  case ret of
    Just (filename) -> do
      pixbufSave pb filename "png" []
      return (State pb is)
    Nothing -> return (State pb is)

scaleImageDlg canvas window (State pb is) = do
  putStrLn ("scaleImage")
  
  origWidth  <- pixbufGetWidth pb
  origHeight <- pixbufGetHeight pb
  ret <- scaleDialog window origWidth origHeight

  let update w h = do
      putStrLn ("seamCarveImage::update w: "++show w++" h: "++show h)
      --scalePixbuf :: Pixbuf -> Int -> Int -> IO Pixbuf
      pxb <- scalePixbuf pb w h
      width  <- pixbufGetWidth pxb
      height <- pixbufGetHeight pxb
      widgetSetSizeRequest canvas width height
      widgetQueueDraw canvas
      --updateCanvas canvas pxb
      return (State pxb NonEmpty)

  case ret of
    Nothing -> return (State pb NonEmpty)
    Just (w,h) -> (update w h)
	
gradientImageDlg canvas window (State pb is) = do
  putStrLn ("gradientImageDlg")
  --scalePixbuf :: Pixbuf -> Int -> Int -> IO Pixbuf
  pxb <- gradientPixbuf pb
  width  <- pixbufGetWidth pxb
  height <- pixbufGetHeight pxb
  widgetSetSizeRequest canvas width height
  widgetQueueDraw canvas
--	updateCanvas canvas pxb
  return (State pxb NonEmpty)
	
seamCarveImageDlg canvas window (State pb is) = do
  origWidth  <- pixbufGetWidth pb
  origHeight <- pixbufGetHeight pb
  ret <- seamCarveDialog window origWidth origHeight 2

  let update w h grdCnt = do
      putStrLn ("seamCarveImageDlg::update w: "++show w++" h: "++show h)
      --scalePixbuf :: Pixbuf -> Int -> Int -> IO Pixbuf
      --pxb <- scalePixbuf pb w h
      cpuStart <- getCPUTime
      pxb <- seamCarvePixbuf pb w h grdCnt
      cpuEnd <- getCPUTime
      putStrLn ("seamCarveImageDlg::cpu time: "++show ((fromIntegral (cpuEnd-cpuStart) :: Double) /1e12))
      width  <- pixbufGetWidth pxb
      height <- pixbufGetHeight pxb
      widgetSetSizeRequest canvas width height
      widgetQueueDraw canvas
      --updateCanvas canvas pxb
      return (State pxb NonEmpty)

  case ret of
    Nothing -> return (State pb NonEmpty)
    Just (w,h,grdCnt) -> (update w h grdCnt)

	
scaleDialog :: Window -> Int -> Int-> IO (Maybe (Int, Int))
scaleDialog parent width height = do

  Just xml <- xmlNew "scaling.glade" 

  dia <- xmlGetWidget xml castToDialog "dialogScale"
  dialogAddButton dia stockCancel  ResponseCancel
  dialogAddButton dia stockOk ResponseOk
  entryWidth <- xmlGetWidget xml castToEntry "entryScalingWidth" 
  entryHeight <- xmlGetWidget xml castToEntry "entryScalingHeight" 
  entrySetText entryWidth (show width)
  entrySetText entryHeight (show height)
  res <- dialogRun dia
  widthStr <- entryGetText entryWidth
  heightStr <- entryGetText entryHeight
  widgetDestroy dia
  putStrLn ("scaleDialog width: "++show width++" height: "++show height)
  case res of
    ResponseOk -> return (Just (read widthStr,read heightStr))
    _ -> return Nothing

seamCarveDialog :: Window -> Int -> Int -> Int -> IO (Maybe (Int, Int, Int))
seamCarveDialog parent width height grdCnt= do

  Just xml <- xmlNew "scaling.glade" 

  dia <- xmlGetWidget xml castToDialog "dialogSeamCarve"
  dialogAddButton dia stockCancel  ResponseCancel
  dialogAddButton dia stockOk ResponseOk
  entryWidth <- xmlGetWidget xml castToEntry "entryWidth" 
  entryHeight <- xmlGetWidget xml castToEntry "entryHeight" 
  entryGrdCnt <- xmlGetWidget xml castToEntry "entryGrdCnt" 
  entrySetText entryWidth (show width)
  entrySetText entryHeight (show height)
  entrySetText entryGrdCnt (show grdCnt)
  res <- dialogRun dia
  widthStr <- entryGetText entryWidth
  heightStr <- entryGetText entryHeight
  grdCntStr <- entryGetText entryGrdCnt
  widgetDestroy dia
  putStrLn ("scaleDialog width: "++show width++" height: "++show height++" grdCnt: "++show grdCnt)
  case res of
    ResponseOk -> return (Just (read widthStr,read heightStr, read grdCntStr))
    _ -> return Nothing

      
openFileDialog :: Window -> IO (Maybe String)
openFileDialog parentWindow = do
  dialog <- fileChooserDialogNew
              (Just "Open Profile... ")
              (Just parentWindow)
	      FileChooserActionOpen
	      [("gtk-cancel", ResponseCancel)
	      ,("gtk-open", ResponseAccept)]
  widgetShow dialog
  response <- dialogRun dialog
  widgetHide dialog
  case response of
      ResponseAccept -> fileChooserGetFilename dialog
      _ -> return Nothing

--simple pixbuf scaling
scalePixbuf :: Pixbuf -> Int -> Int -> IO Pixbuf
scalePixbuf pb newWidth newHeight = do
  width  <- pixbufGetWidth pb
  height <- pixbufGetHeight pb
  row <- pixbufGetRowstride pb
  chan <- pixbufGetNChannels pb
  bits <- pixbufGetBitsPerSample pb
  pbData <- (pixbufGetPixels pb :: IO (PixbufData Int Word8))
  pbn <- pixbufNew ColorspaceRgb False 8 newWidth newHeight
  pbnData <- (pixbufGetPixels pbn :: IO (PixbufData Int Word8))
  newRow <- pixbufGetRowstride pbn
  putStrLn ("bytes per row: "++show row++", channels per pixel: "++show chan++
	    ", bits per sample: "++show bits)
  putStrLn ("width: "++show width++", height: "++show height++", newWidth: "++show newWidth++", newHeight: "++show newHeight++" bytes per row new: "++show newRow)
  
	    
  let stepX = (fromIntegral width) / (fromIntegral newWidth) :: Double
  let stepY = (fromIntegral height) / (fromIntegral newHeight) :: Double

  doFromTo 0 (newHeight-1) $ \y -> do
    let y1 = truncate ((fromIntegral y) * stepY)
    doFromTo 0 (newWidth-1) $ \x -> do
      let x1 = truncate ((fromIntegral x) * stepX)
      let off = (x1*chan+y1*row)
      let offNew = (x*chan+y*newRow)
      --putStrLn ("x: "++show x++", y: "++show y++" x1: "++show x1++", y1: "++show y1++" off:"++show off++" offNew:"++show offNew)
      r <- unsafeRead pbData (off)
      g <- unsafeRead pbData (1+off)
      b <- unsafeRead pbData (2+off)
      unsafeWrite pbnData (offNew) r
      unsafeWrite pbnData (1+offNew) g
      unsafeWrite pbnData (2+offNew) b
  return pbn


{-# INLINE arrmove #-}
arrmove :: (Ix i, MArray a e IO) => a i e -> Int -> Int -> Int -> IO ()
arrmove arr src dst size = do

  --putStrLn("arrmove "++show src++" "++show dst++" "++show size)
  doFromTo 0 (size-1) $ \x -> do
  --forM [0..(size-1)] $ \x -> do
    v <- unsafeRead arr (src+x)
    unsafeWrite arr (dst+x) v
  --putStrLn("arrmove2 "++show src++" "++show dst++" "++show size)
  return ()

 
{-# INLINE arrmovesd #-}
arrmovesd :: (Ix b, MArray a c IO) => a b c -> a b c -> Int -> Int -> Int -> IO ()
arrmovesd arrsrc arrdst src dst size = do
  doFromTo 0 (size-1) $ \x -> do
  --forM [0..(size-1)] $ \x -> do
    v <- unsafeRead arrsrc (src+x)
    unsafeWrite arrdst (dst+x) v
  return ()

{-# INLINE arrmoven #-}
arrmoven :: (Ix i, MArray a e IO) => a i e -> Int -> Int -> Int -> Int -> Int -> IO ()
arrmoven arr src dst size w n = do
  --putStrLn("arrmoven "++show src++" "++show dst++" "++show size++" "++show w++" "++show n)
  doFromToStep 0 ((n-1)*w) w $ \yoff -> do
    arrmove arr (src+yoff) (dst+yoff) size
  return ()

-- content Aware scaling
--TODO!
seamCarvePixbuf :: Pixbuf -> Int -> Int -> Int -> IO Pixbuf
seamCarvePixbuf pb newWidth newHeight grdCnt = do
  width  <- pixbufGetWidth pb
  height <- pixbufGetHeight pb
  row <- pixbufGetRowstride pb
  chan <- pixbufGetNChannels pb
  bits <- pixbufGetBitsPerSample pb
  pbData <- (pixbufGetPixels pb :: IO (PixbufData Int Word8))
  --pbn <- pixbufNew ColorspaceRgb False 8 newWidth newHeight
  pbn <- pixbufNew ColorspaceRgb False 8 newWidth newHeight
  pbnData <- (pixbufGetPixels pbn :: IO (PixbufData Int Word8))
  newRow <- pixbufGetRowstride pbn
  putStrLn ("bytes per row: "++show row++", channels per pixel: "++show chan++
	    ", bits per sample: "++show bits)
  putStrLn ("width: "++show width++", height: "++show height++", newWidth: "++show newWidth++", newHeight: "++show newHeight++" bytes per row new: "++show newRow)

  tmpPB <- pixbufCopy pb
  tmpData <- (pixbufGetPixels tmpPB)  :: IO (PixbufData Int Word8)
  ----double gradient
  
  let computeSrcPic pb cnt | cnt <= 0 = do pixbufCopy pb
                           | cnt >  0 = do
                                          pb <- computeSrcPic pb (cnt-1)
                                          gradientPixbuf pb

  --computing gradient but one more gradient
  --will be compute later by gradientArray function                                               
  tmpPB2 <- computeSrcPic tmpPB (grdCnt-1)
  tmpData2 <- (pixbufGetPixels tmpPB2)  :: IO (PixbufData Int Word8)

  -- array to store x coord of removed pixels
  coordArr <- newArray (0, (max width height)) 0 :: IO (ArrayType Int Int) 
    
  let removeVPixel pixData x y w = do
      --unsafeWrite pixData (0+x*chan+y*row) 255
      --unsafeWrite pixData (1+x*chan+y*row) 255
      --unsafeWrite pixData (2+x*chan+y*row) 255
      --store x-coord of removed pixel
      unsafeWrite coordArr y x
      arrmove pixData ((x+1)*chan+y*row) (x*chan+y*row) ((w-x-1)*chan)
      return ()  
  
  let removeHPixel pixData x y h = do
      --putStrLn("removeHPixel "++show x++" "++show y++" "++show h)
      --store y-coord of removed pixel
      unsafeWrite coordArr y x
      --putStrLn("removeHPixel1.5 "++show x++" "++show y++" "++show h)
      arrmoven pixData (y*chan+(x+1)*row) (y*chan+x*row) chan row (h-x-1)
      --putStrLn("removeHPixel2 "++show x++" "++show y++" "++show h)
      return ()  
      
  let removeVGrdPixel grdData x y w = do
      arrmove grdData (x+1+y*width) (x+y*width) (w-x-1)
      return ()
  
  let removeHGrdPixel grdData x y h = do
      --putStrLn("removeHGrdPixel "++show x++" "++show y++" "++show h)
      arrmoven grdData (y+(x+1)*width) (y+x*width) 1 width (h-x-1)
      --putStrLn("removeHGrdPixel2 "++show x++" "++show y++" "++show h)
      return ()
  
  let vPixIndex x y chan row = (x*chan)+(y*row)
  let hPixIndex x y chan row = (y*chan)+(x*row)

  -- possibly it can be made shorted
  let removeSeam pixIndex rmPixel rmGrdPixel seamArr grdArr x y w = do
        rmPixel tmpData x y w
        rmPixel tmpData2 x y w
        rmGrdPixel grdArr x y w
        unless (y == 0) $ do
            v0 <- if x==0 then return 0x7fffffff else unsafeRead seamArr (pixIndex (x-1) y 1 width)
            v1 <- unsafeRead seamArr (pixIndex x y 1 width)
            v2 <- if x==(w-1) then return 0x7fffffff else unsafeRead seamArr (pixIndex (x+1) y 1 width)
            let nextX | v0 < v1 && v0 < v2 = (x-1)
                      | v2 < v1            = (x+1) 
                      | True               = x
            removeSeam pixIndex rmPixel rmGrdPixel seamArr grdArr nextX (y-1) w

  -- possibly it can be update to be more general
  let updateGradientArray pixIndex grdArr y w h = unless (y == -1) $ do
      x <- unsafeRead coordArr y
      unless (x == 0) $ do
          g <- pixelGradient pixIndex tmpData2 row chan w h (x-1) y
          unsafeWrite grdArr (pixIndex (x-1) y 1 width) g
          unless (y == 0) $ do
              g <- pixelGradient pixIndex tmpData2 row 1 w h (x-1) (y-1)
              unsafeWrite grdArr (pixIndex (x-1) (y-1) 1 width) g
          unless (y == (h-1)) $ do
              g <- pixelGradient pixIndex tmpData2 row 1 w h (x-1) (y+1)
              unsafeWrite grdArr (pixIndex (x-1) (y+1) 1 width) g
      g <- pixelGradient pixIndex tmpData2 row 1 w h x y
      unsafeWrite grdArr (pixIndex x y 1 width) g
      unless (y == 0) $ do
          g <- pixelGradient pixIndex tmpData2 row 1 w h x (y-1)
          unsafeWrite grdArr (pixIndex x (y-1) 1 width) g
      g <- pixelGradient pixIndex tmpData2 row 1 w h x (y+1)
      unless (y == (h-1)) $ do
          g <- pixelGradient pixIndex tmpData2 row 1 w h x (y+1)
          unsafeWrite grdArr (pixIndex x (y+1) 1 width) g
      updateGradientArray pixIndex grdArr (y-1) w h
      return ()
      
  let findMinVal pixIndex seamArr w h = do
      v <- unsafeRead seamArr (pixIndex 0 (h-1) 1 width)
      xRef <- newIORef (v :: Int, 0 :: Int)
      --let modifyState f = readIORef state >>= f >>= writeIORef state
      doFromTo 1 (w-1) $ \x -> do
        --putStrLn("findMinVal loop x: "++show x++" (h-1): "++show (h-1))
        v <- unsafeRead seamArr (pixIndex x (h-1) 1 width)
        (mval, m) <- readIORef xRef
        writeIORef xRef (if v < mval then (v, x) else (mval, m))
      (mval, m) <- readIORef xRef
        
      putStrLn("w: " ++show w++ " minSeam: " ++ show mval ++ " at: "++show m)      
      return m

  grdArr <- gradientArray tmpPB2 width height
  
  let removeVSeam w = do
      seamArr <- (computeVSeamArray grdArr width height w)
      m <- findMinVal vPixIndex seamArr w (height-1)
      removeSeam vPixIndex removeVPixel removeVGrdPixel seamArr grdArr m (height-1) w
      updateGradientArray vPixIndex grdArr (height-1) w height
      return ()

  let removeHSeam h = do
      seamArr <- (computeHSeamArray grdArr width height h)
      m <- findMinVal hPixIndex seamArr h (width-1)
      removeSeam hPixIndex removeHPixel removeHGrdPixel seamArr grdArr m (width-1) h
      updateGradientArray hPixIndex grdArr (width-1) h width
      return ()
                 
  --let nextX | v0 < v1 && v0 < v2 = (x-1)
  --          | v2 < v1            = (x+1) 
  --          | True               = x
      
  let grdSeam w h | w > newWidth && h > newHeight = do
                      --putStrLn("grdSeam: "++show w++" "++show h)
                      vSeamArr <- (computeVSeamArray grdArr width height w)
                      mv <- findMinVal vPixIndex vSeamArr w (height-1)
                      hSeamArr <- (computeHSeamArray grdArr width height h)
                      mh <- findMinVal hPixIndex hSeamArr h (width-1)
                      if mv < mh
                        then do
                          removeSeam vPixIndex removeVPixel removeVGrdPixel vSeamArr grdArr mv (height-1) w
                          updateGradientArray vPixIndex grdArr (height-1) w height
                          grdSeam (w-1) h
                        else do
                          removeSeam hPixIndex removeHPixel removeHGrdPixel hSeamArr grdArr mh (width-1) h
                          updateGradientArray hPixIndex grdArr (width-1) h width
                          grdSeam w (h-1)
                  | w > newWidth = do
                      --putStrLn("grdSeam2: "++show w++" "++show h)
                      removeVSeam w
                      grdSeam (w-1) h
                      
                  | h > newHeight = do
                      --putStrLn("grdSeam3: "++show w++" "++show h)
                      removeHSeam h
                      grdSeam w (h-1)
                  | True = do
                      return ()
      
  -- remove/add seams
  --doFromToDown width (newWidth+1) $ \w -> do
  --  removeVSeam w
        
  --doFromToDown height (newHeight+1) $ \h -> do
  --  removeHSeam h
  
  grdSeam width height
        
      
  doFromTo 0 (newHeight-1) $ \y -> do
    arrmovesd tmpData pbnData (y*row) (y*newRow) newRow
      
  return pbn

-- compute the gradient map
gradientPixbuf :: Pixbuf -> IO Pixbuf
gradientPixbuf pb = do
  width  <- pixbufGetWidth pb
  height <- pixbufGetHeight pb
  row <- pixbufGetRowstride pb
  chan <- pixbufGetNChannels pb
  bits <- pixbufGetBitsPerSample pb
  pbData <- (pixbufGetPixels pb :: IO (PixbufData Int Word8))
  pbn <- pixbufNew ColorspaceRgb False 8 width height
  pbnData <- (pixbufGetPixels pbn :: IO (PixbufData Int Word8))
  putStrLn ("bytes per row: "++show row++", channels per pixel: "++show chan++", bits per sample: "++show bits)
  putStrLn ("width: "++show width++", height: "++show height)
	
  let getpix x y c = do
      case (x < 1 || x >= width || y < 1 || y >= height) of
        True -> return 0
        False -> (unsafeRead pbData (c+x*chan+y*row))
      
  let gradient x y c = do
      let convM = liftM fromIntegral
          blah a b = convM (getpix a b c)
      v00 <- blah (x-1) (y-1)
      v10 <- blah x (y-1)
      v20 <- blah (x+1) (y-1)
      v01 <- blah (x-1) y
      v21 <- blah (x+1) y
      v02 <- blah (x-1) (y+1)
      v12 <- blah x (y+1)
      v22 <- blah (x+1) (y+1)
      
      let gx = abs ((v20-v00)+2*(v21-v01)+(v22-v02))
      let gy = abs ((v02-v00)+2*(v12-v10)+(v22-v20))
      let g = (gx + gy)::Int
      --let g8 = (shiftR g 3)
      let g8 = if g > 255 then 255 else g
      return (fromIntegral(g8) :: Word8)

  let totalGradient x y = do
      rg <- gradient x y 0
      gg <- gradient x y 1
      bg <- gradient x y 2
      let g = rg + gg + bg
      return ((fromIntegral g)::Word8)
  

  doFromTo 0 (height-1) $ \y -> do
    let offY = y*row
    doFromTo 0 (width-1) $ \x -> do
      let offX = x*chan
      doFromTo 0 2 $ \c -> do
        let off = offY+offX + c
        --putStrLn ("x: "++show x++", y: "++show y++" off:"++show off)
        --v <- (totalGradient x y)
        v <- (gradient x y c)
        unsafeWrite pbnData (off) v
  return pbn

-- compute gradient fo single pixel
{-# INLINE pixelGradient #-}
pixelGradient :: (Int -> Int -> Int -> Int -> Int) -> (PixbufData Int Word8) -> Int ->  Int -> Int ->  Int -> Int -> Int -> (IO Word16)
pixelGradient pixIndex pbData row chan w h x y = do
	
  let getpix x y c = do
      case (x < 0 || x >= w || y < 0 || y >= h) of
        True -> return 0
        False -> (unsafeRead pbData (c+(pixIndex x y chan row)))
        --False -> (unsafeRead pbData (c+x*chan+y*row))

  let gradient x y c = do
      let convM = liftM fromIntegral
          blah a b = convM (getpix a b c)
      v00 <- blah (x-1) (y-1)
      v10 <- blah x (y-1)
      v20 <- blah (x+1) (y-1)
      v01 <- blah (x-1) y
      v21 <- blah (x+1) y
      v02 <- blah (x-1) (y+1)
      v12 <- blah x (y+1)
      v22 <- blah (x+1) (y+1)
      
      let gx = abs ((v20-v00)+2*(v21-v01)+(v22-v02))
      let gy = abs ((v02-v00)+2*(v12-v10)+(v22-v20))
      let g = (gx + gy)::Int
      --let g8 = (shiftR g 3)
      let g8 = if g > 255 then 255 else g
      return (fromIntegral(g8) :: Word8)
        
              
  let gradient x y c = do
      let convM = liftM fromIntegral
          blah a b = convM (getpix a b c)
      v00 <- blah (x-1) (y-1)
      v10 <- blah x (y-1)
      v20 <- blah (x+1) (y-1)
      v01 <- blah (x-1) y
      v21 <- blah (x+1) y
      v02 <- blah (x-1) (y+1)
      v12 <- blah x (y+1)
      v22 <- blah (x+1) (y+1)
      let gx = abs ((v20-v00)+2*(v21-v01)+(v22-v02))
      let gy = abs ((v02-v00)+2*(v12-v10)+(v22-v20))
      let g = gx + gy
      return (g :: Int)

  rg <- gradient x y 0
  gg <- gradient x y 1
  bg <- gradient x y 2
  let g = rg + gg + bg
  return ((fromIntegral g) :: Word16)

  
-- compute the gradient map
gradientArray :: Pixbuf -> Int ->  Int -> IO (ArrayType Int Word16)
gradientArray pb w h = do
  width  <- pixbufGetWidth pb
  height <- pixbufGetHeight pb
  row <- pixbufGetRowstride pb
  chan <- pixbufGetNChannels pb
  bits <- pixbufGetBitsPerSample pb
  pbData <- (pixbufGetPixels pb :: IO (PixbufData Int Word8))
  grdArr <- newArray (0, width * height) 0
  putStrLn ("bytes per row: "++show row++", channels per pixel: "++show chan++", bits per sample: "++show bits)
  putStrLn ("width: "++show width++", height: "++show height)

  let vPixIndex x y chan row = x*chan+y*row
  
  doFromTo 0 (h-1) $ \y -> do
    let offY = y*width
    doFromTo 0 (w-1) $ \x -> do
      let off = x + offY
      --v <- (totalGradient x y)
      v <- (pixelGradient vPixIndex pbData row chan w h x y)
      unsafeWrite grdArr (off) v
      --putStrLn ("x: "++show x++" y: "++show y++" v: "++show v)
  return grdArr

computeVSeamArray :: (ArrayType Int Word16) -> Int -> Int -> Int -> IO (ArrayType Int Int)
computeVSeamArray grdArr  width height currentWidth = do
  
  seamArr <- newArray (0, width * height) 0
  --grdArr <- gradientArr
  
  doFromTo 0 (currentWidth-1) $ \x -> do
    v <- unsafeRead grdArr x
    unsafeWrite seamArr x (fromIntegral v :: Int)
    
  doFromTo 1 (height-1) $ \y -> do
    let offY = y*width
    let prevOffY = offY-width
    doFromTo 1 (currentWidth-2) $ \x -> do
      p1 <- unsafeRead seamArr ((x-1)+prevOffY)
      p2 <- unsafeRead seamArr (x+prevOffY)
      p3 <- unsafeRead seamArr ((x+1)+prevOffY)
      v <- unsafeRead grdArr (x+offY)
      unsafeWrite seamArr (x+offY) ((fromIntegral v :: Int) +(min(min p1 p2) p3))
    p2l <- unsafeRead seamArr (0+prevOffY)
    p3l <- unsafeRead seamArr (1+prevOffY)
    vl <- unsafeRead grdArr (0+offY)
    unsafeWrite seamArr (0+offY) ((fromIntegral vl)+(min p2l p3l))
    p1r <- unsafeRead seamArr (currentWidth-2+prevOffY)
    p2r <- unsafeRead seamArr (currentWidth-1+prevOffY)
    vr <- unsafeRead grdArr (currentWidth-1+offY)
    unsafeWrite seamArr (currentWidth-1+offY) ((fromIntegral vr :: Int) +(min p1r p2r))
    
  return seamArr

computeHSeamArray :: (ArrayType Int Word16) -> Int -> Int -> Int -> IO (ArrayType Int Int)
computeHSeamArray grdArr  width height currentHeight = do
  
  seamArr <- newArray (0, width * height) 0
  --grdArr <- gradientArr
  
  doFromTo 0 (currentHeight-1) $ \y -> do
    v <- unsafeRead grdArr (y*width)
    unsafeWrite seamArr (y*width) (fromIntegral v :: Int)
    
  doFromTo 1 (width-1) $ \x -> do
    doFromTo 1 (currentHeight-2) $ \y -> do
      let offY = y*width
      let prevOffY = offY-width
      let nextOffY = offY+width
      p1 <- unsafeRead seamArr (x-1+prevOffY)
      p2 <- unsafeRead seamArr (x-1+offY)
      p3 <- unsafeRead seamArr (x-1+nextOffY)
      v <- unsafeRead grdArr (x+offY)
      unsafeWrite seamArr (x+offY) ((fromIntegral v :: Int) +(min(min p1 p2) p3))
    p2l <- unsafeRead seamArr (x-1+0)
    p3l <- unsafeRead seamArr (x-1+width)
    vl <- unsafeRead grdArr (x+0)
    unsafeWrite seamArr (x+0) ((fromIntegral vl)+(min p2l p3l))
    p1r <- unsafeRead seamArr (x-1+((currentHeight-2)*width))
    p2r <- unsafeRead seamArr (x-1+((currentHeight-1)*width))
    vr <- unsafeRead grdArr (x+((currentHeight-1)*width))
    unsafeWrite seamArr (x+((currentHeight-1)*width)) ((fromIntegral vr :: Int) +(min p1r p2r))
    
  return seamArr
