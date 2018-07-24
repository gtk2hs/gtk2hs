{-# LANGUAGE UnicodeSyntax #-}


module Main where

import           Prelude.Unicode
import           Control.Monad.Unicode

import           Graphics.Rendering.Cairo
import           Graphics.Rendering.Cairo.Matrix      ( Matrix(..) )
import qualified Graphics.Rendering.Cairo.Matrix   as Matrix



main =  islamicWindows =<< makeLattice


makeLattice ∷ IO Surface
makeLattice = do
   let w    = 15
       h    = w
       a    = w/9.5
       w2   = (w/2 - a)
       h2   = (h/2 - a)
       mx   = Matrix   0   1 (-1)  0    w 0
       my   = Matrix   0 (-1)  1   0    0 h
       mxy  = Matrix (-1)  0   0 (-1)   w h
   surface ← createImageSurface FormatARGB32 (truncate w) (truncate h)
   renderWith surface $ do
      moveTo 0 h2
      lineTo a h2
      curveTo a (h2-a) (w2-a) a w2 a
      lineTo w2 0
      p ← copyPath
      let xPath  = transformPath mx p
          yPath  = transformPath my p
          xyPath = transformPath mxy p
      appendPath (move2line xPath)
      appendPath (move2line xyPath)
      appendPath (move2line yPath)
      closePath
      p' ← copyPath
      liftIO ∘ putStrLn $ "Path:"
      liftIO ∘ putStrLn $ unlines (map show p')    -- notice how copyPath adds a MoveTo to the end of the path
      p'' ← copyPathFlat
      liftIO ∘ putStrLn $ "\nFlattened Path:"
      liftIO ∘ putStrLn $ unlines (map show p'')
      setSourceRGB 1 0.8 0
      setLineWidth 0.4
      stroke
   return surface


islamicWindows ∷ Surface → IO ()
islamicWindows bg = frameIt 640 640 "paths.png" $ do
   setSourceRGB 0.2 0.2 0.2
   paint

   setSourceRGB 0 0 1
   setLineWidth 5
   rectangle 10 10 620 620
   stroke

   let w  = 100
       h1 = 84
       h2 = 54
       x1 = w/6
       y1 = h1 + 2⋅h2/3
       x2 = w/4
       y2 = h1 + h2/4
   newPath
   moveTo 0 0
   lineTo 0 h1
   curveTo x1 y1 x2 y2 (w/2) (h1+h2)
   curveTo (w-x2) y2 (w-x1) y1 w h1
   lineTo w 0
   closePath
   p00 ← copyPath

   let w  = 100
       h1 = 108
       h2 = 54
       x1 = w/16
       y1 = h1 + h2/2 - 1/17⋅h2
       x2 = w/2 - w/11
       y2 = h1 + 4/5⋅h2
   newPath
   moveTo 0 0
   lineTo 0 h1
   curveTo x1 y1 x2 y2 (w/2) (h1+h2)
   curveTo (w-x2) y2 (w-x1) y1 w h1
   lineTo w 0
   closePath
   p10 ← copyPath

   let w  = 65
       h1 = 108
       h2 = 64
       x1 = -w/3
       y1 = h1 + 9/16⋅h2
       x2 = w/3
       y2 = h1 + 4/5⋅h2
   newPath
   moveTo 0 0
   lineTo 0 h1
   curveTo x1 y1 x2 y2 (w/2) (h1+h2)
   curveTo (w-x2) y2 (w-x1) y1 w h1
   lineTo w 0
   closePath
   p20 ← copyPath

   let w  = 95
       h1 = 108
   newPath
   moveTo 0 0
   lineTo 0 h1
   arcNegative (w/2) h1 (w/2) π (2⋅π)
   lineTo w 0
   closePath
   p01 ← copyPath

   let w1 = 110
       w2 = 15
       h1 = 108
   newPath
   moveTo 0 0
   lineTo 0 h1
   arcNegative (w1/2) h1 ((w1 - 2⋅w2)/2) π (2⋅π)
   lineTo w1 h1
   lineTo w1 0
   closePath
   p11 ← copyPath

   let w1 = 90
       w2 = 14
       h1 = 95
       r = w1/2
       α  = acos $ (w1/2 - w2) / r
       h2 = r ⋅ sin α
   newPath
   moveTo 0 0
   lineTo 0 h1
   arcNegative (w1/2) (h1+h2) r (π+α) (-α)
   lineTo w1 h1
   lineTo w1 0
   closePath
   p21 ← copyPath

   let w1 = 90
       w2 = 19
       h1 = 95
       h2 = 75
       x1 = -w/3
       y1 = h1 + 9/16⋅h2
       x2 = w/3
       y2 = h1 + 4/5⋅h2
   newPath
   moveTo 0 0
   lineTo 0 h1
   lineTo w2 h1
   curveTo x1 y1 x2 y2 (w1/2) (h1+h2)
   curveTo (w1-x2) y2 (w1-x1) y1 (w1-w2) h1
   lineTo w1 h1
   lineTo w1 0
   closePath
   p02 ← copyPath

   let w  = 95
       h = 70
   newPath
   moveTo 0 0
   lineTo 0 h
   arcNegative (w/2) h (w/2) π (2⋅π)
   lineTo w 0
   arcNegative (w/2) 0 (w/2) 0 π
   p12 ← copyPath

   let w  = 100
       h  = 70
   newPath
   moveTo 0 0
   arc 0 (h/2) (h/2) (3/2⋅π) (π/2)
   lineTo w h
   arc w (h/2) (h/2) (π/2) (3/2⋅π)
   closePath
   p22 ← copyPath


   identityMatrix

   let mats =  [ Matrix 1 0 0 (-1) 60 210
               , Matrix 1 0 0 (-1) 270 210
               , Matrix 1 0 0 (-1) 480 210
               , Matrix 1 0 0 (-1) 60 420
               , Matrix 1 0 0 (-1) 270 420
               , Matrix 1 0 0 (-1) 480 420
               , Matrix 1 0 0 (-1) 60 620
               , Matrix 1 0 0 (-1) 270 570
               , Matrix 1 0 0 (-1) 480 580
               ]

   let paths = zipWith transformPath mats [p00,p10,p20,p01,p11,p21,p02,p12,p22]

   newPath
   mapM_ appendPath paths
   clip
   withPatternForSurface bg $ \bgPat → do
      patternSetExtend bgPat ExtendRepeat
      setSource bgPat
   paint

   setSourceRGB 0.8 0.8 0.8
   setLineWidth 7
   mapM_ (\p → newPath ≫ appendPath p ≫ stroke) paths
--




frameIt ∷ Double → Double → String → Render () → IO ()
frameIt w h filename r =
   renderToPNG filename (truncate w) (truncate h) $ do
      rectangle 9.2 9.2 (w-18.4) (h-18.4)
      p ← copyPath
      clip
      save
      r
      restore
      appendPath p
      setSourceRGBA 1 1 0 0.7
      setLineWidth 5.0
      stroke
--


renderToPNG filename w h renderer = do
   surface ← createImageSurface FormatARGB32 w h
   renderWith surface renderer
   surfaceWriteToPNG surface filename
--



transformPath ∷ Matrix → Path → Path
transformPath mat = map mapElem
   where tm u v = Matrix.transformPoint mat (u,v)
         mapElem ∷ PathElement → PathElement
         mapElem (MoveTo x y)                  = uncurry MoveTo (tm x y)
         mapElem (LineTo x y)                  = uncurry LineTo (tm x y)
         mapElem (CurveTo x₁ y₁ x₂ y₂ x₃ y₃)   =
            uncurry (uncurry (uncurry CurveTo (tm x₁ y₁)) (tm x₂ y₂)) (tm x₃ y₃)
         mapElem ClosePath                     = ClosePath
--

move2line ∷ Path → Path
move2line = map m2l
   where m2l (MoveTo x y)  = uncurry LineTo (x,y)
         m2l x             = x
