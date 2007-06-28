module Main where

import Graphics.SOE.Gtk

type Vector = (Double, Double)

(<+>) :: Vector -> Vector -> Vector
(a, b) <+> (c, d) = (a+c, b+d)

(<->) :: Vector -> Vector -> Vector
(a, b) <-> (c, d) = (a-c, b-d)

(.*) :: Double -> Vector -> Vector
k .* (a, b) = (k * a, k * b)

(*.) :: Vector -> Double -> Vector
(*.) = flip (.*)

(<*>) :: Vector -> Vector -> Double
(a, b) <*> (c, d) = a * c + b * d

norm :: Vector -> Double
norm v = sqrt (v <*> v)

dist :: Vector -> Vector -> Double
dist v1 v2 = norm (v1 <-> v2)

xunit :: Vector
xunit = (1, 0)

yunit :: Vector
yunit = (0,1)

ortho :: Vector -> Vector
ortho v@(a, b) = (m11*a + m12*b, m21*a+m22*b)
    where m11 = cos ang
          m12 = sin ang
          m21 = -sin ang
          m22 = cos ang
          ang = pi/2


type Line = (Vector, Vector)

gen :: [Line] -> [Line]
gen [] = []
gen (l@(v1,v2):ls)
    | dist v1 v2 < 3 = l : gen ls
    | otherwise = let dir = v1 <-> v2
                      ort = ortho dir
                      p = v2 <+> ((1/3) .* dir)
                      q = v2 <+> ((2/3) .* dir)
                      r = v2 <+> (0.5 .* dir) <+> ((1/3) .* ort)
                      s = v2 <+> (0.5 .* dir) <+> ((-1/3) .* ort)
                  in --(v2,p) : (q,v1) : gen ((p,r) : (q,r) : (p,s) : (q,s) : ls)
		     gen ((v2,p) : (q,v1) : (p,r) : (q,r) : (p,s) : (q,s) : ls)
                -- kauniimpi kuva (mutta tehtävänannon vastainen) tulee
                -- korvaamalla edellinen lauseke seuraavalla:
                -- gen ((v2,p) : (q,v1) : (p,r) : (q,r) : (p,s) : (q,s) : ls)

draw :: [Line] -> Graphic
draw [] = emptyGraphic
draw ((p1,p2):ls) = overGraphic (line (f p1) (f p2)) (draw ls)
    where f (x,y) = (round (x), round (y))

test = runGraphics $ do w <- openWindow "T 3.7-8" (200, 200)
                        loop w
                        closeWindow w
    where loop w = do (xmax', ymax') <- getWindowSize w
                      let xmax = fromIntegral xmax'
                          ymax = fromIntegral ymax'
                          ls = gen [((1/8 * xmax, 1/2 * ymax),
                                     (7/8 * xmax, 1/2 * ymax))]
                      setGraphic w (draw ls)
                      e <- getWindowEvent w
                      case e of Resize -> loop w
                                _      -> return ()

main = test
