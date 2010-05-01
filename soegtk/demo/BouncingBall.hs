{- Written by Antti-Juhani Kaijanaho.
   You may treat this file as if it were in the public domain.
-}
module Main where

import Graphics.SOE.Gtk

main = runGraphics $
       do w <- openWindowEx "Bouncing Ball" Nothing (Just (300, 300)) drawBufferedGraphic
          let loop x y xd yd
                  = do setGraphic w $ withColor Yellow $
                                    ellipse (x-5,y-5) (x+5,y+5)
                       (xmax, ymax) <- getWindowSize w
                       let x' = x + xd + 5
                           y' = y + yd + 5
                           xd' | x' >= xmax || x' < 0 = -xd
                               | otherwise = xd
                           yd' | y' >= ymax || y' < 0 = -yd
                               | otherwise = yd
                           x'' = x + xd'
                           y'' = y + yd'
                           x''' | x'' + 5 > xmax = xmax `div` 2
                                | otherwise = x''
                           y''' | y'' + 5 > ymax = ymax `div` 2
                                | otherwise = y''
                       e <- getWindowEvent w
		       case e of Closed -> return ()
                                 _      -> loop x''' y''' xd' yd'
          loop 300 100 5 5

