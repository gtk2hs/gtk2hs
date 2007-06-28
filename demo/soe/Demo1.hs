{- Written by Antti-Juhani Kaijanaho.
   You may treat this file as if it were in the public domain.
-}
module Main where

import Graphics.SOE.Gtk

main = runGraphics $ do w <- openWindow "Testing" (200, 200)
                        drawInWindow w $ text (100,100) "Hello"
                        drawInWindow w $ line (50, 50) (75, 75)
                        drawInWindow w $ withColor White $ polyline [(10,10), (10,40), (20,20)]
                        drawInWindow w $ polygon [(60,60), (60,90), (80,90)]
                        drawInWindow w $ withColor Yellow $
                                     ellipse (290, 190) (150, 150)
			drawInWindow w $ arc (20,190) (90,130) (45) (390)
			drawInWindow w $ withColor Blue $ line (20,190) (90,130)
			drawInWindow w $ withColor Yellow $
                                     shearEllipse (140, 10) (160, 90) (190, 50)
                        loopEvents w
			closeWindow w

loopEvents w = loop
    where loop = do e <- getWindowEvent w
                    case e of Closed -> return ()
                              _     -> loop
