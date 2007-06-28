{- Written by Antti-Juhani Kaijanaho.
   You may treat this file as if it were in the public domain.
-}
module Main where

import Graphics.SOE.Gtk

main = runGraphics $ do w <- openWindow "Testing" (200, 200)
                        drawInWindow w $ text (10,180) "Hello"
			drawInWindow w $ withColor Blue $
			  polyline [(20,20), (20,150), (150,150), (150,20), (20,20)]
			let region =      createRectangle (20,20) (100,100)
                              `orRegion` createRectangle (50,50) (150,150)
			      `diffRegion` createRectangle (100,100) (150,150)
			drawInWindow w $ withColor Blue $
			  polyline [(20,20), (20,100), (100,100), (100,20), (20,20)]
			drawInWindow w $ withColor Blue $
			  polyline [(50,50), (50,150), (150,150), (150,50), (50,50)]
			drawInWindow w $ withColor Green $ drawRegion region
                        loopEvents w
			closeWindow w

loopEvents w = loop
    where loop = do e <- getWindowEvent w
                    case e of Closed -> return ()
                              _     -> loop
