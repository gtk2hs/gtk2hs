-- This example uses SDL 2+ instead of SDL 1 in the gtk2 version, and requires
-- the haskell package sdl2.

import SDL
import Graphics.Rendering.Cairo
import Foreign.Ptr ( castPtr )

demo1 :: Render ()
demo1 = do
  setSourceRGB 0 0 0
  moveTo 100 100
  lineTo 500 500
  stroke

demo2 :: Render ()
demo2 = do
  setSourceRGB 0 0 0
  moveTo 500 100
  lineTo 100 500
  stroke

main = SDL.initialize [ SDL.InitVideo ] $ do
  screen <- SDL.setWindowMode 600 600 32 [ SDL.Surface ]

  --SDL.fillRect screen Nothing (SDL.Pixel maxBound)

  pixels <- fmap castPtr $ screen.surfacePixels

  canvas <- createImageSurfaceForData pixels FormatRGB24 600 600 (600 * 4)
  renderWith canvas demo1

  withImageSurfaceForData pixels FormatRGB24 600 600 (600 * 4) $ \canvas ->
    renderWith canvas demo2

  --SDL.flip screen

  idle
  where
  idle = do
    e <- SDL.waitEvent
    case e of
         SDL.QuitEvent  -> return ()
         otherwise -> idle
