
import System.Environment (getArgs)

import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.SVG

main :: IO ()
main = do
  (file:_) <- getArgs
  svg <- newSVGFromFile file
  let (width, height) = sizeSVG svg
  withImageSurface FormatARGB32 width height $ \result -> do
    renderWith result $ do
      clear
      renderSVG svg
    surfaceWriteToPNG result ("output.png")

clear :: Render ()
clear = do
  save
  setOperator OperatorClear
  paint
  restore
