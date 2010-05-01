
import System.Environment (getArgs)

import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.SVG

main :: IO ()
main = do
  [inFile, outFile] <- getArgs
  svg <- svgNewFromFile inFile
  let (width, height) = svgGetSize svg
  withImageSurface FormatARGB32 width height $ \result -> do
    renderWith result $ do
      clear
      svgRender svg
    surfaceWriteToPNG result outFile

clear :: Render ()
clear = do
  save
  setOperator OperatorClear
  paint
  restore
