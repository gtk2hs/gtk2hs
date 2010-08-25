-- Example of an drawing graphics onto a canvas.
import Graphics.UI.Gtk
import Data.List ( intersperse )

main = do
  initGUI
  fm <- cairoFontMapGetDefault
  ffs <- pangoFontMapListFamilies fm
  mapM_ (\ff -> do
    putStrLn (show ff++": ")
    fcs <- pangoFontFamilyListFaces ff
    mapM_ (\fc -> do
      sizes <- pangoFontFaceListSizes fc
      let showSize Nothing = "all sizes"
          showSize (Just sz) = concat (intersperse ", " (map show sz))++
                               " points"
      putStrLn ("  "++show fc++" in "++showSize sizes)
      ) fcs
    ) ffs
