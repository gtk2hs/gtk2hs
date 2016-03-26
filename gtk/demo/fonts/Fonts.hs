{-# LANGUAGE OverloadedStrings #-}
-- Example of an drawing graphics onto a canvas.
import Data.List ( intersperse )
import qualified GI.Gtk as Gtk (init)
import GI.Pango
       (fontFaceGetFaceName, fontFamilyGetName, fontFamilyListFaces,
        fontFaceListSizes, fontMapListFamilies)
import GI.PangoCairo (fontMapGetDefault)

main = do
  Gtk.init Nothing
  fm <- fontMapGetDefault
  ffs <- fontMapListFamilies fm
  mapM_ (\ff -> do
    ffname <- fontFamilyGetName ff
    putStrLn (show ffname++": ")
    fcs <- fontFamilyListFaces ff
    mapM_ (\fc -> do
      sizes <- fontFaceListSizes fc
      let showSize Nothing = "all sizes"
          showSize (Just sz) = concat (intersperse ", " (map show sz))++
                               " points"
      fcname <- fontFaceGetFaceName fc
      putStrLn ("  "++show fcname++" in "++showSize sizes)
      ) fcs
    ) ffs
