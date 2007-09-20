{- GTK 2.0 Tutorial: Chapter 2: Getting Started

   To run with GHCi: ghci GtkChap2a.hs
   To compile with GHC: ghc --make GtkChap2a.hs -o chap
-}
 
import Graphics.UI.Gtk

main :: IO ()
main = do 
  initGUI
  window <- windowNew
  windowSetTitle window "Step One"
  widgetShowAll window
  mainGUI

{- Notes:
   1) Watch the upper and lower case occurrences
   2) Ctl-z to abort GHCi, not Ctl-c 
-} 



  
