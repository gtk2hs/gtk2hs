{- GTK 2.0 Tutorial: Chapter 3a, Getting Started

   To run with GHCi: ghci GtkChap3.hs
   To compile with GHC: ghc --make GtkChap3.hs -o chap
-}
 
import Graphics.UI.Gtk

main :: IO ()
main = do 
  initGUI
  window <- windowNew
  widgetShowAll window
  mainGUI

{- Notes:
   1) Watch the upper and lower case occurrences
   2) Ctl-z to abort GHCi, not Ctl-c 
-} 



  
