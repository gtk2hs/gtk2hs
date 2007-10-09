import Graphics.UI.Gtk

main :: IO ()
main = do
     initGUI
     window <- windowNew
     set window [windowTitle := "Font and Color Selection", containerBorderWidth := 10 ]
     vb <- vBoxNew False 0
     containerAdd window vb

     qtlab <- labelNew (Just "How poor are they that have not patience!\nWhat wound did ever heal but by degrees?\nThou know'st we work by wit, and not by witchcraft;\nAnd wit depends on dilatory time.")
     boxPackStart vb qtlab PackGrow 0

     srclab <- labelNew (Just "From Othello (II, iii, 376-379)")
     srcfont <- fontDescriptionFromString "Courier Italic 10"
     widgetModifyFont srclab (Just srcfont)
     miscSetAlignment srclab 1.0 0.5
     boxPackStart vb srclab PackNatural 10

     sep <- hSeparatorNew
     boxPackStart vb sep PackGrow 10
     
     fntb <- fontButtonNew
     boxPackStart vb fntb PackGrow 0

     colb <- colorButtonNew
     boxPackStart vb colb PackGrow 0

     onFontSet fntb $ do name <- fontButtonGetFontName fntb
                         fdesc <- fontDescriptionFromString name
                         widgetModifyFont qtlab (Just fdesc)
                         putStrLn name

     onColorSet colb $ do colour <- colorButtonGetColor colb
                          widgetModifyFg qtlab StateNormal colour
                          putStrLn (show  colour)

     widgetShowAll window
     onDestroy window mainQuit
     mainGUI

instance Show Color where
         show (Color r g b) = "Red: " ++ (show r) ++ 
                              " Green: " ++ (show g) ++ 
                              " Blue: " ++ (show b)



