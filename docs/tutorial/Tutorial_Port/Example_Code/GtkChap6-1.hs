import Graphics.UI.Gtk
import Data.IORef 
import System.Random (randomRIO)

main:: IO ()
main= do
     initGUI
     window <- windowNew
     set window [ windowTitle := "Guess a Number", 
                  windowDefaultWidth := 300, windowDefaultHeight := 250]
     mb <- vBoxNew False 0
     containerAdd window mb

     info <- labelNew (Just "Press \"New\" for a random number")
     boxPackStart mb info PackNatural 7
     sep1 <- hSeparatorNew
     boxPackStart mb sep1 PackNatural 7
     
     scrwin <- scrolledWindowNew Nothing Nothing
     boxPackStart mb scrwin PackGrow 0

     table <- tableNew 10 10 True
     scrolledWindowAddWithViewport scrwin table

     buttonlist <- sequence (map numButton [1..100])
     let places = cross [0..9] [0..9]
     sequence_ (zipWith (attachButton table) buttonlist places)

     sep2 <- hSeparatorNew
     boxPackStart mb sep2 PackNatural 7
     hb <- hBoxNew False 0
     boxPackStart mb hb PackNatural 0
     play <- buttonNewFromStock stockNew
     quit <- buttonNewFromStock stockQuit
     boxPackStart hb play PackNatural 0
     boxPackEnd hb quit PackNatural 0
     
     randstore <- newIORef 50
     randomButton info randstore play

     sequence_ (map (actionButton info randstore) buttonlist)  

     widgetShowAll window
     onClicked quit (widgetDestroy window)
     onDestroy window mainQuit
     mainGUI

numButton :: Int -> IO Button
numButton n = do
        button <- buttonNewWithLabel (show n)
        return button

cross :: [Int] -> [Int] -> [(Int,Int)]
cross row col = do 
        x <- row
        y <- col
        return (x,y)

attachButton :: Table -> Button -> (Int,Int) -> IO ()
attachButton ta bu (x,y) = 
              tableAttachDefaults ta bu y (y+1) x (x+1)

actionButton :: ButtonClass b => Label -> IORef Int -> b  -> IO (ConnectId b)
actionButton inf rst b = 
  onClicked b $ do label <- get b buttonLabel
                   let num = (read label):: Int
                   rand <- readIORef rst
                   case compare num rand of
                     GT -> do set inf [labelLabel :=  "Too High"]
                              widgetModifyFg inf StateNormal (Color 65535 0 0)
                     LT -> do set inf [labelLabel := "Too Low"]
                              widgetModifyFg inf StateNormal (Color 65535 0 0)
                     EQ -> do set inf [labelLabel := "Correct"]
                              widgetModifyFg inf StateNormal (Color 0 35000 0)

randomButton :: ButtonClass b => Label -> IORef Int -> b -> IO (ConnectId b)
randomButton inf rst b = 
    onClicked b $ do rand <- randomRIO (1::Int, 100)
                     writeIORef rst rand  
                     set inf [labelLabel := "Ready"]
                     widgetModifyFg inf StateNormal (Color 0 0 65535)
