import Graphics.UI.Gtk
import Data.Char (toUpper)

main :: IO ()
main= do
     initGUI
     window <- windowNew
     set window [windowTitle := "Notebook Example 2", windowDefaultWidth := 300,
                 windowDefaultHeight := 200 ]
     
     ntbk <- notebookNew
     containerAdd window ntbk
     set ntbk [notebookScrollable := True, notebookEnablePopup := True,
               notebookTabPos := PosRight ]

     stls <- stockListIds
     sequence_ (map (myNewPage ntbk) stls)

     onSwitchPage ntbk (putStrLn . ((++)"Page: ") . show)

     widgetShowAll window
     onDestroy window mainQuit
     mainGUI

tabName :: StockId -> String
tabName st = (drop 3) (conv st) where
                  conv (x:[]) = x:[]
                  conv (x:y:ys) | x == '-' = (toUpper y):(conv ys)
                                | otherwise = x: (conv (y:ys))

myNewPage :: Notebook -> StockId -> IO Int
myNewPage noteb stk = 
          do img <- imageNewFromStock stk 4
             let nmstr = tabName stk
             men <- labelNew (Just ((take 1) nmstr))
             cont <- labelNew (Just ("stock" ++ nmstr))
             pagenum <- notebookAppendPageMenu noteb cont img men
             return pagenum          
