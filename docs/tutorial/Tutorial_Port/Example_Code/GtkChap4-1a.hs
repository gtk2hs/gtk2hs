import Graphics.UI.Gtk

main :: IO ()
main = do
  initGUI
  window <- windowNew
  set window [windowTitle := "Pix",
              containerBorderWidth := 10 ]
  button <- buttonNew
  onClicked button (putStrLn "button clicked")
  box <- labelBox "info.xpm" "cool button"
  containerAdd button box
  containerAdd window button
  widgetShowAll window
  onDestroy window mainQuit
  mainGUI

labelBox :: FilePath -> String -> IO HBox
labelBox fn txt = do
  box <- hBoxNew False 0
  set box [containerBorderWidth := 2 ]
  image <- imageNewFromFile fn
  label <- labelNew (Just txt)
  boxPackStart box image PackNatural 3 
  boxPackStart box label PackNatural 3
  return box 
