import Graphics.UI.Gtk

hello :: (ButtonClass o) => o -> IO ()
hello b = set b [buttonLabel := "Hello World"]

main :: IO ()
main = do 
  initGUI
  window <- windowNew
  button <- buttonNew
  set window [windowDefaultWidth := 200, windowDefaultHeight := 200,
              containerChild := button , containerBorderWidth := 10 ]
  onClicked button (hello button)
  onDestroy window mainQuit
  widgetShowAll window
  mainGUI   

{- Notes:
   1) the default size of the window is set to 200 x 200 pixels
   2) when the button is clicked the hello function is 'executed'
-}
