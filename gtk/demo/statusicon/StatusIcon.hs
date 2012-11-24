-- Simple StatusIcon example
import Graphics.UI.Gtk

main = do
  initGUI
  icon <- statusIconNewFromStock stockQuit
  statusIconSetVisible icon True
  statusIconSetTooltip icon "This is a test"
  menu <- mkmenu icon
  on icon statusIconPopupMenu $ \b a -> do
         widgetShowAll menu
         print (b,a)
         menuPopup menu $ maybe Nothing (\b' -> Just (b',a)) b
  on icon statusIconActivate $ do
         putStrLn "'activate' signal triggered"
  mainGUI

mkmenu s = do
  m <- menuNew
  mapM_ (mkitem m) [("Let's blink!",statusIconSetBlinking s True)
                   ,("Let's stop blink!",statusIconSetBlinking s False)
                   ,("Quit",mainQuit)]
  return m
    where
        mkitem menu (label,act) =
            do i <- menuItemNewWithLabel label
               menuShellAppend menu i
               on i menuItemActivated act
