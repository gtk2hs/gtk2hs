{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- Simple StatusIcon example
import GI.Gtk
       (noMenuPositionFunc, noWidget, onMenuItemActivate,
        menuShellAppend, menuItemNewWithLabel, mainQuit, menuNew,
        onStatusIconActivate, menuPopup, widgetShowAll,
        onStatusIconPopupMenu, statusIconSetTooltipText,
        statusIconSetVisible, pattern STOCK_QUIT, statusIconNewFromStock)
import qualified GI.Gtk as Gtk (main, init)

main = do
  Gtk.init Nothing
  icon <- statusIconNewFromStock STOCK_QUIT
  statusIconSetVisible icon True
  statusIconSetTooltipText icon "This is a test"
  menu <- mkmenu icon
  onStatusIconPopupMenu icon $ \b a -> do
         widgetShowAll menu
         print (b,a)
         menuPopup menu noWidget noWidget noMenuPositionFunc b a
  onStatusIconActivate icon $
         putStrLn "'activate' signal triggered"
  Gtk.main

mkmenu s = do
  m <- menuNew
  mapM_ (mkitem m) [("Quit",mainQuit)]
  return m
    where
        mkitem menu (label,act) =
            do i <- menuItemNewWithLabel label
               menuShellAppend menu i
               onMenuItemActivate i act
