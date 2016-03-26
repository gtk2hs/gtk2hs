{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List (elemIndex)
import Data.GI.Base.ManagedPtr ( unsafeCastTo )
import qualified Data.Text as T
import qualified GI.Gtk as Gtk (main, init)
import GI.Gtk
       (widgetShowAll, containerAdd, binGetChild, comboBoxSetActive,
        comboBoxNewWithEntry, mainQuit, onWidgetDestroy, windowNew,
        Entry(..), onEntryActivate, entryGetText)
import GI.Gtk.Enums (WindowType(..))
import Graphics.UI.Gtk.MenuComboToolbar.ComboBox (comboBoxSetModelText, comboBoxAppendText, comboBoxGetModelText)
import Graphics.UI.Gtk.ModelView.ListStore (listStoreToList, listStoreAppend)

main = do
  Gtk.init Nothing

  win <- windowNew WindowTypeToplevel
  onWidgetDestroy win mainQuit

  combo <- comboBoxNewWithEntry
  comboBoxSetModelText combo

  mapM_ (comboBoxAppendText combo)
    (T.words "ice-cream turkey pasta sandwich steak")

  -- select the first item
  comboBoxSetActive combo 0

  -- Get the entry widget that the ComboBoxEntry uses.
  w <- binGetChild combo
  entry <- unsafeCastTo Entry w

  -- Whenever the user has completed editing the text, append the new
  -- text to the store unless it's already in there.
  onEntryActivate entry $ do
    str <- entryGetText entry
    store <- comboBoxGetModelText combo
    elems <- listStoreToList store
    comboBoxSetActive combo (-1)
    idx <- case elemIndex str elems of
      Just idx -> return $ fromIntegral idx
      Nothing -> listStoreAppend store str
    comboBoxSetActive combo idx
    return ()

  containerAdd win combo

  widgetShowAll win
  Gtk.main
