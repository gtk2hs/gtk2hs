module Main where

import Graphics.UI.Gtk
import Control.Concurrent.MVar
import Control.Monad ( liftM )
import Data.Maybe ( fromMaybe )
import Data.List ( findIndex )
import Control.Monad.IO.Class (MonadIO(..))


main = do
  initGUI

  win <- windowNew
  on win deleteEvent $ liftIO mainQuit >> return False

  combo <- comboBoxNewWithEntry
  comboBoxSetModelText combo

  mapM_ (comboBoxAppendText combo)
    (words "ice-cream turkey pasta sandwich steak")

  -- select the first item
  comboBoxSetActive combo 0

  -- Get the entry widget that the ComboBoxEntry uses.
  (Just w) <- binGetChild combo
  let entry = castToEntry w

  -- Whenever the user has completed editing the text, append the new
  -- text to the store unless it's already in there.
  on entry entryActivated $ do
    str <- entryGetText entry
    store <- comboBoxGetModelText combo
    elems <- listStoreToList store
    comboBoxSetActive combo (-1)
    idx <- case (findIndex ((==) str) elems) of
      Just idx -> return idx
      Nothing -> listStoreAppend store str
    comboBoxSetActive combo idx
    return ()

  containerAdd win combo

  widgetShowAll win
  mainGUI
