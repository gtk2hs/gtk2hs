{-# LANGUAGE PatternSynonyms #-}
-- Example of concurrent Haskell and Gtk.
--
-- This demo uses GHC's support for OS level threads. It has to be
-- linked using the ghc -threaded flag.
--
-- Because Gtk+ is single threaded we have to be very careful to call
-- Gtk+ only from the main GUI thread. So while it's ok to forkIO,
-- any GUI actions in that thread have to be 'posted' to the main GUI
-- thread using postGUI, or postGUIAsync as in the example here.

import Control.Applicative
import Prelude
import Control.Concurrent
import qualified GI.Gtk as GI (init)
import GI.Gtk
       (progressBarPulse, ProgressBar, dialogRun, widgetShowAll,
        boxPackStart, progressBarNew, dialogGetContentArea, pattern STOCK_CLOSE,
        dialogAddButton, dialogNew)
import GI.Gtk.Enums (ResponseType(..))
import GI.GLib (pattern PRIORITY_DEFAULT, idleAdd)

main :: IO ()
main = do

  GI.init Nothing

  dia <- dialogNew
  dialogAddButton dia STOCK_CLOSE (fromIntegral $ fromEnum ResponseTypeClose)
  contain <- dialogGetContentArea dia
  pb <- progressBarNew
  boxPackStart contain pb False False 0
  widgetShowAll dia
  forkIO (doTask pb)

  dialogRun dia
  return ()

doTask :: ProgressBar -> IO ()
doTask pb = do
  idleAdd PRIORITY_DEFAULT $ progressBarPulse pb >> return False
  threadDelay 100000
  doTask pb
