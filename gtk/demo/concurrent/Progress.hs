{-# LANGUAGE PatternSynonyms #-}
-- Example of concurrent Haskell and Gtk.
--
-- As long as GHC does not support OS level threads by default, a trick has
-- to be used to let Haskell programs continue while the GUI is running.
-- We attach a call to the Haskell function "yield" to the timeout handler of
-- Gtk's main loop. Thus GHC regularly gets a chance to execute Haskell
-- threads.

import Control.Applicative
import Prelude
import Control.Concurrent
import qualified GI.Gtk as GI (init)
import GI.Gtk
       (progressBarPulse, ProgressBar, dialogRun, widgetShowAll,
        boxPackStart, progressBarNew, dialogGetContentArea, pattern STOCK_CLOSE,
        dialogAddButton, dialogNew)
import GI.Gtk.Enums (ResponseType(..))
import GI.GLib (pattern PRIORITY_DEFAULT, timeoutAdd)

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

  -- 50ms timeout, so GHC will get a chance to scheule about 20 times a second
  -- which gives reasonable latency without the polling generating too much
  -- cpu load.
  timeoutAdd PRIORITY_DEFAULT 50 $ yield >> return True
  dialogRun dia
  return ()

doTask :: ProgressBar -> IO ()
doTask pb = do
  progressBarPulse pb
  threadDelay 100000
  doTask pb
