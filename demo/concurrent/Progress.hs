-- Example of concurrent Haskell and Gtk.
--
-- As long as GHC does not support OS level threads by default, a trick has
-- to be used to let Haskell programs continue while the GUI is running.
-- We attach a call to the Haskell function "yield" to the timeout handler of
-- Gtk's main loop. Thus GHC regularly gets a chance to execute Haskell
-- threads.

import Graphics.UI.Gtk

import Control.Concurrent

main :: IO ()
main = do
  initGUI
  dia <- dialogNew
  dialogAddButton dia stockClose ResponseClose
  contain <- dialogGetUpper dia
  pb <- progressBarNew 
  boxPackStartDefaults contain pb
  widgetShowAll dia
  forkIO (doTask pb)

  -- 50ms timeout, so GHC will get a chance to scheule about 20 times a second
  -- which gives reasonable latency without the polling generating too much
  -- cpu load.
  timeoutAddFull (yield >> return True) priorityDefaultIdle 50
  dialogRun dia
  return ()

doTask :: ProgressBar -> IO ()
doTask pb = do
  progressBarPulse pb
  threadDelay 100000
  doTask pb
