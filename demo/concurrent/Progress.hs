-- Example of concurrent Haskell and Gtk.
--
-- As long as GHC does not support OS level threads by default, a trick has
-- to be used to let Haskell programs continue while the GUI is running.
-- We attach a call to the Haskell function "yield" to the idle handler of
-- Gtk's main loop. Thus, everytime Gtk is idleing, GHC gets a chance to
-- execute Haskell threads.

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
  idleAdd (yield >> return True) priorityDefault
  dialogRun dia
  return ()

doTask :: ProgressBar -> IO ()
doTask pb = do
  progressBarPulse pb
  threadDelay 1000000
  doTask pb
