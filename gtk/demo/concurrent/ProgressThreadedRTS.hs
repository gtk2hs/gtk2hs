-- Example of concurrent Haskell and Gtk.
--
-- This demo uses GHC's support for OS level threads. It has to be
-- linked using the ghc -threaded flag.
--
-- Because Gtk+ is single threaded we have to be very careful to call
-- Gtk+ only from the main GUI thread. So while it's ok to forkIO,
-- any GUI actions in that thread have to be 'posted' to the main GUI
-- thread using postGUI, or postGUIAsync as in the example here.

import Graphics.UI.Gtk

import Control.Concurrent
import Control.Applicative ((<$>))

main :: IO ()
main = do

  -- It is marked unsafe becuase it is your obligation to ensure you
  -- only call Gtk+ from one OS thread, or 'bad things' will happen.
  unsafeInitGUIForThreadedRTS

  dia <- dialogNew
  dialogAddButton dia stockClose ResponseClose
  contain <- castToBox <$> dialogGetContentArea dia
  pb <- progressBarNew
  boxPackStart contain pb PackNatural 0
  widgetShowAll dia
  forkIO (doTask pb)

  dialogRun dia
  return ()

doTask :: ProgressBar -> IO ()
doTask pb = do
  postGUIAsync $ progressBarPulse pb
  threadDelay 100000
  doTask pb
