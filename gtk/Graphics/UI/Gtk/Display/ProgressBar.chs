-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget ProgressBar
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.1 $ from $Date: 2005/01/08 15:14:30 $
--
--  Copyright (c) 1999..2002 Axel Simon
--
--  This file is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This file is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
-- |
--
-- The ProgressBar provides a means for an application to keep the user
-- patient while some time intensive task is going on.
--

module Graphics.UI.Gtk.Display.ProgressBar (
  ProgressBar,
  ProgressBarClass,
  castToProgressBar,
  progressBarNew,
  progressBarPulse,
  progressBarSetText,
  progressBarSetFraction,
  progressBarSetPulseStep,
  progressBarGetFraction,
  progressBarGetPulseStep,
  progressBarGetText,
  ProgressBarOrientation(..),
  progressBarSetOrientation,
  progressBarGetOrientation
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.General.Enums	(ProgressBarOrientation(..))

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Create a new ProgreeBar.
--
progressBarNew :: IO ProgressBar
progressBarNew  = makeNewObject mkProgressBar $ liftM castPtr $
  {#call unsafe progress_bar_new#}

-- | Indicates that some progress is made, but you
-- don't know how much. Causes the progress bar to enter \`activity mode',
-- where a block bounces back and forth. Each call to
-- 'progressBarPulse' causes the block to move on by a little bit
-- (the amount of movement per pulse is determined by
-- 'progressBarSetPulseStep').
--
progressBarPulse :: ProgressBarClass pb => pb -> IO ()
progressBarPulse pb = {#call unsafe progress_bar_pulse#} (toProgressBar pb)

-- | Causes the given @text@ to appear
-- superimposed on the progress bar.
--
progressBarSetText :: ProgressBarClass pb => pb -> String -> IO ()
progressBarSetText pb text = withUTFString text $
  {#call unsafe progress_bar_set_text#} (toProgressBar pb)

-- | Causes the progress bar to \`fill in' the
-- given fraction of the bar. The fraction should be between 0.0 and 1.0,
-- inclusive.
--
progressBarSetFraction :: ProgressBarClass pb => pb -> Double -> IO ()
progressBarSetFraction pb fraction = {#call unsafe progress_bar_set_fraction#}
  (toProgressBar pb) (realToFrac fraction)

-- | Sets the fraction of total progress bar
-- length to move the bouncing block for each call to progressBarPulse.
--
-- * The @fraction@ parameter must be between 0.0 and 1.0.
--
progressBarSetPulseStep :: ProgressBarClass pb => pb -> Double -> IO ()
progressBarSetPulseStep pb fraction = 
  {#call unsafe progress_bar_set_pulse_step#} (toProgressBar pb) 
  (realToFrac fraction)

-- | Returns the current fraction of the task
-- that has been completed.
--
progressBarGetFraction :: ProgressBarClass pb => pb -> IO Double
progressBarGetFraction pb = liftM realToFrac $ 
  {#call unsafe progress_bar_get_fraction#} (toProgressBar pb)

-- | Returns the current pulseStep of the task
-- that has been completed.
--
progressBarGetPulseStep :: ProgressBarClass pb => pb -> IO Double
progressBarGetPulseStep pb = liftM realToFrac $ 
  {#call unsafe progress_bar_get_pulse_step#} (toProgressBar pb)


-- | Retrieve the text displayed superimposed on the
-- ProgressBar.
--
-- * Returns Nothing if no text was set.
--
progressBarGetText :: ProgressBarClass pb => pb -> IO (Maybe String)
progressBarGetText pb = do
  strPtr <- {#call unsafe progress_bar_get_text#} (toProgressBar pb)
  if strPtr==nullPtr then return Nothing else liftM Just $ peekUTFString strPtr

-- | Causes the progress bar to switch to a
-- different orientation (left-to-right, right-to-left, top-to-bottom, or
-- bottom-to-top).
--
progressBarSetOrientation :: ProgressBarClass pb => pb ->
                             ProgressBarOrientation -> IO ()
progressBarSetOrientation pb orientation = 
  {#call progress_bar_set_orientation#} (toProgressBar pb)
  ((fromIntegral.fromEnum) orientation)

-- | Retrieve the current ProgressBar
-- orientation.
--
progressBarGetOrientation :: ProgressBarClass pb => pb ->
                             IO ProgressBarOrientation
progressBarGetOrientation pb = liftM (toEnum.fromIntegral) $
  {#call unsafe progress_bar_get_orientation#} (toProgressBar pb)


