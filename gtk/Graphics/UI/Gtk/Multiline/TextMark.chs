-- -*-haskell-*-
--  GIMP Toolkit (GTK) TextMark TextBuffer
--
--  Author : Axel Simon
--          
--  Created: 23 February 2002
--
--  Version $Revision: 1.1 $ from $Date: 2005/01/08 15:29:36 $
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

module Graphics.UI.Gtk.Multiline.TextMark (
  TextMark,
  TextMarkClass,
  castToTextMark,
  MarkName,
  textMarkSetVisible,
  textMarkGetVisible,
  textMarkGetDeleted,
  textMarkGetName,
  textMarkGetBuffer,
  textMarkGetLeftGravity
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GObject	(makeNewGObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

type MarkName = String

-- methods

-- | Set the visibility of a 'TextMark'.
--
textMarkSetVisible :: TextMarkClass tm => tm -> Bool -> IO ()
textMarkSetVisible tm vis = 
  {#call unsafe text_mark_set_visible#} (toTextMark tm) (fromBool vis)


-- | Get the visibility of a 'TextMark'.
--
textMarkGetVisible :: TextMarkClass tm => tm -> IO Bool
textMarkGetVisible tm = liftM toBool $
  {#call unsafe text_mark_get_visible#} (toTextMark tm)

-- | Query if a 'TextMark' is still valid.
--
textMarkGetDeleted :: TextMarkClass tm => tm -> IO Bool
textMarkGetDeleted tm = liftM toBool $
  {#call unsafe text_mark_get_deleted#} (toTextMark tm)

-- | Get the name of a 'TextMark'.
--
-- * Returns Nothing, if the mark is anonymous.
--
textMarkGetName :: TextMarkClass tm => tm -> IO (Maybe String)
textMarkGetName tm = do
  strPtr <- {#call unsafe text_mark_get_name#} (toTextMark tm)
  if strPtr==nullPtr then return Nothing else liftM Just $ peekUTFString strPtr

-- | Extract the 'TextBuffer' of the mark.
--
-- * Returns Nothing if the mark was deleted.
--
textMarkGetBuffer :: TextMarkClass tm => tm -> IO (Maybe TextBuffer)
textMarkGetBuffer tm = do
  bufPtr <- {#call unsafe text_mark_get_buffer#} (toTextMark tm)
  if bufPtr==nullPtr then return Nothing else liftM Just $
    makeNewGObject mkTextBuffer (return $ castPtr bufPtr)

-- | Determine whether the mark has gravity
-- towards the beginning of a line.
--
-- * The name is misleading as Arabic, Hebrew and some other languages have
--   the beginning of a line towards the right.
--
textMarkGetLeftGravity :: TextMarkClass tm => tm -> IO Bool
textMarkGetLeftGravity tm = liftM toBool $
  {#call unsafe text_mark_get_left_gravity#} (toTextMark tm)


 
