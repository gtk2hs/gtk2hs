-- -*-haskell-*-
--  GIMP Toolkit (GTK) TextMark TextBuffer
--
--  Author : Axel Simon
--
--  Created: 23 February 2002
--
--  Version $Revision: 1.4 $ from $Date: 2005/03/13 19:34:36 $
--
--  Copyright (C) 2002-2005 Axel Simon
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- A position in the buffer preserved across buffer modifications
--
module Graphics.UI.Gtk.Multiline.TextMark (
-- * Description
-- 
-- | You may wish to begin by reading the text widget conceptual overview
-- which gives an overview of all the objects and data types related to the
-- text widget and how they work together.
--
-- A 'TextMark' is like a bookmark in a text buffer; it preserves a position
-- in the text. You can convert the mark to an iterator using
-- 'textBufferGetIterAtMark'. Unlike iterators, marks remain valid across
-- buffer mutations, because their behavior is defined when text is inserted or
-- deleted. When text containing a mark is deleted, the mark remains in the
-- position originally occupied by the deleted text. When text is inserted at a
-- mark, a mark with left gravity will be moved to the beginning of the
-- newly-inserted text, and a mark with right gravity will be moved to the end.
--
-- Marks can be deleted from the buffer at any time
-- with 'textBufferDeleteMark'. Once deleted from the buffer, a mark is
-- essentially useless.
--
-- Marks optionally have names; these can be convenient to avoid passing the
-- 'TextMark' object around.
--
-- Marks are typically created using the 'textBufferCreateMark' function.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----TextMark
-- @

-- * Types
  TextMark,
  TextMarkClass,
  castToTextMark,
  MarkName,

-- * Methods
  textMarkSetVisible,
  textMarkGetVisible,
  textMarkGetDeleted,
  textMarkGetName,
  textMarkGetBuffer,
  textMarkGetLeftGravity,

-- * Properties
  textMarkVisible
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes		(Attr(..))
import System.Glib.GObject		(makeNewGObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

type MarkName = String

--------------------
-- Methods

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

--------------------
-- Properties

-- | \'visible\' property. See 'textMarkGetVisible' and 'textMarkSetVisible'
--
textMarkVisible :: Attr TextMark Bool
textMarkVisible = Attr 
  textMarkGetVisible
  textMarkSetVisible
