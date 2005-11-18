-- -*-haskell-*-
--  GIMP Toolkit (GTK) TextMark TextBuffer
--
--  Author : Axel Simon
--
--  Created: 23 February 2002
--
--  Version $Revision: 1.8 $ from $Date: 2005/11/18 15:54:57 $
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
-- * Detail
-- 
-- | You may wish to begin by reading the text widget conceptual overview
-- which gives an overview of all the objects and data types related to the
-- text widget and how they work together.
--
-- A 'TextMark' is like a bookmark in a text buffer; it preserves a position
-- in the text. You can convert the mark to an iterator using
-- 'Graphics.UI.Gtk.Multiline.TextBuffer.textBufferGetIterAtMark'. Unlike
-- iterators, marks remain valid across buffer mutations, because their
-- behavior is defined when text is inserted or deleted. When text containing
-- a mark is deleted, the mark remains in the position originally occupied by
-- the deleted text. When text is inserted at a mark, a mark with left
-- gravity will be moved to the beginning of the newly-inserted text, and a
-- mark with right gravity will be moved to the end.
--
-- Marks can be deleted from the buffer at any time with
-- 'Graphics.UI.Gtk.Multiline.TextBuffer.textBufferDeleteMark'. Once deleted
-- from the buffer, a mark is essentially useless.
--
-- Marks optionally have names; these can be convenient to avoid passing the
-- 'TextMark' object around.
--
-- Marks are typically created using the
-- 'Graphics.UI.Gtk.Multiline.TextBuffer.textBufferCreateMark' function.

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
  toTextMark,
  MarkName,

-- * Methods
  textMarkSetVisible,
  textMarkGetVisible,
  textMarkGetDeleted,
  textMarkGetName,
  textMarkGetBuffer,
  textMarkGetLeftGravity,

-- * Attributes
  textMarkVisible,
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.GObject		(makeNewGObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

type MarkName = String

--------------------
-- Methods

-- | Sets the visibility of @mark@; the insertion point is normally visible,
-- i.e. you can see it as a vertical bar. Also, the text widget uses a visible
-- mark to indicate where a drop will occur when dragging-and-dropping text.
-- Most other marks are not visible. Marks are not visible by default.
--
textMarkSetVisible :: TextMarkClass self => self -> Bool -> IO ()
textMarkSetVisible self setting =
  {# call unsafe text_mark_set_visible #}
    (toTextMark self)
    (fromBool setting)

-- | Returns @True@ if the mark is visible (i.e. a cursor is displayed for it)
--
textMarkGetVisible :: TextMarkClass self => self -> IO Bool
textMarkGetVisible self =
  liftM toBool $
  {# call unsafe text_mark_get_visible #}
    (toTextMark self)

-- | Returns @True@ if the mark has been removed from its buffer with
-- 'Graphics.UI.Gtk.Multiline.TextBuffer.textBufferDeleteMark'. Marks can't
-- be used once deleted.
--
textMarkGetDeleted :: TextMarkClass self => self -> IO Bool
textMarkGetDeleted self =
  liftM toBool $
  {# call unsafe text_mark_get_deleted #}
    (toTextMark self)

-- | Returns the mark name; returns @Nothing@ for anonymous marks.
--
textMarkGetName :: TextMarkClass self => self -> IO (Maybe MarkName)
textMarkGetName self =
  {# call unsafe text_mark_get_name #}
    (toTextMark self)
  >>= maybePeek peekUTFString

-- | Gets the buffer this mark is located inside, or @Nothing@ if the mark is
-- deleted.
--
textMarkGetBuffer :: TextMarkClass self => self -> IO (Maybe TextBuffer)
textMarkGetBuffer self =
  maybeNull (makeNewGObject mkTextBuffer) $
  {# call unsafe text_mark_get_buffer #}
    (toTextMark self)

-- | Determines whether the mark has left gravity.
--
-- The name is misleading as Arabic, Hebrew and some other languages have the
-- beginning of a line towards the right.
--
textMarkGetLeftGravity :: TextMarkClass self => self -> IO Bool
textMarkGetLeftGravity self =
  liftM toBool $
  {# call unsafe text_mark_get_left_gravity #}
    (toTextMark self)

--------------------
-- Attributes

-- | \'visible\' property. See 'textMarkGetVisible' and 'textMarkSetVisible'
--
textMarkVisible :: TextMarkClass self => Attr self Bool
textMarkVisible = newAttr
  textMarkGetVisible
  textMarkSetVisible
