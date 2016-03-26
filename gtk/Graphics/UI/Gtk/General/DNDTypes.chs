{-# OPTIONS_HADDOCK hide #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Type declarations for DND and Selections
--
--  Author : Axel Simon
--
--  Created: 11 April 2007
--
--  Copyright (C) 2007 Axel Simon
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
-- functions that seem to be internal: gtk_selection_convert
-- functions that relate to target tables are not bound since they seem
-- superfluous
--
-- Type declarations for Selections that are used for DND and Clipboards.
-- #hide
module Graphics.UI.Gtk.General.DNDTypes (

-- * Types
  InfoId,
  TargetTag,
  SelectionTag,
  SelectionTypeTag,
  PropertyTag,
  Atom(Atom),
  SelectionDataM,
  ) where

import Control.Monad.Reader ( ReaderT )
import Data.Word (Word32)
import Foreign.Ptr (Ptr)
import GI.Gdk.Structs (Atom(..))
import GI.Gtk.Structs (SelectionData)

-- | A number that the application can use to differentiate between different
--   data types or application states.
type InfoId = Word32

-- | A tag that uniquely identifies a selection. A selection denotes the
-- exchange mechanism that is being used, for instance, the clipboard is the
-- most common exchange mechanism. For drag and drop applications, a new
-- selection tag is usually created for each different kind of data that is
-- being exchanged.
type SelectionTag = Atom

-- | A tag that uniquely identifies a target. A target describes the format of
-- the underlying data source, for instance, it might be a string. A single
-- selection may support multiple targets: suppose a new target is created for
-- the Haskell data type 'Double'. In this case, the value of the floating
-- point number could also be offered as a string.
type TargetTag = Atom

-- | A tag that defines the encoding of the binary data. For instance, a
-- string might be encoded as UTF-8 or in a different locale. Each encoding
-- would use the same 'TargetTag' but a different 'SelectionTypeTag'.
type SelectionTypeTag = Atom

-- | A tag
-- that uniquely identifies a property of a
-- 'Graphics.UI.Gtk.Gdk.DrawWindow.DrawWindow'.
--
type PropertyTag = Atom

-- | An atom is an index into a global string table. It is possible to
-- associate binary data with each entry. This facility is used for
-- inter-application data exchange such as properties of
-- 'Graphics.UI.Gtk.Gdk.DrawWindow.DrawWindow' (using 'PropertyTag'),
-- 'Graphics.UI.Gtk.Clipboard.Clipboard' or 'Graphics.UI.Gtk.General.Drag'
-- ('SelectionId' and 'TargetId').

-- | A monad providing access to selection data.
--
type SelectionDataM a = ReaderT (Ptr SelectionData) IO a
