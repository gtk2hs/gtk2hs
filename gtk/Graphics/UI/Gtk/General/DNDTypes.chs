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
  TargetList(TargetList),
  SelectionData,
  SelectionDataM,

-- * Constructors
  atomNew,
  targetListNew,
  mkTargetList
  ) where

import System.Glib.FFI
import System.Glib.UTFString
{#import Graphics.UI.Gtk.Types#} ()
import Control.Monad ( liftM )
import Control.Monad.Reader ( ReaderT )

{# context lib="gtk" prefix="gtk" #}

-- | A number that the application can use to differentiate between different
--   data types or application states.
type InfoId = {#type guint#}

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
newtype Atom = Atom (Ptr ()) deriving Eq

instance Show Atom where
  show (Atom ptr) = show (atomToString ptr :: DefaultGlibString)

atomToString ptr = unsafePerformIO $ do
        strPtr <- {#call unsafe gdk_atom_name#} ptr
        readUTFString strPtr

-- | A 'TargetList' contains information about all possible formats
-- (represented as 'TargetTag') that a widget can create or receive in form of
-- a selection.
--
{#pointer *GtkTargetList as TargetList foreign newtype#}

--------------------
-- Constructors


-- | Create a new 'TargetTag', 'SelectionTag', 'SelectionTypeTag' or
--   'PropertyTag'. Note that creating two target tags with the same name will
--   create the same tag, in particular, the tag will be the same across
--   different applications. Note that the name of an 'Atom' can be printed
--   by 'show' though comparing the atom is merely an integer comparison.
--
atomNew :: GlibString string => string -> IO Atom
atomNew name = withUTFString name $ \strPtr ->
  liftM Atom $ {#call unsafe gdk_atom_intern#} strPtr 0

-- | Create a new, empty 'TargetList'.
--
targetListNew :: IO TargetList
targetListNew = do
  tlPtr <- {#call unsafe target_list_new#} nullPtr 0
  liftM TargetList $ newForeignPtr tlPtr target_list_unref

foreign import ccall unsafe "&gtk_target_list_unref"
  target_list_unref :: FinalizerPtr TargetList

-- Wrap a 'TargetList' pointer.
mkTargetList :: Ptr TargetList -> IO TargetList
mkTargetList tlPtr = do
  tl <- liftM TargetList $ newForeignPtr tlPtr target_list_unref
  {#call unsafe target_list_ref#} tl
  return tl

-- | A pointer to selection data.
{#pointer *SelectionData #}

-- | A monad providing access to selection data.
--
type SelectionDataM a = ReaderT (Ptr ()) IO a
