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
-- Type declarations for DND and Selections
-- #hide
module Graphics.UI.Gtk.General.DNDTypes (

-- * Types
  InfoId,
  TargetTag(TargetTag),
  SelectionTag(SelectionTag),
  TargetList(TargetList),
  SelectionData,
  SelectionDataM,
  
-- * Constructors
  targetTagNew,
  selectionTagNew,
  targetListNew,
  mkTargetList  
  ) where

import System.Glib.FFI
{#import Graphics.UI.Gtk.Types#} ()
import System.Glib.UTFString ( peekUTFString, withUTFString )
import Control.Monad ( liftM )
import Control.Monad.Reader ( ReaderT )

{# context lib="gtk" prefix="gtk" #}

-- | A number that the application can use to differentiate between different
--   data types or application states.
type InfoId = {#type guint#}

-- | A tag that uniquely identifies a target.
newtype TargetTag = TargetTag (Ptr ()) deriving Eq

instance Show TargetTag where
  show (TargetTag ptr) = atomToString ptr

-- | A tag that uniquely identifies a selection.
newtype SelectionTag = SelectionTag (Ptr ()) deriving Eq

instance Show SelectionTag where
  show (SelectionTag ptr) = atomToString ptr

atomToString ptr = unsafePerformIO $ do
	strPtr <- {#call unsafe gdk_atom_name#} ptr
	str <- peekUTFString strPtr
	{#call unsafe g_free#} (castPtr strPtr)
	return str

-- | A 'TargetList' contains information about all possible formats
-- (represented as 'TargetTag') that a widget can create or receive in form of
-- a selection.
--
{#pointer *GtkTargetList as TargetList foreign newtype#}

--------------------
-- Constructors


-- | Create a new 'TargetTag'. Note that creating two target tags with the
--   same name will yield two different tags. The name is merely meant to
--   ease application development.
--
targetTagNew :: String -> IO TargetTag
targetTagNew name = withUTFString name $ \strPtr ->
  liftM TargetTag $ {#call unsafe gdk_atom_intern#} strPtr 0

-- | Create a new 'SelectionTag'. Note that creating two selection tags with the
--   same name will yield two different tags. The name is merely meant to
--   ease application development.
--
selectionTagNew :: String -> IO SelectionTag
selectionTagNew name = withUTFString name $ \strPtr ->
  liftM SelectionTag $ {#call unsafe gdk_atom_intern#} strPtr 0

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
