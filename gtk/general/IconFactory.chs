-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: IconFactory
--
--  Author : Axel Simon
--          
--  Created: 24 May 2001
--
--  Version $Revision: 1.1.1.1 $ from $Date: 2002/03/24 21:56:20 $
--
--  Copyright (c) [1999.2001] Manuel Chakravarty, Axel Simon
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
--- DESCRIPTION ---------------------------------------------------------------
--
-- * This module provides access to IconFactory, IconSet and IconSource.
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------
--
-- * The following functions are not bound:
--   iconFactoryLookup, iconFactoryLookupDefault
--   It is not a good idea to lookup an IconSet directly. If an Icon needs to
--   be displayed it happends always in the context of a widget. The best
--   practice is to get the widgets Style and call styleLookupIconSet.
--
module IconFactory(
  IconFactory,
  iconFactoryNew,
  iconFactoryAdd,
  iconFactoryAddDefault,
  iconFactoryRemoveDefault,
  IconSet,
  iconSetNew,
  iconSetAddSource,
  IconSource,
  iconSourceNew,
  TextDirection(..),
  iconSourceGetDirection,
  iconSourceSetDirection,
  iconSourceResetDirection,
  iconSourceGetFilename,
  iconSourceSetFilename,
  iconSourceGetSize,
  iconSourceSetSize,
  iconSourceResetSize,
  StateType(..),
  iconSourceGetState,
  iconSourceGetState,
  iconSourceSetState,
  iconSourceResetState,
  IconSize,
  iconSizeMenu,
  iconSizeSmallToolbar,
  iconSizeLargeToolbar,
  iconSizeButton,
  iconSizeDialog,
  iconSizeCheck,
  iconSizeRegister,
  iconSizeRegisterAlias,
  iconSizeFromName,
  iconSizeGetName
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import GObject	(makeNewGObject)
{#import Hierarchy#}
{#import Signal#}
import Enums	(TextDirection(..), StateType(..))
import Structs	(IconSize, iconSizeInvalid, iconSizeMenu, iconSizeSmallToolbar,
		 iconSizeLargeToolbar, iconSizeButton, iconSizeDialog)

{# context lib="gtk" prefix="gtk" #}

{#pointer *IconSource foreign newtype#}

{#pointer *IconSet foreign newtype#}

-- methods


-- Add an IconSet to an IconFactory. (EXPORTED)
--
-- * In order to use the new stock object, the factory as to be added to the
--   default factories by iconFactoryAddDefault.
--
iconFactoryAdd :: String -> IconSet -> IconFactory -> IO ()
iconFactoryAdd stockId iconSet i = withCString stockId $ \strPtr ->
  {#call unsafe icon_factory_add#} i strPtr iconSet

-- Add all entries of the IconFactory to the applications stock object 
-- database. (EXPORTED)
--
iconFactoryAddDefault :: IconFactory -> IO ()
iconFactoryAddDefault = {#call unsafe icon_factory_add_default#}

-- Create a new IconFactory. (EXPORTED)
--
-- * An application should create a new IconFactory and add all needed icons.
--   By calling @iconFactoryAddDefault these icons become available as stock
--   objects and can easily be displayed by @Image. Furthermore, a theme can
--   override the icons defined by the application.
--
iconFactoryNew :: IO IconFactory
iconFactoryNew = makeNewGObject mkIconFactory {#call unsafe icon_factory_new#}

-- Remove an IconFactory from the application's stock database. (EXPORTED)
--
iconFactoryRemoveDefault :: IconFactory -> IO ()
iconFactoryRemoveDefault = {#call unsafe icon_factory_remove_default#}

-- Add an @IconSource (an Icon with attributes) to an @IconSet. (EXPORTED)
--
-- * If an icon is looked up in the IconSet @set the best matching IconSource
--   will be taken. It is therefore advisable to add a default (wildcarded) 
--   icon, than can be used if no exact match is found.
--
iconSetAddSource :: IconSource -> IconSet -> IO ()
iconSetAddSource source set = {#call unsafe icon_set_add_source#} set source

-- Create a new IconSet. (EXPORTED)
--
-- * Each icon in an application is contained in an @IconSet. The @IconSet
--   contains several variants (@IconSource@s) to accomodate for different
--   sizes and states.
--
iconSetNew :: IO IconSet
iconSetNew = do
  isPtr <- {#call unsafe icon_set_new#}
  liftM IconSet $ newForeignPtr isPtr (icon_set_unref isPtr)

foreign import ccall "gtk_icon_set_unref" unsafe
  icon_set_unref :: Ptr IconSet -> IO ()

-- Check if a given IconSize is registered. (EXPORTED)
--
-- * Useful if your application expects a theme to install a set with a
--   specific size. You can test if this actually happend and use another
--   size if not.
--
iconSizeCheck :: IconSize -> IO Bool
iconSizeCheck size = liftM toBool $
  {#call icon_size_lookup#} (fromIntegral size) nullPtr nullPtr

-- Register a new IconSize. (EXPORTED)
--
iconSizeRegister :: String -> Int -> Int -> IO IconSize
iconSizeRegister name width height = liftM fromIntegral $
  withCString name $ \strPtr -> {#call unsafe icon_size_register#} 
  strPtr (fromIntegral width) (fromIntegral height)

-- Register an additional alias for a name. (EXPORTED)
--
iconSizeRegisterAlias :: String -> IconSize -> IO ()
iconSizeRegisterAlias alias target = withCString alias $ \strPtr ->
  {#call unsafe icon_size_register_alias#} strPtr (fromIntegral target)

-- Lookup an IconSize by name. (EXPORTED)
--
-- * This fixed value @iconSizeInvalid is returned if the name was not found.
--
iconSizeFromName :: String -> IO IconSize
iconSizeFromName name = liftM fromIntegral $
  withCString name {#call unsafe icon_size_from_name#}

-- Lookup the name of an IconSize. (EXPORTED)
--
-- * Returns Nothing if the name was not found.
--
iconSizeGetName :: IconSize -> IO (Maybe String)
iconSizeGetName size = do
  strPtr <- {#call unsafe icon_size_get_name#} (fromIntegral size)
  if strPtr==nullPtr then return Nothing else liftM Just $ peekCString strPtr

-- Retrieve the @TextDirection of this IconSource. (EXPORTED)
--
-- * Nothing is returned if no explicit direction was set.
--
iconSourceGetDirection :: IconSource -> IO (Maybe TextDirection)
iconSourceGetDirection is = do
  res <- {#call icon_source_get_direction_wildcarded#} is
  if (toBool res) then return Nothing else liftM (Just .toEnum.fromIntegral) $
    {#call unsafe icon_source_get_direction#} is

-- Retrieve the filename this IconSource was based on. (EXPORTED)
--
-- * Returns Nothing if the IconSource was generated by a Pixbuf.
--
iconSourceGetFilename :: IconSource -> IO (Maybe String)
iconSourceGetFilename is = do
  strPtr <- {#call unsafe icon_source_get_filename#} is
  if strPtr==nullPtr then return Nothing else liftM Just $ peekCString strPtr

-- Retrieve the @IconSize of this IconSource. (EXPORTED)
--
-- * Nothing is returned if no explicit size was set (i.e. this @IconSource
--   matches all sizes).
--
iconSourceGetSize :: IconSource -> IO (Maybe IconSize)
iconSourceGetSize is = do
  res <- {#call unsafe icon_source_get_size_wildcarded#} is
  if (toBool res) then return Nothing else liftM (Just .fromIntegral) $
    {#call unsafe icon_source_get_size#} is

-- Retrieve the @StateType of this @IconSource. (EXPORTED)
--
-- * Nothing is returned if the @IconSource matches all states.
--
iconSourceGetState :: IconSource -> IO (Maybe StateType)
iconSourceGetState is = do
  res <- {#call unsafe icon_source_get_state_wildcarded#} is
  if (toBool res) then return Nothing else liftM (Just .toEnum.fromIntegral) $
    {#call unsafe icon_source_get_state#} is

-- Create a new IconSource. (EXPORTED)
--
-- * An IconSource is a single image that is usually added to an IconSet.
--   Next to the image it contains information about which state,
--   text direction and size it should apply.
--
iconSourceNew :: IO IconSource
iconSourceNew = do
  isPtr <- {#call unsafe icon_source_new#}
  liftM IconSource $ newForeignPtr isPtr (icon_source_free isPtr)

foreign import ccall "gtk_icon_source_free" unsafe
  icon_source_free :: Ptr IconSource -> IO ()

-- Mark this @IconSource that it should only apply to the specified 
-- @TextDirection. (EXPORTED)
--
iconSourceSetDirection :: TextDirection -> IconSource -> IO ()
iconSourceSetDirection td is = do
  {#call unsafe icon_source_set_direction_wildcarded#} is (fromBool False)
  {#call unsafe icon_source_set_direction#} is ((fromIntegral.fromEnum) td)

-- Reset the specific @TextDirection set with @iconSourceSetDirection.
-- (EXPORTED)
--
iconSourceResetDirection is =
  {#call unsafe icon_source_set_direction_wildcarded#} is (fromBool True)

-- Load an icon picture from this filename. (EXPORTED)
--
iconSourceSetFilename :: FilePath -> IconSource -> IO ()
iconSourceSetFilename name is = 
  withCString name $ {#call unsafe icon_source_set_filename#} is

-- Set this @IconSource to a specific size. (EXPORTED)
--
iconSourceSetSize :: IconSize -> IconSource -> IO ()
iconSourceSetSize size is = do
  {#call unsafe icon_source_set_size_wildcarded#} is (fromBool False)
  {#call unsafe icon_source_set_size#} is (fromIntegral size)

-- Reset the @IconSize of this @IconSource so that is matches anything.
-- (EXPORTED)
--
iconSourceResetSize :: IconSource -> IO ()
iconSourceResetSize is = 
  {#call unsafe icon_source_set_size_wildcarded#} is (fromBool True)

-- Mark this icon to be used only with this specific state. (EXPORTED)
--
iconSourceSetState :: StateType -> IconSource -> IO ()
iconSourceSetState state is = do
  {#call unsafe icon_source_set_state_wildcarded#} is (fromBool False)
  {#call unsafe icon_source_set_state#} is ((fromIntegral.fromEnum) state)

-- Reset the @StateType of this @IconSource so that is matches anything.
-- (EXPORTED)
--
iconSourceResetState :: IconSource -> IO ()
iconSourceResetState is = 
  {#call unsafe icon_source_set_state_wildcarded#} is (fromBool True)




