-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry IconFactory@
--
--  Author : Axel Simon
--          
--  Created: 24 May 2001
--
--  Version $Revision: 1.3 $ from $Date: 2002/08/05 16:41:34 $
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
-- @description@ --------------------------------------------------------------
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


-- @method iconFactoryAdd@ Add an IconSet to an IconFactory.
--
-- * In order to use the new stock object, the factory as to be added to the
--   default factories by iconFactoryAddDefault.
--
iconFactoryAdd :: IconFactory -> String -> IconSet -> IO ()
iconFactoryAdd i stockId iconSet = withCString stockId $ \strPtr ->
  {#call unsafe icon_factory_add#} i strPtr iconSet

-- @method iconFactoryAddDefault@ Add all entries of the IconFactory to the
-- applications stock object database.
--
iconFactoryAddDefault :: IconFactory -> IO ()
iconFactoryAddDefault  = {#call unsafe icon_factory_add_default#}

-- @constructor iconFactoryNew@ Create a new IconFactory.
--
-- * An application should create a new IconFactory and add all needed icons.
--   By calling @ref method iconFactoryAddDefault@ these icons become
--   available as stock objects and can easily be displayed by
--   @ref arg Image@. Furthermore, a theme can override the icons defined by
--   the application.
--
iconFactoryNew :: IO IconFactory
iconFactoryNew  = makeNewGObject mkIconFactory {#call unsafe icon_factory_new#}

-- @method iconFactoryRemoveDefault@ Remove an IconFactory from the
-- application's stock database.
--
iconFactoryRemoveDefault :: IconFactory -> IO ()
iconFactoryRemoveDefault  = {#call unsafe icon_factory_remove_default#}

-- @method iconSetAddSource@ Add an @ref type IconSource@ (an Icon with
-- attributes) to an @ref type IconSet@.
--
-- * If an icon is looked up in the IconSet @ref arg set@ the best matching
--   IconSource will be taken. It is therefore advisable to add a default
--   (wildcarded) icon, than can be used if no exact match is found.
--
iconSetAddSource :: IconSet -> IconSource -> IO ()
iconSetAddSource set source = {#call unsafe icon_set_add_source#} set source

-- @constructor iconSetNew@ Create a new IconSet.
--
-- * Each icon in an application is contained in an @ref data IconSet@. The
--   @ref data IconSet@ contains several variants (@ref data IconSource@s) to
--   accomodate for different sizes and states.
--
iconSetNew :: IO IconSet
iconSetNew  = do
  isPtr <- {#call unsafe icon_set_new#}
  liftM IconSet $ newForeignPtr isPtr (icon_set_unref isPtr)

foreign import ccall "gtk_icon_set_unref" unsafe
  icon_set_unref :: Ptr IconSet -> IO ()

-- @method iconSizeCheck@ Check if a given IconSize is registered.
--
-- * Useful if your application expects a theme to install a set with a
--   specific size. You can test if this actually happend and use another size
--   if not.
--
iconSizeCheck :: IconSize -> IO Bool
iconSizeCheck size = liftM toBool $
  {#call icon_size_lookup#} (fromIntegral size) nullPtr nullPtr

-- @method iconSizeRegister@ Register a new IconSize.
--
iconSizeRegister :: Int -> String -> Int -> IO IconSize
iconSizeRegister height name width = liftM fromIntegral $
  withCString name $ \strPtr -> {#call unsafe icon_size_register#} 
  strPtr (fromIntegral width) (fromIntegral height)

-- @method iconSizeRegisterAlias@ Register an additional alias for a name.
--
iconSizeRegisterAlias :: IconSize -> String -> IO ()
iconSizeRegisterAlias target alias = withCString alias $ \strPtr ->
  {#call unsafe icon_size_register_alias#} strPtr (fromIntegral target)

-- @method iconSizeFromName@ Lookup an IconSize by name.
--
-- * This fixed value @ref method iconSizeInvalid@ is returned if the name was
--   not found.
--
iconSizeFromName :: String -> IO IconSize
iconSizeFromName name = liftM fromIntegral $
  withCString name {#call unsafe icon_size_from_name#}

-- @method iconSizeGetName@ Lookup the name of an IconSize.
--
-- * Returns Nothing if the name was not found.
--
iconSizeGetName :: IconSize -> IO (Maybe String)
iconSizeGetName size = do
  strPtr <- {#call unsafe icon_size_get_name#} (fromIntegral size)
  if strPtr==nullPtr then return Nothing else liftM Just $ peekCString strPtr

-- @method iconSourceGetDirection@ Retrieve the @ref arg TextDirection@ of
-- this IconSource.
--
-- * Nothing is returned if no explicit direction was set.
--
iconSourceGetDirection :: IconSource -> IO (Maybe TextDirection)
iconSourceGetDirection is = do
  res <- {#call icon_source_get_direction_wildcarded#} is
  if (toBool res) then return Nothing else liftM (Just .toEnum.fromIntegral) $
    {#call unsafe icon_source_get_direction#} is

-- @method iconSourceGetFilename@ Retrieve the filename this IconSource was
-- based on.
--
-- * Returns Nothing if the IconSource was generated by a Pixbuf.
--
iconSourceGetFilename :: IconSource -> IO (Maybe String)
iconSourceGetFilename is = do
  strPtr <- {#call unsafe icon_source_get_filename#} is
  if strPtr==nullPtr then return Nothing else liftM Just $ peekCString strPtr

-- @method iconSourceGetSize@ Retrieve the @ref type IconSize@ of this
-- IconSource.
--
-- * Nothing is returned if no explicit size was set (i.e. this
--   @ref type IconSource@ matches all sizes).
--
iconSourceGetSize :: IconSource -> IO (Maybe IconSize)
iconSourceGetSize is = do
  res <- {#call unsafe icon_source_get_size_wildcarded#} is
  if (toBool res) then return Nothing else liftM (Just .fromIntegral) $
    {#call unsafe icon_source_get_size#} is

-- @method iconSourceGetState@ Retrieve the @ref arg StateType@ of this
-- @ref type IconSource@.
--
-- * Nothing is returned if the @ref type IconSource@ matches all states.
--
iconSourceGetState :: IconSource -> IO (Maybe StateType)
iconSourceGetState is = do
  res <- {#call unsafe icon_source_get_state_wildcarded#} is
  if (toBool res) then return Nothing else liftM (Just .toEnum.fromIntegral) $
    {#call unsafe icon_source_get_state#} is

-- @constructor iconSourceNew@ Create a new IconSource.
--
-- * An IconSource is a single image that is usually added to an IconSet. Next
--   to the image it contains information about which state, text direction
--   and size it should apply.
--
iconSourceNew :: IO IconSource
iconSourceNew  = do
  isPtr <- {#call unsafe icon_source_new#}
  liftM IconSource $ newForeignPtr isPtr (icon_source_free isPtr)

foreign import ccall "gtk_icon_source_free" unsafe
  icon_source_free :: Ptr IconSource -> IO ()

-- @method iconSourceSetDirection@ Mark this @ref type IconSource@ that it
-- should only apply to the specified @ref arg TextDirection@.
--
iconSourceSetDirection :: IconSource -> TextDirection -> IO ()
iconSourceSetDirection is td = do
  {#call unsafe icon_source_set_direction_wildcarded#} is (fromBool False)
  {#call unsafe icon_source_set_direction#} is ((fromIntegral.fromEnum) td)

-- @method iconSourceResetDirection@ Reset the specific
-- @ref arg TextDirection@ set with @ref method iconSourceSetDirection@.
--
iconSourceResetDirection is =
  {#call unsafe icon_source_set_direction_wildcarded#} is (fromBool True)

-- @method iconSourceSetFilename@ Load an icon picture from this filename.
--
iconSourceSetFilename :: IconSource -> FilePath -> IO ()
iconSourceSetFilename is name = 
  withCString name $ {#call unsafe icon_source_set_filename#} is

-- @method iconSourceSetSize@ Set this @ref type IconSource@ to a specific
-- size.
--
iconSourceSetSize :: IconSource -> IconSize -> IO ()
iconSourceSetSize is size = do
  {#call unsafe icon_source_set_size_wildcarded#} is (fromBool False)
  {#call unsafe icon_source_set_size#} is (fromIntegral size)

-- @method iconSourceResetSize@ Reset the @ref type IconSize@ of this
-- @ref type IconSource@ so that is matches anything.
--
iconSourceResetSize :: IconSource -> IO ()
iconSourceResetSize is = 
  {#call unsafe icon_source_set_size_wildcarded#} is (fromBool True)

-- @method iconSourceSetState@ Mark this icon to be used only with this
-- specific state.
--
iconSourceSetState :: IconSource -> StateType -> IO ()
iconSourceSetState is state = do
  {#call unsafe icon_source_set_state_wildcarded#} is (fromBool False)
  {#call unsafe icon_source_set_state#} is ((fromIntegral.fromEnum) state)

-- @method iconSourceResetState@ Reset the @ref arg StateType@ of this
-- @ref type IconSource@ so that is matches anything.
--
iconSourceResetState :: IconSource -> IO ()
iconSourceResetState is = 
  {#call unsafe icon_source_set_state_wildcarded#} is (fromBool True)




