{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) IconFactory
--
--  Author : Axel Simon
--
--  Created: 24 May 2001
--
--  Copyright (C) 1999-2005 Axel Simon
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
-- Manipulating stock icons
--
module Graphics.UI.Gtk.General.IconFactory (
-- * Detail
--
-- | Browse the available stock icons in the list of stock IDs found here. You
-- can also use the gtk-demo application for this purpose.
--
-- An icon factory manages a collection of 'IconSet'; a 'IconSet' manages a
-- set of variants of a particular icon (i.e. a 'IconSet' contains variants for
-- different sizes and widget states). Icons in an icon factory are named by a
-- stock ID, which is a simple string identifying the icon. Each 'Style' has a
-- list of 'IconFactory' derived from the current theme; those icon factories
-- are consulted first when searching for an icon. If the theme doesn't set a
-- particular icon, Gtk+ looks for the icon in a list of default icon
-- factories, maintained by 'iconFactoryAddDefault' and
-- 'iconFactoryRemoveDefault'. Applications with icons should add a default
-- icon factory with their icons, which will allow themes to override the icons
-- for the application.
--
-- To display an icon, always use
-- 'Graphics.UI.Gtk.General.Style.styleLookupIconSet' on the widget that
-- will display the icon, or the convenience function
-- 'Graphics.UI.Gtk.Abstract.Widget.widgetRenderIcon'. These
-- functions take the theme into account when looking up the icon to use for a
-- given stock ID.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----IconFactory
-- @

-- * Types
  IconFactory,
  IconFactoryClass,
  castToIconFactory, gTypeIconFactory,
  toIconFactory,

-- * Constructors
  iconFactoryNew,

-- * Methods
  iconFactoryAdd,
  iconFactoryAddDefault,
  iconFactoryLookup,
  iconFactoryLookupDefault,
  iconFactoryRemoveDefault,
  IconSet,
  iconSetNew,
  iconSetNewFromPixbuf,
  iconSetAddSource,
  iconSetRenderIcon,
  iconSetGetSizes,
  IconSource,
  iconSourceNew,
  TextDirection(..),
  iconSourceGetDirection,
  iconSourceSetDirection,
  iconSourceResetDirection,
  iconSourceGetFilename,
  iconSourceSetFilename,
  iconSourceGetPixbuf,
  iconSourceSetPixbuf,
  iconSourceGetSize,
  iconSourceSetSize,
  iconSourceResetSize,
  StateType(..),
  iconSourceGetState,
  iconSourceSetState,
  iconSourceResetState,
  IconSize(..),
  iconSizeCheck,
  iconSizeRegister,
  iconSizeRegisterAlias,
  iconSizeFromName,
  iconSizeGetName
  ) where

import Control.Applicative
import Prelude
import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.UTFString
{#import Graphics.UI.Gtk.Types#}
import Graphics.UI.Gtk.General.Enums    (TextDirection(..), StateType(..))
import Graphics.UI.Gtk.General.StockItems
import Graphics.UI.Gtk.General.Structs  (IconSize(..))

{# context lib="gtk" prefix="gtk" #}

{#pointer *IconSource foreign newtype#}

{#pointer *IconSet foreign newtype#}

-- The Show instance for IconSize is here since we need c2hs.
instance Show IconSize where
  show i = unsafePerformIO (lookupSizeString (fromEnum i))
    where
    lookupSizeString n = do
      ptr <- {#call unsafe icon_size_get_name#} (fromIntegral n)
      if ptr==nullPtr then return "" else glibToString <$> peekUTFString ptr

--------------------
-- Constructors

-- | Create a new IconFactory.
--
-- * An application should create a new 'IconFactory' and add all
--   needed icons.
--   By calling 'iconFactoryAddDefault' these icons become
--   available as stock objects and can easily be displayed by
--   'Image'. Furthermore, a theme can override the icons defined by
--   the application.
--
iconFactoryNew :: IO IconFactory
iconFactoryNew  =
  wrapNewGObject mkIconFactory {#call unsafe icon_factory_new#}

--------------------
-- Methods

-- | Add an IconSet to an IconFactory.
--
-- In order to use the new stock object, the factory as to be added to the
-- default factories by 'iconFactoryAddDefault'.
--
iconFactoryAdd :: IconFactory -> StockId -> IconSet -> IO ()
iconFactoryAdd i stockId iconSet = withUTFString stockId $ \strPtr ->
  {#call unsafe icon_factory_add#} i strPtr iconSet

-- | Add all entries of the IconFactory to the
-- applications stock object database.
--
iconFactoryAddDefault :: IconFactory -> IO ()
iconFactoryAddDefault  = {#call unsafe icon_factory_add_default#}

-- | Looks up the stock id in the icon factory, returning an icon set if found,
-- otherwise Nothing.
--
-- For display to the user, you should use
-- 'Graphics.UI.Gtk.General.Style.styleLookupIconSet' on the
-- 'Graphics.UI.Gtk.General.Style.Style'
-- for the widget that will display the icon, instead of using this function
-- directly, so that themes are taken into account.
--
iconFactoryLookup :: IconFactory -> StockId -> IO (Maybe IconSet)
iconFactoryLookup i stockId =
  withUTFString stockId $ \strPtr -> do
  iconSetPtr <- {#call unsafe icon_factory_lookup#} i strPtr
  if iconSetPtr == nullPtr then return Nothing else liftM (Just . IconSet) $
    newForeignPtr iconSetPtr icon_set_unref

-- | Looks for an icon in the list of default icon factories.
--
-- For display to the user, you should use
-- 'Graphics.UI.Gtk.General.Style.styleLookupIconSet' on the
-- 'Graphics.UI.Gtk.General.Style.Style'
-- for the widget that will display the icon, instead of using this function
-- directly, so that themes are taken into account.
--
iconFactoryLookupDefault :: StockId -> IO (Maybe IconSet)
iconFactoryLookupDefault stockId =
  withUTFString stockId $ \strPtr -> do
  iconSetPtr <- {#call unsafe icon_factory_lookup_default#} strPtr
  if iconSetPtr == nullPtr then return Nothing else liftM (Just . IconSet) $
    newForeignPtr iconSetPtr icon_set_unref

-- | Remove an IconFactory from the
-- application's stock database.
--
iconFactoryRemoveDefault :: IconFactory -> IO ()
iconFactoryRemoveDefault  = {#call unsafe icon_factory_remove_default#}

-- | Add an 'IconSource' (an Icon with
-- attributes) to an 'IconSet'.
--
-- * If an icon is looked up in the IconSet @set@ the best matching
--   IconSource will be taken. It is therefore advisable to add a default
--   (wildcarded) icon, than can be used if no exact match is found.
--
iconSetAddSource :: IconSet -> IconSource -> IO ()
iconSetAddSource set source = {#call unsafe icon_set_add_source#} set source

iconSetRenderIcon :: WidgetClass widget => IconSet
                  -> TextDirection
                  -> StateType
                  -> IconSize
                  -> widget
                  -> IO Pixbuf
iconSetRenderIcon set dir state size widget = wrapNewGObject mkPixbuf $
  {#call icon_set_render_icon#} set (Style nullForeignPtr)
    ((fromIntegral.fromEnum) dir) ((fromIntegral.fromEnum) state)
    ((fromIntegral.fromEnum) size) (toWidget widget) nullPtr

-- | Create a new IconSet.
--
-- * Each icon in an application is contained in an 'IconSet'. The
--   'IconSet' contains several variants ('IconSource's) to
--   accomodate for different sizes and states.
--
iconSetNew :: IO IconSet
iconSetNew  = do
  isPtr <- {#call unsafe icon_set_new#}
  liftM IconSet $ newForeignPtr isPtr icon_set_unref

-- | Creates a new 'IconSet' with the given pixbuf as the default\/fallback
-- source image. If you don't add any additional "IconSource" to the icon set,
-- all variants of the icon will be created from the pixbuf, using scaling,
-- pixelation, etc. as required to adjust the icon size or make the icon look
-- insensitive\/prelighted.
--
iconSetNewFromPixbuf :: Pixbuf -> IO IconSet
iconSetNewFromPixbuf pixbuf = do
  isPtr <- {#call unsafe icon_set_new_from_pixbuf#} pixbuf
  liftM IconSet $ newForeignPtr isPtr icon_set_unref

-- | Obtains a list of icon sizes this icon set can render.
--
iconSetGetSizes :: IconSet -> IO [IconSize]
iconSetGetSizes set =
  alloca $ \sizesArrPtr -> alloca $ \lenPtr -> do
  {#call unsafe icon_set_get_sizes#} set sizesArrPtr lenPtr
  len <- peek lenPtr
  sizesArr <- peek sizesArrPtr
  list <- peekArray (fromIntegral len) sizesArr
  {#call unsafe g_free#} (castPtr sizesArr)
  return $ map (toEnum.fromIntegral) list

foreign import ccall unsafe "&gtk_icon_set_unref"
  icon_set_unref :: FinalizerPtr IconSet

-- | Check if a given IconSize is registered.
--
-- * Useful if your application expects a theme to install a set with a
--   specific size. You can test if this actually happend and use another size
--   if not.
--
iconSizeCheck :: IconSize -> IO Bool
iconSizeCheck size = liftM toBool $
  {#call icon_size_lookup#} ((fromIntegral . fromEnum) size) nullPtr nullPtr

-- | Register a new IconSize.
--
iconSizeRegister :: GlibString string
  => string -- ^ the new name of the size
  -> Int -- ^ the width of the icon
  -> Int -- ^ the height of the icon
  -> IO IconSize -- ^ the new icon size
iconSizeRegister name width height = liftM (toEnum . fromIntegral) $
  withUTFString name $ \strPtr -> {#call unsafe icon_size_register#}
  strPtr (fromIntegral width) (fromIntegral height)

-- | Register an additional alias for a name.
--
iconSizeRegisterAlias :: GlibString string => IconSize -> string -> IO ()
iconSizeRegisterAlias target alias = withUTFString alias $ \strPtr ->
  {#call unsafe icon_size_register_alias#} strPtr ((fromIntegral . fromEnum) target)

-- | Lookup an IconSize by name.
--
-- * This fixed value 'iconSizeInvalid' is returned if the name was
--   not found.
--
iconSizeFromName :: GlibString string => string -> IO IconSize
iconSizeFromName name = liftM (toEnum . fromIntegral) $
  withUTFString name {#call unsafe icon_size_from_name#}

-- | Lookup the name of an IconSize.
--
-- * Returns @Nothing@ if the name was not found.
--
iconSizeGetName :: GlibString string => IconSize -> IO (Maybe string)
iconSizeGetName size = do
  strPtr <- {#call unsafe icon_size_get_name#} ((fromIntegral . fromEnum) size)
  if strPtr==nullPtr then return Nothing else liftM Just $ peekUTFString strPtr

-- | Retrieve the 'TextDirection' of
-- this IconSource.
--
-- * @Nothing@ is returned if no explicit direction was set.
--
iconSourceGetDirection :: IconSource -> IO (Maybe TextDirection)
iconSourceGetDirection is = do
  res <- {#call icon_source_get_direction_wildcarded#} is
  if (toBool res) then return Nothing else liftM (Just .toEnum.fromIntegral) $
    {#call unsafe icon_source_get_direction#} is

-- | Retrieve the filename this IconSource was
-- based on.
--
-- * Returns @Nothing@ if the IconSource was generated by a Pixbuf.
--
iconSourceGetFilename :: GlibString string => IconSource -> IO (Maybe string)
iconSourceGetFilename is = do
#if defined (WIN32) && GTK_CHECK_VERSION(2,6,0) && GTK_MAJOR_VERSION < 3
  strPtr <- {#call unsafe icon_source_get_filename_utf8#} is
#else
  strPtr <- {#call unsafe icon_source_get_filename#} is
#endif
  if strPtr==nullPtr then return Nothing else liftM Just $ peekUTFString strPtr

-- | Retrieve the 'IconSize' of this
-- IconSource.
--
-- * @Nothing@ is returned if no explicit size was set (i.e. this
--   'IconSource' matches all sizes).
--
iconSourceGetSize :: IconSource -> IO (Maybe IconSize)
iconSourceGetSize is = do
  res <- {#call unsafe icon_source_get_size_wildcarded#} is
  if (toBool res) then return Nothing else liftM (Just . toEnum . fromIntegral) $
    {#call unsafe icon_source_get_size#} is

-- | Retrieve the 'StateType' of this
-- 'IconSource'.
--
-- * @Nothing@ is returned if the 'IconSource' matches all
--   states.
--
iconSourceGetState :: IconSource -> IO (Maybe StateType)
iconSourceGetState is = do
  res <- {#call unsafe icon_source_get_state_wildcarded#} is
  if (toBool res) then return Nothing else liftM (Just .toEnum.fromIntegral) $
    {#call unsafe icon_source_get_state#} is

-- | Create a new IconSource.
--
-- * An IconSource is a single image that is usually added to an IconSet. Next
--   to the image it contains information about which state, text direction
--   and size it should apply.
--
iconSourceNew :: IO IconSource
iconSourceNew  = do
  isPtr <- {#call unsafe icon_source_new#}
  liftM IconSource $ newForeignPtr isPtr icon_source_free

foreign import ccall unsafe "&gtk_icon_source_free"
  icon_source_free :: FinalizerPtr IconSource

-- | Mark this 'IconSource' that it
-- should only apply to the specified 'TextDirection'.
--
iconSourceSetDirection :: IconSource -> TextDirection -> IO ()
iconSourceSetDirection is td = do
  {#call unsafe icon_source_set_direction_wildcarded#} is (fromBool False)
  {#call unsafe icon_source_set_direction#} is ((fromIntegral.fromEnum) td)

-- | Reset the specific
-- 'TextDirection' set with 'iconSourceSetDirection'.
--
iconSourceResetDirection :: IconSource -> IO ()
iconSourceResetDirection is =
  {#call unsafe icon_source_set_direction_wildcarded#} is (fromBool True)

-- | Load an icon picture from this filename.
--
iconSourceSetFilename :: GlibFilePath fp => IconSource -> fp -> IO ()
iconSourceSetFilename is name =
#if defined (WIN32) && GTK_CHECK_VERSION(2,6,0) && GTK_MAJOR_VERSION < 3
  withUTFFilePath name $ {# call unsafe icon_source_set_filename_utf8 #} is
#else
  withUTFFilePath name $ {# call unsafe icon_source_set_filename #} is
#endif

-- | Retrieves the source pixbuf, or Nothing if none is set.
--
iconSourceGetPixbuf :: IconSource -> IO (Maybe Pixbuf)
iconSourceGetPixbuf is = maybeNull (makeNewGObject mkPixbuf) $
  {#call unsafe icon_source_get_pixbuf#} is

-- | Sets a pixbuf to use as a base image when creating icon variants for
-- 'IconSet'.
--
iconSourceSetPixbuf :: IconSource -> Pixbuf -> IO ()
iconSourceSetPixbuf is pb = do
  {#call icon_source_set_pixbuf#} is pb

-- | Set this 'IconSource' to a specific
-- size.
--
iconSourceSetSize :: IconSource -> IconSize -> IO ()
iconSourceSetSize is size = do
  {#call unsafe icon_source_set_size_wildcarded#} is (fromBool False)
  {#call unsafe icon_source_set_size#} is ((fromIntegral . fromEnum) size)

-- | Reset the 'IconSize' of this
-- 'IconSource' so that is matches anything.
--
iconSourceResetSize :: IconSource -> IO ()
iconSourceResetSize is =
  {#call unsafe icon_source_set_size_wildcarded#} is (fromBool True)

-- | Mark this icon to be used only with this
-- specific state.
--
iconSourceSetState :: IconSource -> StateType -> IO ()
iconSourceSetState is state = do
  {#call unsafe icon_source_set_state_wildcarded#} is (fromBool False)
  {#call unsafe icon_source_set_state#} is ((fromIntegral.fromEnum) state)

-- | Reset the 'StateType' of this
-- 'IconSource' so that is matches anything.
--
iconSourceResetState :: IconSource -> IO ()
iconSourceResetState is =
  {#call unsafe icon_source_set_state_wildcarded#} is (fromBool True)
