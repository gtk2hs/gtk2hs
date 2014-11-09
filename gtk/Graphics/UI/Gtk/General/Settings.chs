{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Clipboard
--
--  Author : Axel Simon
--
--  Created: 26 March 2007
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
-- I removed all definitions for the clipboard by Juergen Nicklisch since
-- the way the clipboards were selected didn't tie in with the Selection
-- module.
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Storing data on clipboards
--
module Graphics.UI.Gtk.General.Settings (

-- * Class Hierarchy
--
-- |
-- @
-- |  'GObject'
-- |   +----Settings
-- @

-- * Types
  Settings,
  SettingsClass,
  castToSettings, gTypeSettings,
  toSettings,

-- * Methods
  settingsGetDefault,
#if GTK_CHECK_VERSION(2,2,0)
  settingsGetForScreen,
#endif
  settingsSetLongProperty,
  settingsSetStringProperty
  ) where

import System.Glib.FFI
import System.Glib.UTFString
{#import Graphics.UI.Gtk.Types#}

{# context lib="gtk" prefix="gtk" #}


--------------------
-- Methods

-- | Gets the Settings object for the default GDK screen, creating
-- it if necessary. See 'settingsGetForScreen'.
--
settingsGetDefault ::
    IO (Maybe Settings) -- ^ returns a Settings. If there is no default
                        -- screen, then returns Nothing.
settingsGetDefault =
  maybeNull (makeNewGObject mkSettings)
    {# call gtk_settings_get_default #}

#if GTK_CHECK_VERSION(2,2,0)
-- | Gets the Settings object for screen, creating it if necessary.
--
settingsGetForScreen ::
    ScreenClass screen
 => screen
 -> IO Settings
settingsGetForScreen screen =
  makeNewGObject mkSettings $
    {# call gtk_settings_get_for_screen #}
      (toScreen screen)
#endif

settingsSetLongProperty ::
    (SettingsClass settings, GlibString string)
 => settings
 -> string
 -> Int
 -> string
 -> IO ()
settingsSetLongProperty settings name value origin =
  withUTFString name $ \namePtr ->
  withUTFString origin $ \originPtr ->
  {# call gtk_settings_set_long_property #}
    (toSettings settings)
    namePtr
    (fromIntegral value)
    originPtr

settingsSetStringProperty ::
    (SettingsClass settings, GlibString string)
 => settings
 -> string
 -> string
 -> string
 -> IO ()
settingsSetStringProperty settings name value origin =
  withUTFString name $ \namePtr ->
  withUTFString value $ \valuePtr ->
  withUTFString origin $ \originPtr ->
  {# call gtk_settings_set_string_property #}
    (toSettings settings)
    namePtr
    valuePtr
    originPtr

