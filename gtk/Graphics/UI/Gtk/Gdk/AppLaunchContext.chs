{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget AppLaunchContext
--
--  Author : Andy Stewart
--
--  Created: 30 Mar 2010
--
--  Copyright (C) 2010 Andy Stewart
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
--
module Graphics.UI.Gtk.Gdk.AppLaunchContext (
#if GTK_CHECK_VERSION(2,14,0)
-- * Types
  AppLaunchContext,
  AppLaunchContextClass,
  castToAppLaunchContext,
  gTypeAppLaunchContext,
  toAppLaunchContext,

-- * Constructors
  appLaunchContextNew,

-- * Methods
  appLaunchContextSetDisplay,
  appLaunchContextSetScreen,
  appLaunchContextSetDesktop,
  appLaunchContextSetTimestamp,
  appLaunchContextSetIconName,
#ifdef HAVE_GIO
  appLaunchContextSetIcon,
#endif
#endif
  ) where

import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.Gdk.EventM (TimeStamp)
{#import Graphics.UI.Gtk.Types#}
#ifdef HAVE_GIO
{#import System.GIO.Types#} (Icon (..), IconClass, toIcon)
#endif

{# context lib="gdk" prefix="gdk" #}

#if GTK_CHECK_VERSION(2,14,0)
--------------------
-- Constructors

-- | Creates a new 'AppLaunchContext'.
appLaunchContextNew :: IO AppLaunchContext
appLaunchContextNew =
  wrapNewGObject mkAppLaunchContext $
  {# call gdk_app_launch_context_new #}

--------------------
-- Methods

-- | Sets the workspace on which applications will be launched when using this context when running under
-- a window manager that supports multiple workspaces, as described in the Extended Window Manager
-- Hints.
--
-- When the workspace is not specified or desktop is set to -1, it is up to the window manager to pick
-- one, typically it will be the current workspace.
appLaunchContextSetDesktop :: AppLaunchContext -> Int -> IO ()
appLaunchContextSetDesktop self desktop =
  {# call gdk_app_launch_context_set_desktop #}
    self
    (fromIntegral desktop)

-- | Sets the display on which applications will be launched when using this context. See also
-- 'appLaunchContextSetScreen'.
appLaunchContextSetDisplay :: AppLaunchContext -> Display -> IO ()
appLaunchContextSetDisplay self display =
  {# call gdk_app_launch_context_set_display #}
    self
    display

#ifdef HAVE_GIO
-- | Sets the icon for applications that are launched with this context.
--
-- Window Managers can use this information when displaying startup notification.
appLaunchContextSetIcon :: IconClass icon => AppLaunchContext -> icon -> IO ()
appLaunchContextSetIcon self icon =
  {# call gdk_app_launch_context_set_icon #}
    self
    (toIcon icon)
#endif

-- | Sets the icon for applications that are launched with this context. The @iconName@ will be
-- interpreted in the same way as the Icon field in desktop files. See also
-- 'appLaunchContextSetIcon'.
--
-- If both icon and @iconName@ are set, the @iconName@ takes priority. If neither icon or @iconName@ is
-- set, the icon is taken from either the file that is passed to launched application or from the
-- GAppInfo for the launched application itself.
appLaunchContextSetIconName :: GlibString string => AppLaunchContext -> string -> IO ()
appLaunchContextSetIconName self iconName =
  withUTFString iconName $ \iconNamePtr ->
  {# call gdk_app_launch_context_set_icon_name #}
    self
    iconNamePtr

-- | Sets the screen on which applications will be launched when using this context. See also
-- 'appLaunchContextSetDisplay'.
--
-- If both screen and display are set, the screen takes priority. If neither screen or display are set,
-- the default screen and display are used.
appLaunchContextSetScreen :: AppLaunchContext -> Screen -> IO ()
appLaunchContextSetScreen self screen =
  {# call gdk_app_launch_context_set_screen #}
    self
    screen

-- | Sets the timestamp of context. The timestamp should ideally be taken from the event that triggered
-- the launch.
--
-- Window managers can use this information to avoid moving the focus to the newly launched application
-- when the user is busy typing in another window. This is also known as 'focus stealing prevention'.
appLaunchContextSetTimestamp :: AppLaunchContext -> TimeStamp -> IO ()
appLaunchContextSetTimestamp self timestamp =
  {# call gdk_app_launch_context_set_timestamp #}
    self
    (fromIntegral timestamp)
#endif
