{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget RecentAction
--
--  Author : Andy Stewart
--
--  Created: 24 Mar 2010
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
-- An action of which represents a list of recently used files
--
-- * Module available since Gtk+ version 2.12
--
module Graphics.UI.Gtk.ActionMenuToolbar.RecentAction (

-- * Detail
--
-- | A 'RecentAction' represents a list of recently used files, which can be
-- shown by widgets such as 'RecentChooserDialog' or 'RecentChooserMenu'.
--
-- To construct a submenu showing recently used files, use a 'RecentAction'
-- as the action for a \<menuitem>. To construct a menu toolbutton showing the
-- recently used files in the popup menu, use a 'RecentAction' as the action
-- for a \<toolitem> element.

-- * Class Hierarchy
--
-- |
-- @
-- |  'GObject'
-- |   +----'Action'
-- |         +----RecentAction
-- @

#if GTK_CHECK_VERSION(2,12,0)
-- * Types
  RecentAction,
  RecentActionClass,
  castToRecentAction,
  toRecentAction,

-- * Constructors
  recentActionNew,
  recentActionNewForManager,

-- * Attributes
  recentActionShowNumbers,
#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.Attributes
import System.Glib.Properties
import System.Glib.UTFString
{#import Graphics.UI.Gtk.Types#}

{# context lib="gtk" prefix="gtk" #}

#if GTK_CHECK_VERSION(2,12,0)
--------------------
-- Constructors

-- | Creates a new 'RecentAction' object. To add the action to a 'ActionGroup'
-- and set the accelerator for the action, call
-- 'actionGroupAddActionWithAccel'.
--
recentActionNew :: GlibString string
 => string -- ^ @name@ - a unique name for the action
 -> Maybe string -- ^ @label@ - the label displayed in menu items and on buttons, or 'Nothing'
 -> Maybe string -- ^ @tooltip@ - a tooltip for the action, or 'Nothing'
 -> Maybe string -- ^ @stockId@ - the stock icon to display in widgets representing
           -- the action, or 'Nothing'
 -> IO RecentAction
recentActionNew name label tooltip stockId =
  wrapNewGObject mkRecentAction $
  liftM castPtr $
  withUTFString name $ \namePtr ->
  maybeWith withUTFString label $ \labelPtr ->
  maybeWith withUTFString tooltip $ \tooltipPtr ->
  maybeWith withUTFString stockId $ \stockIdPtr ->
  {# call gtk_recent_action_new #}
    namePtr
    labelPtr
    tooltipPtr
    stockIdPtr

-- | Creates a new 'RecentAction' object. To add the action to a 'ActionGroup'
-- and set the accelerator for the action, call
-- 'actionGroupAddActionWithAccel'.
--
recentActionNewForManager :: (RecentManagerClass manager, GlibString string) =>
    string  -- ^ @name@ - a unique name for the action
 -> Maybe string  -- ^ @label@ - the label displayed in menu items and on buttons,
            -- or 'Nothing'
 -> Maybe string  -- ^ @tooltip@ - a tooltip for the action, or 'Nothing'
 -> Maybe string  -- ^ @stockId@ - the stock icon to display in widgets representing
            -- the action, or 'Nothing'
 -> Maybe manager -- ^ @manager@ - a 'RecentManager', or 'Nothing' for the
            -- default 'RecentManager'
 -> IO RecentAction
recentActionNewForManager name label tooltip stockId manager =
  wrapNewGObject mkRecentAction $ liftM castPtr $
  withUTFString name $ \namePtr ->
  maybeWith withUTFString label $ \labelPtr ->
  maybeWith withUTFString tooltip $ \tooltipPtr ->
  maybeWith withUTFString stockId $ \stockIdPtr -> do
    {# call gtk_recent_action_new_for_manager #}
      namePtr
      labelPtr
      tooltipPtr
      stockIdPtr
      (maybe (RecentManager nullForeignPtr) toRecentManager manager)

--------------------
-- Attributes

-- | If recent items should be shown with numbers next to them.
--
recentActionShowNumbers :: RecentActionClass self => Attr self Bool
recentActionShowNumbers = newAttrFromBoolProperty "show-numbers"
#endif
