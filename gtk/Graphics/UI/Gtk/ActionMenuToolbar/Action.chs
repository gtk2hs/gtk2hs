{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Action
--
--  Author : Duncan Coutts, Andy Stewart
--
--  Created: 6 April 2005
--
--  Copyright (C) 2005 Duncan Coutts
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
-- An action which can be triggered by a menu or toolbar item
--
-- * Module available since Gtk+ version 2.4
--
module Graphics.UI.Gtk.ActionMenuToolbar.Action (
-- * Detail
--
-- | Actions represent operations that the user can be perform, along with
-- some information how it should be presented in the interface. Each action
-- provides methods to create icons, menu items and toolbar items representing
-- itself.
--
-- As well as the callback that is called when the action gets activated,
-- the following also gets associated with the action:
--
-- * a name (not translated, for path lookup)
--
-- * a label (translated, for display)
--
-- * an accelerator
--
-- * whether label indicates a stock id
--
-- * a tooltip (optional, translated)
--
-- * a toolbar label (optional, shorter than label)
--
-- The action will also have some state information:
--
-- * visible (shown\/hidden)
--
-- * sensitive (enabled\/disabled)
--
-- Apart from regular actions, there are toggle actions, which can be
-- toggled between two states and radio actions, of which only one in a group
-- can be in the \"active\" state. Other actions can be implemented as 'Action'
-- subclasses.
--
-- Each action can have one or more proxy menu item, toolbar button or other
-- proxy widgets. Proxies mirror the state of the action (text label, tooltip,
-- icon, visible, sensitive, etc), and should change when the action's state
-- changes. When the proxy is activated, it should activate its action.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----Action
-- |         +----'ToggleAction'
-- @

#if GTK_CHECK_VERSION(2,4,0)
-- * Types
  Action,
  ActionClass,
  castToAction, gTypeAction,
  toAction,

-- * Constructors
  actionNew,

-- * Methods
  actionGetName,
  actionIsSensitive,
  actionGetSensitive,
#if GTK_CHECK_VERSION(2,6,0)
  actionSetSensitive,
#endif
  actionIsVisible,
  actionGetVisible,
#if GTK_CHECK_VERSION(2,6,0)
  actionSetVisible,
#endif
  actionActivate,
  actionCreateMenuItem,
  actionCreateToolItem,
#if GTK_MAJOR_VERSION < 3
  actionConnectProxy,
  actionDisconnectProxy,
#endif
  actionGetProxies,
  actionConnectAccelerator,
  actionDisconnectAccelerator,
#if GTK_CHECK_VERSION(2,6,0)
  actionGetAccelPath,
#endif
  actionSetAccelPath,
  actionSetAccelGroup,

-- * Attributes
  actionName,
  actionLabel,
  actionShortLabel,
  actionTooltip,
  actionStockId,
  actionVisibleHorizontal,
#if GTK_CHECK_VERSION(2,6,0)
  actionVisibleOverflown,
#endif
  actionVisibleVertical,
  actionIsImportant,
  actionHideIfEmpty,
#if GTK_CHECK_VERSION(2,6,0)
  actionSensitive,
  actionVisible,
  actionAccelPath,
#endif
#if GTK_CHECK_VERSION(2,20,0)
  actionAlwaysShowImage,
#endif

-- * Signals
  actionActivated,

-- * Deprecated
#ifndef DISABLE_DEPRECATED
  onActionActivate,
  afterActionActivate,
#endif
#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GList
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.General.StockItems

{# context lib="gtk" prefix="gtk" #}

#if GTK_CHECK_VERSION(2,4,0)
--------------------
-- Constructors

-- | Creates a new 'Action' object. To add the action to a 'ActionGroup' and
-- set the accelerator for the action, call
-- 'Graphics.UI.Gtk.ActionMenuToolbar.ActionGroup.actionGroupAddActionWithAccel'.
-- See "Graphics.UI.Gtk.ActionMenuToolbar.UIManager#XML-UI" for information on
-- allowed action names.
--
actionNew :: GlibString string
 => string        -- ^ @name@ - A unique name for the action
 -> string        -- ^ @label@ - the label displayed in menu items and on
                  -- buttons
 -> Maybe string  -- ^ @tooltip@ - a tooltip for the action
 -> Maybe StockId -- ^ @stockId@ - the stock icon to display in widgets
                  -- representing the action
 -> IO Action
actionNew name label tooltip stockId =
  wrapNewGObject mkAction $
  maybeWith withUTFString stockId $ \stockIdPtr ->
  maybeWith withUTFString tooltip $ \tooltipPtr ->
  withUTFString label $ \labelPtr ->
  withUTFString name $ \namePtr ->
  {# call gtk_action_new #}
    namePtr
    labelPtr
    tooltipPtr
    stockIdPtr

--------------------
-- Methods

-- | Returns the name of the action.
--
actionGetName :: (ActionClass self, GlibString string) => self -> IO string
actionGetName self =
  {# call gtk_action_get_name #}
    (toAction self)
  >>= peekUTFString

-- | Returns whether the action is effectively sensitive.
--
actionIsSensitive :: ActionClass self => self
 -> IO Bool -- ^ returns @True@ if the action and its associated action group
            -- are both sensitive.
actionIsSensitive self =
  liftM toBool $
  {# call gtk_action_is_sensitive #}
    (toAction self)

-- | Returns whether the action itself is sensitive. Note that this doesn't
-- necessarily mean effective sensitivity. See 'actionIsSensitive' for that.
--
actionGetSensitive :: ActionClass self => self
 -> IO Bool -- ^ returns @True@ if the action itself is sensitive.
actionGetSensitive self =
  liftM toBool $
  {# call gtk_action_get_sensitive #}
    (toAction self)

#if GTK_CHECK_VERSION(2,6,0)
-- | Sets the sensitive property of the action to @sensitive@. Note that
-- this doesn't necessarily mean effective sensitivity. See 'actionIsSensitive'
-- for that.
--
-- * Available since Gtk+ version 2.6
--
actionSetSensitive :: ActionClass self => self
 -> Bool  -- ^ @sensitive@ - @True@ to make the action sensitive
 -> IO ()
actionSetSensitive self sensitive =
  {# call gtk_action_set_sensitive #}
    (toAction self)
    (fromBool sensitive)
#endif

-- | Returns whether the action is effectively visible.
--
actionIsVisible :: ActionClass self => self
 -> IO Bool -- ^ returns @True@ if the action and its associated action group
            -- are both visible.
actionIsVisible self =
  liftM toBool $
  {# call gtk_action_is_visible #}
    (toAction self)

-- | Returns whether the action itself is visible. Note that this doesn't
-- necessarily mean effective visibility. See 'actionIsSensitive' for that.
--
actionGetVisible :: ActionClass self => self
 -> IO Bool -- ^ returns @True@ if the action itself is visible.
actionGetVisible self =
  liftM toBool $
  {# call gtk_action_get_visible #}
    (toAction self)

#if GTK_CHECK_VERSION(2,6,0)
-- | Sets the visible property of the action to @visible@. Note that this
-- doesn't necessarily mean effective visibility. See 'actionIsVisible' for
-- that.
--
-- * Available since Gtk+ version 2.6
--
actionSetVisible :: ActionClass self => self
 -> Bool  -- ^ @visible@ - @True@ to make the action visible
 -> IO ()
actionSetVisible self visible =
  {# call gtk_action_set_visible #}
    (toAction self)
    (fromBool visible)
#endif

-- | Emits the \"activate\" signal on the specified action, if it isn't
-- insensitive. This gets called by the proxy widgets when they get activated.
--
-- It can also be used to manually activate an action.
--
actionActivate :: ActionClass self => self -> IO ()
actionActivate self =
  {# call gtk_action_activate #}
    (toAction self)

-- | Creates a menu item widget that proxies for the given action.
--
actionCreateMenuItem :: ActionClass self => self
 -> IO Widget -- ^ returns a menu item connected to the action.
actionCreateMenuItem self =
  makeNewObject mkWidget $
  {# call gtk_action_create_menu_item #}
    (toAction self)

-- | Creates a toolbar item widget that proxies for the given action.
--
actionCreateToolItem :: ActionClass self => self
 -> IO Widget -- ^ returns a toolbar item connected to the action.
actionCreateToolItem self =
  makeNewObject mkWidget $
  {# call gtk_action_create_tool_item #}
    (toAction self)

#if GTK_MAJOR_VERSION < 3
-- | Connects a widget to an action object as a proxy. Synchronises various
-- properties of the action with the widget (such as label text, icon, tooltip,
-- etc), and attaches a callback so that the action gets activated when the
-- proxy widget does.
--
-- If the widget is already connected to an action, it is disconnected
-- first.
--
-- Removed in Gtk3.
actionConnectProxy :: (ActionClass self, WidgetClass proxy) => self
 -> proxy -- ^ @proxy@ - the proxy widget
 -> IO ()
actionConnectProxy self proxy =
  {# call gtk_action_connect_proxy #}
    (toAction self)
    (toWidget proxy)

-- | Disconnects a proxy widget from an action.
--
-- Removed in Gtk3.
actionDisconnectProxy :: (ActionClass self, WidgetClass proxy) => self
 -> proxy -- ^ @proxy@ - the proxy widget
 -> IO ()
actionDisconnectProxy self proxy =
  {# call gtk_action_disconnect_proxy #}
    (toAction self)
    (toWidget proxy)
#endif

-- | Returns the proxy widgets for an action.
--
actionGetProxies :: ActionClass self => self -> IO [Widget]
actionGetProxies self =
  {# call gtk_action_get_proxies #}
    (toAction self)
  >>= readGSList
  >>= mapM (\elemPtr -> makeNewObject mkWidget (return elemPtr))

-- | Installs the accelerator for @action@ if @action@ has an accel path and
-- group. See 'actionSetAccelPath' and 'actionSetAccelGroup'
--
-- Since multiple proxies may independently trigger the installation of the
-- accelerator, the @action@ counts the number of times this function has been
-- called and doesn't remove the accelerator until
-- 'actionDisconnectAccelerator' has been called as many times.
--
actionConnectAccelerator :: ActionClass self => self -> IO ()
actionConnectAccelerator self =
  {# call gtk_action_connect_accelerator #}
    (toAction self)

-- | Undoes the effect of one call to 'actionConnectAccelerator'.
--
actionDisconnectAccelerator :: ActionClass self => self -> IO ()
actionDisconnectAccelerator self =
  {# call gtk_action_disconnect_accelerator #}
    (toAction self)

#if GTK_CHECK_VERSION(2,6,0)
-- | Returns the accel path for this action.
--
-- * Available since Gtk+ version 2.6
--
actionGetAccelPath :: (ActionClass self, GlibString string) => self
 -> IO (Maybe string) -- ^ returns the accel path for this action, or
                      -- @Nothing@ if none is set.
actionGetAccelPath self =
  {# call gtk_action_get_accel_path #}
    (toAction self)
  >>= maybePeek peekUTFString
#endif

-- | Sets the accel path for this action. All proxy widgets associated with
-- the action will have this accel path, so that their accelerators are
-- consistent.
--
actionSetAccelPath :: (ActionClass self, GlibString string) => self
 -> string -- ^ @accelPath@ - the accelerator path
 -> IO ()
actionSetAccelPath self accelPath =
  withUTFString accelPath $ \accelPathPtr ->
  {# call gtk_action_set_accel_path #}
    (toAction self)
    accelPathPtr

-- | Sets the 'AccelGroup' in which the accelerator for this action will be
-- installed.
--
actionSetAccelGroup :: ActionClass self => self -> AccelGroup -> IO ()
actionSetAccelGroup self accelGroup =
  {# call gtk_action_set_accel_group #}
    (toAction self)
    accelGroup

--------------------
-- Attributes

-- | A unique name for the action.
--
-- Default value: \"\"
--
actionName :: GlibString string => ActionClass self => Attr self string
actionName = newAttrFromStringProperty "name"

-- | The label used for menu items and buttons that activate this action.
--
-- Default value: \"\"
--
actionLabel :: GlibString string => ActionClass self => Attr self string
actionLabel = newAttrFromStringProperty "label"

-- | A shorter label that may be used on toolbar buttons.
--
-- Default value: \"\"
--
actionShortLabel :: GlibString string => ActionClass self => Attr self string
actionShortLabel = newAttrFromStringProperty "short-label"

-- | A tooltip for this action.
--
-- Default value: @Nothing@
--
actionTooltip :: GlibString string => ActionClass self => Attr self (Maybe string)
actionTooltip = newAttrFromMaybeStringProperty "tooltip"

-- | The stock icon displayed in widgets representing this action.
--
-- Default value: @Nothing@
--
actionStockId :: GlibString string => ActionClass self => Attr self (Maybe string)
actionStockId = newAttrFromMaybeStringProperty "stock_id"

-- | Whether the toolbar item is visible when the toolbar is in a horizontal
-- orientation.
--
-- Default value: @True@
--
actionVisibleHorizontal :: ActionClass self => Attr self Bool
actionVisibleHorizontal = newAttrFromBoolProperty "visible-horizontal"

#if GTK_CHECK_VERSION(2,6,0)
-- | When @True@, toolitem proxies for this action are represented in the
-- toolbar overflow menu.
--
-- Default value: @True@
--
-- * Available since Gtk+ version 2.6
--
actionVisibleOverflown :: ActionClass self => Attr self Bool
actionVisibleOverflown = newAttrFromBoolProperty "visible-overflown"
#endif

-- | Whether the toolbar item is visible when the toolbar is in a vertical
-- orientation.
--
-- Default value: @True@
--
actionVisibleVertical :: ActionClass self => Attr self Bool
actionVisibleVertical = newAttrFromBoolProperty "visible-vertical"

-- | Whether the action is considered important. When @True@, toolitem proxies
-- for this action show text in
-- 'Graphics.UI.Gtk.MenuComboToolbar.Toolbar.ToolbarBothHoriz' mode.
--
-- Default value: @False@
--
actionIsImportant :: ActionClass self => Attr self Bool
actionIsImportant = newAttrFromBoolProperty "is-important"

-- | When @True@, empty menu proxies for this action are hidden.
--
-- Default value: @True@
--
actionHideIfEmpty :: ActionClass self => Attr self Bool
actionHideIfEmpty = newAttrFromBoolProperty "hide-if-empty"

#if GTK_CHECK_VERSION(2,6,0)
-- | Whether the action is enabled.
--
-- Default value: @True@
--
-- * Available since Gtk+ version 2.6
--
actionSensitive :: ActionClass self => Attr self Bool
actionSensitive = newAttr
  actionGetSensitive
  actionSetSensitive

-- | Whether the action is visible.
--
-- Default value: @True@
--
-- * Available since Gtk+ version 2.6
--
actionVisible :: ActionClass self => Attr self Bool
actionVisible = newAttr
  actionGetVisible
  actionSetVisible

-- | \'accelPath\' property. See 'actionGetAccelPath' and 'actionSetAccelPath'
--
-- * Available since Gtk+ version 2.6
--
actionAccelPath :: GlibString string => ActionClass self => ReadWriteAttr self (Maybe string) string
actionAccelPath = newAttr
  actionGetAccelPath
  actionSetAccelPath
#endif

#if GTK_CHECK_VERSION(2,20,0)
-- | If 'True', the action's menu item proxies will ignore the 'menuImages' setting and always show
-- their image, if available.
--
-- Use this property if the menu item would be useless or hard to use without their image.
--
-- Default value: 'False'
--
-- Since 2.20
actionAlwaysShowImage :: ActionClass self => Attr self Bool
actionAlwaysShowImage = newAttrFromBoolProperty "always-show-image"
#endif

--------------------
-- Signals

-- %hash c:4608 d:49a3
-- | The \"activate\" signal is emitted when the action is activated.
--
actionActivated :: ActionClass self => Signal self (IO ())
actionActivated = Signal (connect_NONE__NONE "activate")

--------------------
-- Deprecated Signals

#ifndef DISABLE_DEPRECATED
-- | The \"activate\" signal is emitted when the action is activated.
--
onActionActivate, afterActionActivate :: ActionClass self => self
 -> IO ()
 -> IO (ConnectId self)
onActionActivate = connect_NONE__NONE "activate" False
afterActionActivate = connect_NONE__NONE "activate" True
#endif
#endif
