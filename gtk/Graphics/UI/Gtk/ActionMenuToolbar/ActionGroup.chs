{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget ActionGroup
--
--  Author : Duncan Coutts
--
--  Created: 6 April 2005
--
--  Copyright (C) 2005 Duncan Coutts
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
-- A group of actions
--
-- * Module available since Gtk+ version 2.4
--
module Graphics.UI.Gtk.ActionMenuToolbar.ActionGroup (
-- * Detail
--
-- | Actions are organised into groups. An action group is essentially a map
-- from names to 'Action' objects.
--
-- All actions that would make sense to use in a particular context should
-- be in a single group. Multiple action groups may be used for a particular
-- user interface. In fact, it is expected that most nontrivial applications
-- will make use of multiple groups. For example, in an application that can
-- edit multiple documents, one group holding global actions (e.g. quit, about,
-- new), and one group per document holding actions that act on that document
-- (eg. save, cut\/copy\/paste, etc). Each window's menus would be constructed
-- from a combination of two action groups.
--
-- Accelerators are handled by the Gtk+ accelerator map. All actions are
-- assigned an accelerator path (which normally has the form
-- @\<Actions>\/group-name\/action-name@) and a shortcut is associated with
-- this accelerator path. All menuitems and toolitems take on this accelerator
-- path. The Gtk+ accelerator map code makes sure that the correct shortcut is
-- displayed next to the menu item.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----ActionGroup
-- @

#if GTK_CHECK_VERSION(2,4,0)
-- * Types
  ActionGroup,
  ActionGroupClass,
  castToActionGroup, gTypeActionGroup,
  toActionGroup,
  ActionEntry(..),
  ToggleActionEntry(..),
  RadioActionEntry(..),

-- * Constructors
  actionGroupNew,

-- * Methods
  actionGroupGetName,
  actionGroupGetSensitive,
  actionGroupSetSensitive,
  actionGroupGetVisible,
  actionGroupSetVisible,
  actionGroupGetAction,
  actionGroupListActions,
  actionGroupAddAction,
  actionGroupAddActionWithAccel,
  actionGroupRemoveAction,
  actionGroupAddActions,
  actionGroupAddToggleActions,
  actionGroupAddRadioActions,
  actionGroupSetTranslateFunc,
  actionGroupSetTranslationDomain,
#if GTK_CHECK_VERSION(2,6,0)
  actionGroupTranslateString,
#endif

-- * Attributes
  actionGroupName,
  actionGroupSensitive,
  actionGroupVisible,

-- * Signals
--  onConnectProxy,
--  afterConnectProxy,
--  onDisconnectProxy,
--  afterDisconnectProxy,
--  onPreActivate,
--  afterPreActivate,
--  onPostActivate,
--  afterPostActivate,
#endif
  ) where

import Control.Monad    (liftM, foldM, when)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GList
import System.Glib.Attributes
import System.Glib.Properties
{#import Graphics.UI.Gtk.Types#}
import System.Glib.Signals (on)

import Graphics.UI.Gtk.ActionMenuToolbar.Action
import Graphics.UI.Gtk.ActionMenuToolbar.ToggleAction
import Graphics.UI.Gtk.ActionMenuToolbar.RadioAction

{# context lib="gtk" prefix="gtk" #}

#if GTK_CHECK_VERSION(2,4,0)
--------------------
-- Constructors

-- | Creates a new 'ActionGroup' object. The name of the action group is used
-- when associating keybindings with the actions.
--
actionGroupNew :: GlibString string
 => string         -- ^ @name@ - the name of the action group.
 -> IO ActionGroup
actionGroupNew name =
  wrapNewGObject mkActionGroup $
  withUTFString name $ \namePtr ->
  {# call gtk_action_group_new #}
    namePtr

--------------------
-- Methods

-- | Gets the name of the action group.
--
actionGroupGetName :: GlibString string => ActionGroup
 -> IO string   -- ^ returns the name of the action group.
actionGroupGetName self =
  {# call gtk_action_group_get_name #}
    self
  >>= peekUTFString

-- | Returns @True@ if the group is sensitive. The constituent actions can
-- only be logically sensitive (see 'actionIsSensitive') if they are sensitive
-- (see 'actionGetSensitive') and their group is sensitive.
--
actionGroupGetSensitive :: ActionGroup -> IO Bool
actionGroupGetSensitive self =
  liftM toBool $
  {# call gtk_action_group_get_sensitive #}
    self

-- | Changes the sensitivity of @actionGroup@
--
actionGroupSetSensitive :: ActionGroup -> Bool -> IO ()
actionGroupSetSensitive self sensitive =
  {# call gtk_action_group_set_sensitive #}
    self
    (fromBool sensitive)

-- | Returns @True@ if the group is visible. The constituent actions can only
-- be logically visible (see 'actionIsVisible') if they are visible (see
-- 'actionGetVisible') and their group is visible.
--
actionGroupGetVisible :: ActionGroup -> IO Bool
actionGroupGetVisible self =
  liftM toBool $
  {# call gtk_action_group_get_visible #}
    self

-- | Changes the visible of @actionGroup@.
--
actionGroupSetVisible :: ActionGroup -> Bool -> IO ()
actionGroupSetVisible self visible =
  {# call gtk_action_group_set_visible #}
    self
    (fromBool visible)

-- | Looks up an action in the action group by name.
--
actionGroupGetAction :: GlibString string => ActionGroup
 -> string            -- ^ @actionName@ - the name of the action
 -> IO (Maybe Action) -- ^ returns the action, or @Nothing@ if no action by
                      -- that name exists
actionGroupGetAction self actionName =
  maybeNull (makeNewGObject mkAction) $
  withUTFString actionName $ \actionNamePtr ->
  {# call gtk_action_group_get_action #}
    self
    actionNamePtr

-- | Lists the actions in the action group.
--
actionGroupListActions :: ActionGroup
 -> IO [Action] -- ^ returns a list of the action objects in the action group
actionGroupListActions self =
  {# call gtk_action_group_list_actions #}
    self
  >>= fromGList
  >>= mapM (\elemPtr -> makeNewGObject mkAction (return elemPtr))

-- | Adds an action object to the action group. Note that this function does
-- not set up the accel path of the action, which can lead to problems if a
-- user tries to modify the accelerator of a menuitem associated with the
-- action. Therefore you must either set the accel path yourself with
-- 'actionSetAccelPath', or use @'actionGroupAddActionWithAccel' ... Nothing@.
--
actionGroupAddAction :: ActionClass action => ActionGroup -> action -> IO ()
actionGroupAddAction self action =
  {# call gtk_action_group_add_action #}
    self
    (toAction action)

-- | Adds an action object to the action group and sets up the accelerator.
--
-- If @accelerator@ is @Nothing@, attempts to use the accelerator associated
-- with the stock id of the action.
--
-- Accel paths are set to @\<Actions>\/group-name\/action-name@.
--
actionGroupAddActionWithAccel :: (ActionClass action, GlibString string) => ActionGroup
 -> action       -- ^ @action@ - the action to add
 -> Maybe string -- ^ @accelerator@ - the accelerator for the action, in the
                 -- format understood by 'acceleratorParse', or \"\" for no
                 -- accelerator, or @Nothing@ to use the stock accelerator
 -> IO ()
actionGroupAddActionWithAccel self action accelerator =
  maybeWith withUTFString accelerator $ \acceleratorPtr ->
  {# call gtk_action_group_add_action_with_accel #}
    self
    (toAction action)
    acceleratorPtr

-- | Removes an action object from the action group.
--
actionGroupRemoveAction :: ActionClass action => ActionGroup -> action -> IO ()
actionGroupRemoveAction self action =
  {# call gtk_action_group_remove_action #}
    self
    (toAction action)

-- Note: for these next few funtions we cannot use the C version because the
-- callback doesn't allow for propper memory management. So like pygtk we
-- implement them natively since they are only convenience functions afterall.


-- | A description of an action.
data ActionEntry = ActionEntry {
       actionEntryName        :: DefaultGlibString,
       actionEntryLabel       :: DefaultGlibString,
       actionEntryStockId     :: Maybe DefaultGlibString,
       actionEntryAccelerator :: Maybe DefaultGlibString,
       actionEntryTooltip     :: Maybe DefaultGlibString,
       actionEntryCallback    :: IO ()
     }

-- | This is a convenience function to create a number of actions and add them
-- to the action group.
--
-- The 'actionActivated' signals of the actions are connected to the callbacks
-- and their accel paths are set to @\<Actions>\/group-name\/action-name@.
--
actionGroupAddActions :: ActionGroup
 -> [ActionEntry] -- ^ @entries@ - a list of action descriptions
 -> IO ()
actionGroupAddActions self entries =
  flip mapM_ entries $ \(ActionEntry name label stockId
                        accelerator tooltip callback) -> do
    action <- actionNew name label tooltip stockId
    action `on` actionActivated $ callback
    actionGroupAddActionWithAccel self action accelerator

-- | A description of an action for an entry that can be toggled.
data ToggleActionEntry = ToggleActionEntry {
       toggleActionName        :: DefaultGlibString,
       toggleActionLabel       :: DefaultGlibString,
       toggleActionStockId     :: Maybe DefaultGlibString,
       toggleActionAccelerator :: Maybe DefaultGlibString,
       toggleActionTooltip     :: Maybe DefaultGlibString,
       toggleActionCallback    :: IO (),
       toggleActionIsActive    :: Bool
     }

-- | This is a convenience function to create a number of toggle actions and
-- add them to the action group.
--
-- The 'actionActivated' signals of the actions are connected to the callbacks
-- and their accel paths are set to @\<Actions>\/group-name\/action-name@.
--
actionGroupAddToggleActions :: ActionGroup
 -> [ToggleActionEntry] -- ^ @entries@ - a list of toggle action descriptions
 -> IO ()
actionGroupAddToggleActions self entries =
  flip mapM_ entries $ \(ToggleActionEntry name label stockId
                        accelerator tooltip callback isActive) -> do
    action <- toggleActionNew name label tooltip stockId
    toggleActionSetActive action isActive
    action `on` actionActivated $ callback
    actionGroupAddActionWithAccel self action accelerator

-- | A description of an action for an entry that provides a multiple choice.
data RadioActionEntry = RadioActionEntry {
       radioActionName        :: DefaultGlibString,
       radioActionLabel       :: DefaultGlibString,
       radioActionStockId     :: Maybe DefaultGlibString,
       radioActionAccelerator :: Maybe DefaultGlibString,
       radioActionTooltip     :: Maybe DefaultGlibString,
       radioActionValue       :: Int
     }

-- | This is a convenience routine to create a group of radio actions and add
-- them to the action group.
--
-- The 'radioActionChanged' signal of the first radio action is connected to the
-- @onChange@ callback and the accel paths of the actions are set to
-- @\<Actions>\/group-name\/action-name@.
--
actionGroupAddRadioActions :: ActionGroup
 -> [RadioActionEntry] -- ^ @entries@ - a list of radio action descriptions
 -> Int                -- ^ @value@ - the value of the action to activate
                       -- initially, or -1 if no action should be activated
 -> (RadioAction -> IO ()) -- ^ @onChange@ - the callback for the changed signal
 -> IO ()
actionGroupAddRadioActions self entries initialValue onChange = do
  group <- foldM
    (\group (RadioActionEntry name label stockId
               accelerator tooltip value) -> do
    action <- radioActionNew name label tooltip stockId value
    case group of
      Nothing -> return ()
      Just group -> radioActionSetGroup action group
    when (initialValue == value) (toggleActionSetActive action True)
    actionGroupAddActionWithAccel self action accelerator
    return (Just action))
    Nothing entries
  case group of
      Nothing -> return ()
      Just group -> do
        group `on` radioActionChanged $ onChange
        return ()

-- | Sets a function to be used for translating the @label@ and @tooltip@ of
-- 'ActionEntry's added by 'actionGroupAddActions'.
--
-- If you\'re using \'gettext\', it is enough to set the translation domain
-- with 'actionGroupSetTranslationDomain'.
--
actionGroupSetTranslateFunc :: GlibString string => ActionGroup
 -> (string -> IO string) -- ^ @(\label -> ...)@ - a translation function
 -> IO ()
actionGroupSetTranslateFunc self func = do
  funcPtr <- mkTranslateFunc $ \strPtr _ -> do
               str <- peekUTFString strPtr
               translatedStr <- func str
               newUTFString translatedStr
  {# call gtk_action_group_set_translate_func #}
    self
    funcPtr
    (castFunPtrToPtr funcPtr)
    destroyFunPtr

{#pointer TranslateFunc#}

foreign import ccall "wrapper" mkTranslateFunc ::
  (CString -> Ptr () -> IO CString) -> IO TranslateFunc

-- | Sets the translation domain and uses \'dgettext\' for translating the
-- @label@ and @tooltip@ of 'ActionEntry's added by 'actionGroupAddActions'.
--
-- If you\'re not using \'gettext\' for localization, see
-- 'actionGroupSetTranslateFunc'.
--
actionGroupSetTranslationDomain :: GlibString string => ActionGroup
 -> string      -- ^ @domain@ - the translation domain to use for \'dgettext\'
                -- calls
 -> IO ()
actionGroupSetTranslationDomain self domain =
  withUTFString domain $ \domainPtr ->
  {# call gtk_action_group_set_translation_domain #}
    self
    domainPtr

#if GTK_CHECK_VERSION(2,6,0)
-- | Translates a string.
--
-- * Available since Gtk+ version 2.6
--
actionGroupTranslateString :: GlibString string => ActionGroup
 -> string      -- ^ @string@ - a string
 -> IO string   -- ^ returns the translation of @string@
actionGroupTranslateString self string =
  withUTFString string $ \stringPtr ->
  {# call gtk_action_group_translate_string #}
    self
    stringPtr
  >>= peekUTFString
#endif

--------------------
-- Attributes

-- | A name for the action group.
--
-- Default value: \"\"
--
actionGroupName :: GlibString string => Attr ActionGroup string
actionGroupName = newAttrFromStringProperty "name"

-- | Whether the action group is enabled.
--
-- Default value: @True@
--
actionGroupSensitive :: Attr ActionGroup Bool
actionGroupSensitive = newAttr
  actionGroupGetSensitive
  actionGroupSetSensitive

-- | Whether the action group is visible.
--
-- Default value: @True@
--
actionGroupVisible :: Attr ActionGroup Bool
actionGroupVisible = newAttr
  actionGroupGetVisible
  actionGroupSetVisible

--------------------
-- Signals

{-
-- | The connect_proxy signal is emitted after connecting a proxy to an action
-- in the group. Note that the proxy may have been connected to a different
-- action before.
--
-- This is intended for simple customizations for which a custom action
-- class would be too clumsy, e.g. showing tooltips for menuitems in the
-- statusbar.
--
onConnectProxy, afterConnectProxy :: ActionGroupClass self => self
 -> ({-GtkAction-} -> {-GtkWidget-} -> IO ())
 -> IO (ConnectId self)
onConnectProxy = connect_{-GtkAction-}_{-GtkWidget-}__NONE "connect_proxy" False
afterConnectProxy = connect_{-GtkAction-}_{-GtkWidget-}__NONE "connect_proxy" True

-- | The disconnect_proxy signal is emitted after disconnecting a proxy from
-- an action in the group.
--
onDisconnectProxy, afterDisconnectProxy :: ActionGroupClass self => self
 -> ({-GtkAction-} -> {-GtkWidget-} -> IO ())
 -> IO (ConnectId self)
onDisconnectProxy = connect_{-GtkAction-}_{-GtkWidget-}__NONE "disconnect_proxy" False
afterDisconnectProxy = connect_{-GtkAction-}_{-GtkWidget-}__NONE "disconnect_proxy" True

-- | The pre_activate signal is emitted just before the @action@ in the
-- @actionGroup@ is activated
--
-- This is intended for 'UIManager' to proxy the signal and provide global
-- notification just before any action is activated.
--
onPreActivate, afterPreActivate :: ActionGroupClass self => self
 -> ({-GtkAction-} -> IO ())
 -> IO (ConnectId self)
onPreActivate = connect_{-GtkAction-}__NONE "pre_activate" False
afterPreActivate = connect_{-GtkAction-}__NONE "pre_activate" True

-- | The post_activate signal is emitted just after the @action@ in the
-- @actionGroup@ is activated
--
-- This is intended for 'UIManager' to proxy the signal and provide global
-- notification just after any action is activated.
--
onPostActivate, afterPostActivate :: ActionGroupClass self => self
 -> ({-GtkAction-} -> IO ())
 -> IO (ConnectId self)
onPostActivate = connect_{-GtkAction-}__NONE "post_activate" False
afterPostActivate = connect_{-GtkAction-}__NONE "post_activate" True
-}
#endif
