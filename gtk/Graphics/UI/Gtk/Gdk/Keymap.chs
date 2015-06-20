{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Keymap
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
-- Functions for manipulating keyboard codes
--
module Graphics.UI.Gtk.Gdk.Keymap (

-- * Details
--
-- | Key values are the codes which are sent whenever a key is pressed or released. They appear in the
-- keyval field of the 'EventKey' structure, which is passed to signal handlers for the
-- 'keyPressEvent' and 'keyReleaseEvent' signals.
--
-- Key values are regularly updated from the upstream X.org X11 implementation, so new values are added
-- regularly. They will be prefixed with GDK_ rather than XF86XK_ or ' (for older symbols)'.
--
-- Key values can be converted into a string representation using 'keyvalName'. The reverse
-- function, converting a string to a key value, is provided by 'keyvalFromName'.
--
-- The case of key values can be determined using 'keyvalIsUpper'. Key
-- values can be converted to upper or lower case using 'keyvalToUpper' and
-- 'keyvalToLower'.
--
-- When it makes sense, key values can be converted to and from Unicode characters with
-- 'keyvalToUnicode'.
--
-- One 'Keymap' object exists for each user display. 'keymapGetDefault' returns the 'Keymap'
-- for the default display; to obtain keymaps for other displays, use 'keymapGetForDisplay'. A
-- keymap is a mapping from 'KeymapKey' to key values. You can think of a 'KeymapKey' as a
-- representation of a symbol printed on a physical keyboard key. That is, it contains three pieces of
-- information. First, it contains the hardware keycode; this is an identifying number for a physical
-- key. Second, it contains the level of the key. The level indicates which symbol on the key will be
-- used, in a vertical direction. So on a standard US keyboard, the key with the number \"1\" on it also
-- has the exclamation point \"!\" character on it. The level indicates whether to use the \"1\" or the
-- \"!\" symbol. The letter keys are considered to have a lowercase letter at level 0, and an uppercase
-- letter at level 1, though only the uppercase letter is printed. Third, the 'KeymapKey' contains a
-- group; groups are not used on standard US keyboards, but are used in many other countries. On a
-- keyboard with groups, there can be 3 or 4 symbols printed on a single key. The group indicates
-- movement in a horizontal direction. Usually groups are used for two different languages. In group 0,
-- a key might have two English characters, and in group 1 it might have two Hebrew characters. The
-- Hebrew characters will be printed on the key next to the English characters.
--
-- In order to use a keymap to interpret a key event, it's necessary to first convert the keyboard
-- state into an effective group and level. This is done via a set of rules that varies widely
-- according to type of keyboard and user configuration.  The function
-- 'keymapTranslateKeyboardState' accepts a keyboard state -- consisting of hardware keycode
-- pressed, active modifiers, and active group -- applies the appropriate rules, and returns the
-- group/level to be used to index the keymap, along with the modifiers which did not affect the group
-- and level. i.e. it returns "unconsumed modifiers." The keyboard group may differ from the effective
-- group used for keymap lookups because some keys don't have multiple groups - e.g. the Enter key is
-- always in group 0 regardless of keyboard state.
--
-- Note that 'keymapTranslateKeyboardState' also returns the keyval, i.e. it goes ahead and
-- performs the keymap lookup in addition to telling you which effective group/level values were used
-- for the lookup. 'EventKey' already contains this keyval, however, so you don't normally need to
-- call 'keymapTranslateKeyboardState' just to get the keyval.

-- * Class Hierarchy
--
-- |
-- @
-- |  'GObject'
-- |   +----Keymap
-- @

-- * Types
  Keymap,
  KeymapClass,
  castToKeymap,
  toKeymap,
  KeymapKey,

-- * Methods
  keymapGetDefault,
#if GTK_CHECK_VERSION(2,2,0)
  keymapGetForDisplay,
#endif
  keymapLookupKey,
  keymapTranslateKeyboardState,
  keymapGetEntriesForKeyval,
  keymapGetEntriesForKeycode,
  keymapGetDirection,
#if GTK_CHECK_VERSION(2,12,0)
  keymapHaveBidiLayouts,
#endif
#if GTK_CHECK_VERSION(2,16,0)
  keymapGetCapsLockState,
#endif

-- * Signals
#if GTK_CHECK_VERSION(2,0,0)
  keymapDirectionChanged,
#if GTK_CHECK_VERSION(2,2,0)
  keymapKeysChanged,
#if GTK_CHECK_VERSION(2,16,0)
  keymapStateChanged,
#endif
#endif
#endif
  ) where

import Control.Monad    (liftM)
import System.Glib.FFI
import Graphics.UI.Gtk.Gdk.Enums        (Modifier(..))
import Graphics.UI.Gtk.Gdk.Keys (KeyVal (..))
{#import Graphics.Rendering.Pango.Enums#}
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.General.Structs (KeymapKey (..))

{# context lib="gdk" prefix="gdk" #}

--------------------
-- Methods

-- | Returns the 'Keymap' attached to the default display.
--
keymapGetDefault ::
    IO Keymap -- ^ returns the 'Keymap' attached to the default display.
keymapGetDefault =
  makeNewGObject mkKeymap $
  {# call gdk_keymap_get_default #}

#if GTK_CHECK_VERSION(2,2,0)
-- | Returns the 'Keymap' attached to @display@.
--
-- * Available since Gdk version 2.2
--
keymapGetForDisplay ::
    Display   -- ^ @display@ - the 'Display'.
 -> IO Keymap -- ^ returns the 'Keymap' attached to @display@.
keymapGetForDisplay display =
  makeNewGObject mkKeymap $
  {# call gdk_keymap_get_for_display #}
    display
#endif

-- | Looks up the keyval mapped to a keycode\/group\/level triplet. If no
-- keyval is bound to @key@, returns 0. For normal user input, you want to use
-- 'keymapTranslateKeyboardState' instead of this function, since the effective
-- group\/level may not be the same as the current keyboard state.
--
keymapLookupKey :: KeymapClass self
                => (Maybe self) -- ^ @keymap@  a 'Keymap' or 'Nothing' to use the default keymap
                -> KeymapKey -- ^ @key@ - a 'KeymapKey'
                            -- with keycode, group, and level initialized
                -> IO Int    -- ^ returns a keyval, or 0 if none was mapped to
                            -- the given @key@
keymapLookupKey Nothing key =
  liftM fromIntegral $
  allocaBytes {# sizeof GdkKeymapKey #} $ \ keyPtr -> do
    poke keyPtr key
    {# call gdk_keymap_lookup_key #}
      (Keymap nullForeignPtr)
      (castPtr keyPtr)
keymapLookupKey (Just self) key =
  liftM fromIntegral $
  allocaBytes {# sizeof GdkKeymapKey #} $ \ keyPtr -> do
    poke keyPtr key
    {# call gdk_keymap_lookup_key #}
      (toKeymap self)
      (castPtr keyPtr)

-- | Translates the contents of a 'EventKey' into a
-- keyval, effective group, and level. Modifiers that affected the translation
-- and are thus unavailable for application use are returned in
-- @consumedModifiers@. See 'keyvalGetKeys' for an explanation of groups and
-- levels. The @effectiveGroup@ is the group that was actually used for the
-- translation; some keys such as Enter are not affected by the active keyboard
-- group. The @level@ is derived from @state@. For convenience, 'EventKey'
-- already contains the translated keyval, so this function
-- isn't as useful as you might think.
--
keymapTranslateKeyboardState :: KeymapClass self => self
 -> Int                          -- ^ @hardwareKeycode@ - a keycode
 -> Modifier           -- ^ @state@ - a modifier state
 -> Int                      -- ^ @group@ - active keyboard group
 -> IO (Maybe (Int, Int, Int, Modifier))
keymapTranslateKeyboardState self hardwareKeycode state group =
  alloca $ \keyvalPtr ->
  alloca $ \effectiveGroupPtr ->
  alloca $ \levelPtr ->
  alloca $ \modifierPtr -> do
    success <- liftM toBool $
              {# call gdk_keymap_translate_keyboard_state #}
                (toKeymap self)
                (fromIntegral hardwareKeycode)
                ((fromIntegral . fromEnum) state)
                (fromIntegral group)
                keyvalPtr
                effectiveGroupPtr
                levelPtr
                modifierPtr
    if success
       then do
         keyval <- peek keyvalPtr
         effectiveGroup <- peek effectiveGroupPtr
         level <- peek levelPtr
         modifier <- peek modifierPtr
         return (Just (fromIntegral keyval
                      ,fromIntegral effectiveGroup
                      ,fromIntegral level
                      ,toEnum $ fromIntegral modifier))
       else return Nothing

-- | Obtains a list of keycode\/group\/level combinations that will generate
-- @keyval@. Groups and levels are two kinds of keyboard mode; in general, the
-- level determines whether the top or bottom symbol on a key is used, and the
-- group determines whether the left or right symbol is used. On US keyboards,
-- the shift key changes the keyboard level, and there are no groups. A group
-- switch key might convert a keyboard between Hebrew to English modes, for
-- example. 'EventKey' contains a @group@ field that
-- indicates the active keyboard group. The level is computed from the modifier
-- mask.
--
keymapGetEntriesForKeyval :: KeymapClass self => self
 -> KeyVal                -- ^ @keyval@ - a keyval, such as @GDK_a@, @GDK_Up@,
                       -- @GDK_Return@, etc.
 -> IO (Maybe [KeymapKey])
keymapGetEntriesForKeyval self keyval =
  alloca $ \nKeysPtr ->
  allocaArray 0 $ \ keysPtr -> do
    success <- liftM toBool $
              {# call gdk_keymap_get_entries_for_keyval #}
                (toKeymap self)
                (fromIntegral keyval)
                (castPtr keysPtr)
                nKeysPtr
    if success
       then do
         nKeys <- liftM fromIntegral $ peek nKeysPtr
         keys <- peekArray nKeys keysPtr
         keyList <- mapM peek keys
         {#call unsafe g_free#} (castPtr keysPtr)
         return (Just keyList)
       else return Nothing

-- | Returns the keyvals bound to @hardwareKeycode@. The Nth 'KeymapKey'
-- in @keys@ is bound to the Nth keyval in @keyvals@.
-- When a keycode is pressed by the user, the
-- keyval from this list of entries is selected by considering the effective
-- keyboard group and level. See 'keymapTranslateKeyboardState'.
--
keymapGetEntriesForKeycode :: KeymapClass self => self
 -> Int                -- ^ @hardwareKeycode@ - a keycode
 -> IO (Maybe ([KeymapKey], [KeyVal]))
keymapGetEntriesForKeycode self hardwareKeycode =
  alloca $ \nEntriesPtr ->
  allocaArray 0 $ \ keysPtr ->
  allocaArray 0 $ \ keyvalsPtr -> do
    success <- liftM toBool $
              {# call gdk_keymap_get_entries_for_keycode #}
                (toKeymap self)
                (fromIntegral hardwareKeycode)
                (castPtr keysPtr)
                keyvalsPtr
                nEntriesPtr
    if success
       then do
         nEntries <- liftM fromIntegral $ peek nEntriesPtr
         keys <- peekArray nEntries keysPtr
         keyvals <- peekArray nEntries keyvalsPtr
         keyvalsList <- mapM (\x -> liftM fromIntegral $ peek x) keyvals
         keysList <- mapM peek keys
         {#call unsafe g_free#} (castPtr keysPtr)
         {#call unsafe g_free#} (castPtr keyvalsPtr)
         return (Just (keysList, keyvalsList))
       else return Nothing

-- | Returns the direction of effective layout of the keymap.
--
-- Returns the direction of the keymap.
--
keymapGetDirection :: KeymapClass self => self
 -> IO PangoDirection -- ^ returns 'DirectionLtr' or 'DirectionRtl' if it can
                 -- determine the direction. 'DirectionNeutral' otherwise.
keymapGetDirection self =
  liftM (toEnum . fromIntegral) $
  {# call gdk_keymap_get_direction #}
    (toKeymap self)

#if GTK_CHECK_VERSION(2,12,0)
-- | Determines if keyboard layouts for both right-to-left and left-to-right
-- languages are in use.
--
-- * Available since Gdk version 2.12
--
keymapHaveBidiLayouts :: KeymapClass self => self
 -> IO Bool -- ^ returns @True@ if there are layouts in both directions,
            -- @False@ otherwise
keymapHaveBidiLayouts self =
  liftM toBool $
  {# call gdk_keymap_have_bidi_layouts #}
    (toKeymap self)
#endif

#if GTK_CHECK_VERSION(2,16,0)
-- | Returns whether the Caps Lock modifer is locked.
--
-- * Available since Gdk version 2.16
--
keymapGetCapsLockState :: KeymapClass self => self
 -> IO Bool -- ^ returns @True@ if Caps Lock is on
keymapGetCapsLockState self =
  liftM toBool $
  {# call gdk_keymap_get_caps_lock_state #}
    (toKeymap self)
#endif

--------------------
-- Signals

#if GTK_CHECK_VERSION(2,0,0)
-- | The 'keymapDirectionChanged' signal gets emitted when the direction of the
-- keymap changes.
--
-- * Available since Gdk version 2.0
--
keymapDirectionChanged :: KeymapClass self => Signal self (IO ())
keymapDirectionChanged = Signal (connect_NONE__NONE "direction_changed")

#if GTK_CHECK_VERSION(2,2,0)
-- | The 'keymapKeysChanged' signal is emitted when the mapping represented by
-- @keymap@ changes.
--
-- * Available since Gdk version 2.2
--
keymapKeysChanged :: KeymapClass self => Signal self (IO ())
keymapKeysChanged = Signal (connect_NONE__NONE "keys_changed")

#if GTK_CHECK_VERSION(2,16,0)
-- | The 'keymapStateChanged' signal is emitted when the state of the keyboard
-- changes, e.g when Caps Lock is turned on or off. See
-- 'keymapGetCapsLockState'.
--
-- * Available since Gdk version 2.16
--
keymapStateChanged :: KeymapClass self => Signal self (IO ())
keymapStateChanged = Signal (connect_NONE__NONE "state_changed")
#endif
#endif
#endif

