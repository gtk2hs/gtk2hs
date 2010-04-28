{-# LANGUAGE CPP #-}
-- -*-haskell-*-
-----------------------------------------------------------------------------
--  Module      :  Graphics.UI.Gtk.WebKit.WebNavigationAction
--  Author      :  Andy Stewart
--  Copyright   :  (c) 2010 Andy Stewart <lazycat.manatee@gmail.com>
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
-- Access to the WebKit NavigationAction
-----------------------------------------------------------------------------

module Graphics.UI.Gtk.WebKit.WebNavigationAction (
-- * Types
  WebNavigationAction,

-- * Enums
  NavigationReason(..),

-- * Methods  
  webNavigationActionGetButton,
  webNavigationActionGetModifierState,
  webNavigationActionGetOriginalUri,
  webNavigationActionSetOriginalUri,
  webNavigationActionGetReason,
  webNavigationActionSetReason,
  webNavigationActionGetTargetFrame,
) where

import Control.Monad		(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GList
import System.Glib.GError 
import Graphics.UI.Gtk.Gdk.Events

{#import Graphics.UI.Gtk.Abstract.Object#}	(makeNewObject)
{#import Graphics.UI.Gtk.WebKit.Signals#}
{#import Graphics.UI.Gtk.WebKit.Types#}
{#import System.Glib.GObject#}

{#context lib="webkit" prefix ="webkit"#}

-- * Enums

{#enum WebNavigationReason as NavigationReason {underscoreToCase}#}

-- * Methods

-- | Returns the DOM identifier for the mouse button used to click. 
-- DOM button values are 0, 1 and 2 for left, middle and right buttons. 
-- If the action was not initiated by a mouse click, returns -1.
webNavigationActionGetButton :: 
   WebNavigationActionClass self => self
 -> IO Int
webNavigationActionGetButton action =  
    liftM fromIntegral $ {#call web_navigation_action_get_button#} (toWebNavigationAction action)

-- | Returns a bitmask with the the state of the modifier keys.
webNavigationActionGetModifierState ::  
   WebNavigationActionClass self => self
 -> IO Int   
webNavigationActionGetModifierState action = 
    liftM fromIntegral $ {#call web_navigation_action_get_modifier_state#} (toWebNavigationAction action)
  
-- | Returns the URI that was originally requested. 
-- This may differ from the navigation target, for instance because of a redirect.
webNavigationActionGetOriginalUri ::   
   WebNavigationActionClass self => self
 -> IO String
webNavigationActionGetOriginalUri action = 
    {#call web_navigation_action_get_original_uri#} (toWebNavigationAction action) >>= peekCString
  
-- | Returns the reason why WebKit is requesting a navigation.
webNavigationActionGetReason ::
   WebNavigationActionClass self => self
 -> IO NavigationReason  
webNavigationActionGetReason action = 
    liftM (toEnum . fromIntegral) $ {#call web_navigation_action_get_reason#} (toWebNavigationAction action)

-- | Returns the target frame of the action.
webNavigationActionGetTargetFrame ::  
   WebNavigationActionClass self => self
 -> IO String
webNavigationActionGetTargetFrame action = 
    {#call web_navigation_action_get_target_frame#} (toWebNavigationAction action) >>= peekCString
  
-- | Sets the URI that was originally requested. 
-- This may differ from the navigation target, for instance because of a redirect.
webNavigationActionSetOriginalUri ::
   WebNavigationActionClass self => self
 -> String
 -> IO ()
webNavigationActionSetOriginalUri action uri =
    withCString uri $ \uriPtr -> 
        {#call web_navigation_action_set_original_uri#} 
        (toWebNavigationAction action)
        uriPtr

-- | Sets the reason why WebKit is requesting a navigation.
webNavigationActionSetReason ::
   WebNavigationActionClass self => self
 -> NavigationReason
 -> IO ()
webNavigationActionSetReason action reason =
    {#call web_navigation_action_set_reason#} (toWebNavigationAction action) (fromIntegral (fromEnum reason))
  
