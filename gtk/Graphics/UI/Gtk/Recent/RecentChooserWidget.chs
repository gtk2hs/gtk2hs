{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget RecentChooserWidget
--
--  Author : Andy Stewart
--
--  Created: 27 Mar 2010
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
-- Displays recently used files
--
-- * Module available since Gtk+ version 2.10
--
module Graphics.UI.Gtk.Recent.RecentChooserWidget (

-- * Detail
--
-- | 'RecentChooserWidget' is a widget suitable for selecting recently used
-- files. It is the main building block of a 'RecentChooserDialog'. Most
-- applications will only need to use the latter; you can use
-- 'RecentChooserWidget' as part of a larger window if you have special needs.
--
-- Note that 'RecentChooserWidget' does not have any methods of its own.
-- Instead, you should use the functions that work on a 'RecentChooser'.
--
-- Recently used files are supported since Gtk+ 2.10.

-- * Class Hierarchy
--
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Box'
-- |                           +----'VBox'
-- |                                 +----RecentChooserWidget
-- @

#if GTK_CHECK_VERSION(2,10,0)
-- * Types
  RecentChooserWidget,
  RecentChooserWidgetClass,
  castToRecentChooserWidget,
  toRecentChooserWidget,

-- * Constructors
  recentChooserWidgetNew,
  recentChooserWidgetNewForManager,
#endif
  ) where

#if GTK_CHECK_VERSION(2,10,0)

import Control.Monad    (liftM)

import System.Glib.FFI
{#import Graphics.UI.Gtk.Types#}
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Creates a new 'RecentChooserWidget' object. This is an embeddable widget
-- used to access the recently used resources list.
--
recentChooserWidgetNew :: IO RecentChooserWidget
recentChooserWidgetNew =
  makeNewObject mkRecentChooserWidget $
  liftM (castPtr :: Ptr Widget -> Ptr RecentChooserWidget) $
  {# call gtk_recent_chooser_widget_new #}

-- | Creates a new 'RecentChooserWidget' with a specified recent manager.
--
-- This is useful if you have implemented your own recent manager, or if you
-- have a customized instance of a 'RecentManager' object.
--
recentChooserWidgetNewForManager :: RecentManagerClass manager
                                 => manager -- ^ @manager@ - a 'RecentManager'
                                 -> IO RecentChooserWidget -- ^ returns a new 'RecentChooserWidget'
recentChooserWidgetNewForManager manager =
  makeNewObject mkRecentChooserWidget $
  liftM (castPtr :: Ptr Widget -> Ptr RecentChooserWidget) $
  {# call gtk_recent_chooser_widget_new_for_manager #}
      (toRecentManager manager)
#endif
