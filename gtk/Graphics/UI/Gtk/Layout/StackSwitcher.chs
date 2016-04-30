{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widgets StackSwitcher
--
--  Author : Moritz Schulte
--
--  Created: 27 April 2016
--
--  Copyright (C) 2016 Moritz Schulte
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
-- A widget which controls the alignment and size of its child
--
module Graphics.UI.Gtk.Layout.StackSwitcher (
-- * Detail
--
-- [...]
--
-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Box'
-- |                           +----'StackSwitcher'
-- @

-- * Types
#if GTK_CHECK_VERSION(3,10,0)
    StackSwitcher
  , castToStackSwitcher
  , gTypeStackSwitcher
  , toStackSwitcher

-- * Constructors
  , stackSwitcherNew

-- * Methods
  , stackSwitcherSetStack
  , stackSwitcherGetStack

-- * Attributes
  , stackSwitcherIconSize
  , stackSwitcherStack
#endif
) where

#if GTK_CHECK_VERSION(3,10,0)

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Creates a new 'StackSwitcher'.
--
stackSwitcherNew :: IO StackSwitcher
stackSwitcherNew =
  makeNewObject mkStackSwitcher $
  liftM (castPtr :: Ptr Widget -> Ptr StackSwitcher) $
  {# call unsafe stack_switcher_new #}

--------------------
-- Methods

-- | Sets the stack to control.
stackSwitcherSetStack :: (StackSwitcherClass self, StackClass stack) => self
 -> stack
 -> IO ()
stackSwitcherSetStack self stack =
  {# call stack_switcher_set_stack #}
    (toStackSwitcher self)
    (toStack stack)

-- | Retrieves the stack.
stackSwitcherGetStack :: StackSwitcherClass self => self
 -> IO (Maybe Stack)
stackSwitcherGetStack self =
  maybeNull (makeNewObject mkStack) $
  {# call stack_switcher_get_stack #}
    (toStackSwitcher self)

--------------------
-- Attributes

-- | Use the "icon-size" property to change the size of the image
-- displayed when a GtkStackSwitcher is displaying icons.
--
-- Default value: @1@
--
stackSwitcherIconSize :: StackSwitcherClass self => Attr self Int
stackSwitcherIconSize = newAttrFromIntProperty "icon-size"

-- | The 'Stack' controlled by this 'StackSwitcher'.
--
stackSwitcherStack :: (StackSwitcherClass self, StackClass stack) =>
  ReadWriteAttr self (Maybe Stack) (Maybe stack)
stackSwitcherStack = newAttrFromMaybeObjectProperty "stack" gTypeContainer

#endif
