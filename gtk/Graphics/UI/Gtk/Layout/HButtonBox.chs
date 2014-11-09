{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget HButtonBox
--
--  Author : Matthew Walton
--
--  Created: 29 April 2004
--
--  Copyright (C) 2004-2005 Matthew Walton
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
-- A container for arranging buttons horizontally
--
module Graphics.UI.Gtk.Layout.HButtonBox (
-- * Detail
--
-- | A button box should be used to provide a consistent layout of buttons
-- throughout your application. The layout\/spacing can be altered by the
-- programmer, or if desired, by the user to alter the \'feel\' of a program to
-- a small degree.
--
-- A 'HButtonBox' is created with 'hButtonBoxNew'. Buttons are packed
-- into a button box the same way widgets are added to any other
-- container, using
-- 'Graphics.UI.Gtk.Abstract.Container.containerAdd'. You can also use
-- 'Graphics.UI.Gtk.Abstract.Box.boxPackStart' or
-- 'Graphics.UI.Gtk.Abstract.Box.boxPackEnd', but for button boxes
-- both these functions work just like
-- 'Graphics.UI.Gtk.Abstract.Container.containerAdd', ie., they pack
-- the button in a way that depends on the current layout style and on
-- whether the button has had
-- 'Graphics.UI.Gtk.Abstract.ButtonBox.buttonBoxSetChildSecondary'
-- called on it.
--
-- The spacing between buttons can be set with
-- 'Graphics.UI.Gtk.Abstract.Box.boxSetSpacing'. The arrangement and
-- layout of the buttons can be changed with
-- 'Graphics.UI.Gtk.Abstract.ButtonBox.buttonBoxSetLayout'.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Box'
-- |                           +----'ButtonBox'
-- |                                 +----HButtonBox
-- @

-- * Types
  HButtonBox,
  HButtonBoxClass,
  castToHButtonBox, gTypeHButtonBox,
  toHButtonBox,

-- * Constructors
  hButtonBoxNew,
  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
{#import Graphics.UI.Gtk.Types#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Creates a new horizontal button box.
--
hButtonBoxNew :: IO HButtonBox
hButtonBoxNew =
  makeNewObject mkHButtonBox $
  liftM (castPtr :: Ptr Widget -> Ptr HButtonBox) $
  {# call unsafe hbutton_box_new #}
