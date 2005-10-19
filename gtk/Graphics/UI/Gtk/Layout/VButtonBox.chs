-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget VButtonBox
--
--  Author : Matthew Walton
--
--  Created: 28 April 2004
--
--  Version $Revision: 1.5 $ from $Date: 2005/10/19 12:57:37 $
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
-- A container for arranging buttons vertically
--
module Graphics.UI.Gtk.Layout.VButtonBox (
-- * Detail
-- 
-- | A button box should be used to provide a consistent layout of buttons
-- throughout your application. The layout\/spacing can be altered by the
-- programmer, or if desired, by the user to alter the \'feel\' of a program to
-- a small degree.
--
-- A 'VButtonBox' is created with 'vButtonBoxNew'. Buttons are packed into a
-- button box the same way widgets are added to any other container, using
-- 'containerAdd'. You can also use 'boxPackStart' or 'boxPackEnd', but for
-- button boxes both these functions work just like 'containerAdd', ie., they
-- pack the button in a way that depends on the current layout style and on
-- whether the button has had 'buttonBoxSetChildSecondary' called on it.
--
-- The spacing between buttons can be set with 'boxSetSpacing'. The
-- arrangement and layout of the buttons can be changed with
-- 'buttonBoxSetLayout'.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Box'
-- |                           +----'ButtonBox'
-- |                                 +----VButtonBox
-- @

-- * Types
  VButtonBox,
  VButtonBoxClass,
  castToVButtonBox,
  toVButtonBox,

-- * Constructors
  vButtonBoxNew,
  ) where

import Monad (liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Creates a new vertical button box.
--
vButtonBoxNew :: IO VButtonBox
vButtonBoxNew =
  makeNewObject mkVButtonBox $
  liftM (castPtr :: Ptr Widget -> Ptr VButtonBox) $
  {# call unsafe vbutton_box_new #}
