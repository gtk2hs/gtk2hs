{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget ImageMenuItem
--
--  Author : Jonas Svensson
--
--  Created: 12 Aug 2002
--
--  Copyright (C) 2002 Jonas Svensson
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
-- TODO
--
-- imageMenuItemNewFromSock should also have a AccelGroup argument
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- A menu item with an icon
--
module Graphics.UI.Gtk.MenuComboToolbar.ImageMenuItem (
-- * Detail
--
-- | A 'ImageMenuItem' is a menu item which has an icon next to the text
-- label.
--
-- Note that the user can disable display of menu icons, so make sure to
-- still fill in the text label.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Bin'
-- |                           +----'Item'
-- |                                 +----'MenuItem'
-- |                                       +----ImageMenuItem
-- @

-- * Types
  ImageMenuItem,
  ImageMenuItemClass,
  castToImageMenuItem, gTypeImageMenuItem,
  toImageMenuItem,

-- * Constructors
  imageMenuItemNew,
  imageMenuItemNewFromStock,
  imageMenuItemNewWithLabel,
  imageMenuItemNewWithMnemonic,

-- * Methods
  imageMenuItemSetImage,
  imageMenuItemGetImage,

-- * Attributes
  imageMenuItemImage,
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}
import Graphics.UI.Gtk.General.StockItems

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Creates a new 'ImageMenuItem' with an empty label.
--
imageMenuItemNew :: IO ImageMenuItem
imageMenuItemNew =
  makeNewObject mkImageMenuItem $
  liftM (castPtr :: Ptr Widget -> Ptr ImageMenuItem) $
  {# call unsafe image_menu_item_new #}

-- | Creates a new 'ImageMenuItem' containing the image and text from a stock
-- item.
--
imageMenuItemNewFromStock ::
    StockId          -- ^ @stockId@ - the name of the stock item.
 -> IO ImageMenuItem
imageMenuItemNewFromStock stockId =
  makeNewObject mkImageMenuItem $
  liftM (castPtr :: Ptr Widget -> Ptr ImageMenuItem) $
  withUTFString stockId $ \stockIdPtr ->
  {# call unsafe image_menu_item_new_from_stock #}
    stockIdPtr
    (AccelGroup nullForeignPtr)

-- | Creates a new 'ImageMenuItem' containing a label.
--
imageMenuItemNewWithLabel :: GlibString string
 => string           -- ^ @label@ - the text of the menu item.
 -> IO ImageMenuItem
imageMenuItemNewWithLabel label =
  makeNewObject mkImageMenuItem $
  liftM (castPtr :: Ptr Widget -> Ptr ImageMenuItem) $
  withUTFString label $ \labelPtr ->
  {# call unsafe image_menu_item_new_with_label #}
    labelPtr

-- | Creates a new 'ImageMenuItem' containing a label. The label will be
-- created using 'Graphics.UI.Gtk.Display.Label.labelNewWithMnemonic', so
-- underscores in @label@ indicate the mnemonic for the menu item.
--
imageMenuItemNewWithMnemonic :: GlibString string
 => string           -- ^ @label@ - the text of the menu item, with an
                     -- underscore in front of the mnemonic character
 -> IO ImageMenuItem
imageMenuItemNewWithMnemonic label =
  makeNewObject mkImageMenuItem $
  liftM (castPtr :: Ptr Widget -> Ptr ImageMenuItem) $
  withUTFString label $ \labelPtr ->
  {# call unsafe image_menu_item_new_with_mnemonic #}
    labelPtr

--------------------
-- Methods

-- | Sets the image of the image menu item to the given widget. Note that it
-- depends on the \"show-menu-images\" setting whether the image will be
-- displayed or not.
--
imageMenuItemSetImage :: (ImageMenuItemClass self, WidgetClass image) => self
 -> image -- ^ @image@ - a widget to set as the image for the menu item.
 -> IO ()
imageMenuItemSetImage self image =
  {# call unsafe image_menu_item_set_image #}
    (toImageMenuItem self)
    (toWidget image)

-- | Gets the widget that is currently set as the image.
-- See 'imageMenuItemSetImage'.
--
imageMenuItemGetImage :: ImageMenuItemClass self => self
 -> IO (Maybe Widget) -- ^ returns the widget set as image of or @Nothing@ if
                      -- none has been set.
imageMenuItemGetImage self =
  maybeNull (makeNewObject mkWidget) $
  {# call unsafe image_menu_item_get_image #}
    (toImageMenuItem self)

--------------------
-- Attributes

-- | Child widget to appear next to the menu text.
--
imageMenuItemImage :: (ImageMenuItemClass self, WidgetClass image) => ReadWriteAttr self (Maybe Widget) image
imageMenuItemImage = newAttr
  imageMenuItemGetImage
  imageMenuItemSetImage
