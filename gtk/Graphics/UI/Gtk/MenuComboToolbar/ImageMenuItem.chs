-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget ImageMenuItem
--
--  Author : Jonas Svensson
--
--  Created: 12 Aug 2002
--
--  Version $Revision: 1.4 $ from $Date: 2005/02/25 22:53:41 $
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
-- This widget implements a 'MenuItem' with an image next to it 
--
module Graphics.UI.Gtk.MenuComboToolbar.ImageMenuItem (
-- * Description
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
  castToImageMenuItem,

-- * Constructors
  imageMenuItemNew,
  imageMenuItemNewFromStock,
  imageMenuItemNewWithLabel,
  imageMenuItemNewWithMnemonic,

-- * Methods
  imageMenuItemSetImage,
  imageMenuItemGetImage
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Create a new 'MenuItem' with a image next to it.
--
imageMenuItemNew :: IO ImageMenuItem
imageMenuItemNew  = makeNewObject mkImageMenuItem $ liftM castPtr $
  {#call unsafe image_menu_item_new#}

-- | Create a new 'MenuItem' with a stock image.
--
imageMenuItemNewFromStock :: String -> IO ImageMenuItem
imageMenuItemNewFromStock str = withUTFString str $ \strPtr ->
  makeNewObject mkImageMenuItem $ liftM castPtr $ 
  {#call unsafe image_menu_item_new_from_stock#} strPtr
    (AccelGroup nullForeignPtr)

-- | Create a new 'MenuItem' with a label.
--
imageMenuItemNewWithLabel :: String -> IO ImageMenuItem
imageMenuItemNewWithLabel str = withUTFString str $ \strPtr ->
  makeNewObject mkImageMenuItem $ liftM castPtr $ 
  {#call unsafe image_menu_item_new_with_label#} strPtr

-- | Create a new 'MenuItem' with a label where underscored indicate the
-- mnemonic.
--
imageMenuItemNewWithMnemonic :: String -> IO ImageMenuItem
imageMenuItemNewWithMnemonic str = withUTFString str $ \strPtr ->
  makeNewObject mkImageMenuItem $ liftM castPtr $ 
  {#call unsafe image_menu_item_new_with_mnemonic#} strPtr

--------------------
-- Methods

-- | Sets the image for the ImageMenuItem.
--
imageMenuItemSetImage :: (ImageMenuItemClass imi,WidgetClass wd) =>
                         imi -> wd -> IO ()
imageMenuItemSetImage imi wd =
  {#call unsafe image_menu_item_set_image#} (toImageMenuItem imi) 
                                            (toWidget wd)

-- | Get the image that is currently set a the image.
--
imageMenuItemGetImage :: ImageMenuItemClass imi => imi -> IO (Maybe Widget)
imageMenuItemGetImage imi = do
   imPtr <- {#call unsafe image_menu_item_get_image#} (toImageMenuItem imi)
   if imPtr==nullPtr then return Nothing else do
     liftM Just $ makeNewObject mkWidget $ return imPtr
