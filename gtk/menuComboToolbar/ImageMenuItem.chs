-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget ImageMenuItem@
--
--  Author : Jonas Svensson
--          
--  Created: 12 Aug 2002
--
--  Version $Revision: 1.2 $
--
--  Copyright (c) 2002 Jonas Svensson
--
--  This file is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This file is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
-- @description@ --------------------------------------------------------------
--
-- * This widget implements a @ref data MenuItem@ with an image next to it 
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------
--
-- imageMenuItemNewFromSock should also have a AccelGroup argument
-- 

module ImageMenuItem(
  ImageMenuItem,
  ImageMenuItemClass,
  imageMenuItemSetImage,
  imageMenuItemGetImage,
  imageMenuItemNew,
  imageMenuItemNewFromStock,
  imageMenuItemNewWithLabel,
  imageMenuItemNewWithMnemonic
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}

{#context lib="gtk" prefix="gtk" #}

-- methods


-- @method imageMenuItemSetImage@ Sets the image for the ImageMenuItem.
--
imageMenuItemSetImage :: (ImageMenuItemClass imi,WidgetClass wd) =>
                         imi -> wd -> IO ()
imageMenuItemSetImage imi wd =
  {#call unsafe image_menu_item_set_image#} (toImageMenuItem imi) 
                                            (toWidget wd)

-- @method imageMenuItemGetImage@ Get the image that is currently 
-- set a the image.
--
imageMenuItemGetImage :: ImageMenuItemClass imi => imi -> IO (Maybe Widget)
imageMenuItemGetImage imi = do
   imPtr <- {#call unsafe image_menu_item_get_image#} (toImageMenuItem imi)
   if imPtr==nullPtr then return Nothing else do
     liftM Just $ makeNewObject mkWidget $ return imPtr

-- @constructor imageMenuItemNew@ Create a new @ref arg MenuItem@ with a image
-- next to it.
--
imageMenuItemNew :: IO ImageMenuItem
imageMenuItemNew  = makeNewObject mkImageMenuItem $ liftM castPtr $
  {#call unsafe image_menu_item_new#}

-- @constructor imageMenuItemNewFromStock@ Create a new @ref arg MenuItem@
-- with a stock image.
--
imageMenuItemNewFromStock :: String -> IO ImageMenuItem
imageMenuItemNewFromStock str = withCString str $ \strPtr ->
  makeNewObject mkImageMenuItem $ liftM castPtr $ 
  {#call unsafe image_menu_item_new_from_stock#} strPtr nullPtr

-- @constructor imageMenuItemNewWithLabel@ Create a new @ref arg MenuItem@
-- with a label.
--
imageMenuItemNewWithLabel :: String -> IO ImageMenuItem
imageMenuItemNewWithLabel str = withCString str $ \strPtr ->
  makeNewObject mkImageMenuItem $ liftM castPtr $ 
  {#call unsafe image_menu_item_new_with_label#} strPtr

-- @constructor imageMenuItemNewWithMnemonic@ Create a new @ref arg MenuItem@
-- with a label where underscored indicate the mnemonic.
--
imageMenuItemNewWithMnemonic :: String -> IO ImageMenuItem
imageMenuItemNewWithMnemonic str = withCString str $ \strPtr ->
  makeNewObject mkImageMenuItem $ liftM castPtr $ 
  {#call unsafe image_menu_item_new_with_mnemonic#} strPtr
