-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget Image@
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.2 $ from $Date: 2002/05/24 09:43:24 $
--
--  Copyright (c) 1999..2002 Axel Simon
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
-- * This widget displays an image.
--
-- @documentation@ ------------------------------------------------------------
--
-- * Because Haskell is not the best language to modify large images directly
--   only functions are bound that allow loading images from disc or by stock
--   names.
--
-- * Another function for extracting the GdkPixbuf is added for @CellRenderer.
--
-- @todo@ ---------------------------------------------------------------------
--
-- * Figure out what other functions are useful within Haskell. Maybe we should
--   support loading Pixmaps without exposing them.
--
module Image(
  Image,
  ImageClass,
  castToImage,
  imageNewFromFile,
  IconSize,
  iconSizeMenu,
  iconSizeSmallToolbar,
  iconSizeLargeToolbar,
  iconSizeButton,
  iconSizeDialog,
  imageNewFromStock,
  imageGetPixbuf,
  imageNewFromPixbuf
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
import GObject	(makeNewGObject)
{#import Hierarchy#}
{#import Signal#}
import Structs	(IconSize, iconSizeInvalid, iconSizeMenu, iconSizeSmallToolbar,
		 iconSizeLargeToolbar, iconSizeButton, iconSizeDialog)

{# context lib="gtk" prefix="gtk" #}

-- methods

-- @method imageNewFromFile@ Create an image by loading a file.
--
imageNewFromFile :: FilePath -> IO Image
imageNewFromFile path = makeNewObject mkImage $ liftM castPtr $ 
  withCString path {#call unsafe image_new_from_file#}

-- @method imageNewFromStock@ Create a set of images by specifying a stock
-- object.
--
imageNewFromStock :: String -> IconSize -> IO Image
imageNewFromStock stock ic = withCString stock $ \strPtr -> 
  makeNewObject mkImage $ liftM castPtr $ {#call unsafe image_new_from_stock#}
  strPtr (fromIntegral ic)

-- Extract the GdkPixbuf from the @Image.
--
imageGetPixbuf :: Image -> IO GdkPixbuf
imageGetPixbuf img = makeNewGObject mkGdkPixbuf $ liftM castPtr $
  throwIfNull "Image.imageGetPixbuf: The image contains no GdkPixbuf object." $
  {#call unsafe image_get_pixbuf#} img


-- Create an @Image from a @Pixbuf.
--
imageNewFromPixbuf :: GdkPixbuf -> IO Image
imageNewFromPixbuf pbuf = makeNewObject mkImage $ liftM castPtr $
  {#call unsafe image_new_from_pixbuf#} pbuf