-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Image
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
--
--  Version $Revision: 1.2 $ from $Date: 2005/02/12 17:19:22 $
--
--  Copyright (C) 2001-2005 Axel Simon
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
-- This widget displays an image.
--
--
-- * Because Haskell is not the best language to modify large images directly
--   only functions are bound that allow loading images from disc or by stock
--   names.
--
-- * Another function for extracting the 'Pixbuf' is added for 
--   'CellRenderer'.
--
-- TODO
--
-- * Figure out what other functions are useful within Haskell. Maybe we should
--   support loading Pixmaps without exposing them.
--
module Graphics.UI.Gtk.Display.Image (
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
  imageSetFromPixbuf,
  imageNewFromPixbuf
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
import System.Glib.GObject		(makeNewGObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.General.Structs	(IconSize, iconSizeInvalid, iconSizeMenu,
					 iconSizeSmallToolbar, iconSizeLargeToolbar,
					 iconSizeButton, iconSizeDialog)

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Create an image by loading a file.
--
imageNewFromFile :: FilePath -> IO Image
imageNewFromFile path = makeNewObject mkImage $ liftM castPtr $ 
#ifdef WIN32 && GTK_VERSION(2,6,0)
  withUTFString path {#call unsafe image_new_from_file_utf8#}
#else
  withUTFString path {#call unsafe image_new_from_file#}
#endif

-- | Create a set of images by specifying a stock
-- object.
--
imageNewFromStock :: String -> IconSize -> IO Image
imageNewFromStock stock ic = withUTFString stock $ \strPtr -> 
  makeNewObject mkImage $ liftM castPtr $ {#call unsafe image_new_from_stock#}
  strPtr (fromIntegral ic)

-- | Extract the Pixbuf from the 'Image'.
--
imageGetPixbuf :: Image -> IO Pixbuf
imageGetPixbuf img = makeNewGObject mkPixbuf $ liftM castPtr $
  throwIfNull "Image.imageGetPixbuf: The image contains no Pixbuf object." $
  {#call unsafe image_get_pixbuf#} img


-- | Overwrite the current content of the 'Image' with a new 'Pixbuf'.
--
imageSetFromPixbuf :: Image -> Pixbuf -> IO ()
imageSetFromPixbuf img pb = {#call unsafe gtk_image_set_from_pixbuf#} img pb

-- | Create an 'Image' from a 
-- 'Pixbuf'.
--
imageNewFromPixbuf :: Pixbuf -> IO Image
imageNewFromPixbuf pbuf = makeNewObject mkImage $ liftM castPtr $
  {#call unsafe image_new_from_pixbuf#} pbuf
