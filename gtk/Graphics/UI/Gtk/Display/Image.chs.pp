-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Image
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
--
--  Version $Revision: 1.6 $ from $Date: 2005/04/02 19:38:29 $
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
-- TODO
--
-- Figure out what other functions are useful within Haskell. Maybe we should
--   support loading Pixmaps without exposing them.
--
-- Because Haskell is not the best language to modify large images directly
--   only functions are bound that allow loading images from disc or by stock
--   names.
--
-- Another function for extracting the 'Pixbuf' is added for 
--   'CellRenderer'.
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- A widget displaying an image
--
module Graphics.UI.Gtk.Display.Image (
-- * Description
-- 
-- | The 'Image' widget displays an image. Various kinds of object can be
-- displayed as an image; most typically, you would load a 'Pixbuf' (\"pixel
-- buffer\") from a file, and then display that. There's a convenience function
-- to do this, 'imageNewFromFile', used as follows: If the file isn't loaded
-- successfully, the image will contain a \"broken image\" icon similar to that
-- used in many web browsers. If you want to handle errors in loading the file
-- yourself, for example by displaying an error message, then load the image
-- with 'pixbufNewFromFile', then create the 'Image' with 'imageNewFromPixbuf'.
--
-- >   image <- imageNewFromFile "myfile.png"
--
-- The image file may contain an animation, if so the 'Image' will display
-- an animation ('PixbufAnimation') instead of a static image.
--
-- 'Image' is a subclass of 'Misc', which implies that you can align it
-- (center, left, right) and add padding to it, using 'Misc' methods.
--
-- 'Image' is a \"no window\" widget (has no \"Gdk Window\" of its own), so by
-- default does not receive events. If you want to receive events on the image,
-- such as button clicks, place the image inside a 'EventBox', then connect to
-- the event signals on the event box.
--
-- When handling events on the event box, keep in mind that coordinates in
-- the image may be different from event box coordinates due to the alignment
-- and padding settings on the image (see 'Misc'). The simplest way to solve
-- this is to set the alignment to 0.0 (left\/top), and set the padding to
-- zero. Then the origin of the image will be the same as the origin of the
-- event box.
--
-- Sometimes an application will want to avoid depending on external data
-- files, such as image files. Gtk+ comes with a program to avoid this, called
-- gdk-pixbuf-csource. This program allows you to convert an image into a C
-- variable declaration, which can then be loaded into a 'Pixbuf' using
-- 'pixbufNewFromInline'.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Misc'
-- |                     +----Image
-- @

-- * Types
  Image,
  ImageClass,
  castToImage,

-- * Constructors
  imageNewFromFile,
  imageNewFromPixbuf,
  imageNewFromStock,

-- * Methods
  imageGetPixbuf,
  imageSetFromPixbuf,

-- * Icon Sizes
  IconSize,
  iconSizeMenu,
  iconSizeSmallToolbar,
  iconSizeLargeToolbar,
  iconSizeButton,
  iconSizeDialog,
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

--------------------
-- Constructors

-- | Creates a new 'Image' displaying the file @filename@. If the file isn't
-- found or can't be loaded, the resulting 'Image' will display a \"broken
-- image\" icon.
--
-- If the file contains an animation, the image will contain an animation.
--
-- If you need to detect failures to load the file, use 'pixbufNewFromFile'
-- to load the file yourself, then create the 'Image' from the pixbuf. (Or for
-- animations, use 'pixbufAnimationNewFromFile').
--
-- The storage type ('imageGetStorageType') of the returned image is not
-- defined, it will be whatever is appropriate for displaying the file.
--
imageNewFromFile :: FilePath -> IO Image
imageNewFromFile filename =
  makeNewObject mkImage $
  liftM (castPtr :: Ptr Widget -> Ptr Image) $
  withUTFString filename $ \filenamePtr ->
#if defined (WIN32) && GTK_CHECK_VERSION(2,6,0)
  {# call unsafe gtk_image_new_from_file_utf8 #}
#else
  {# call unsafe gtk_image_new_from_file #}
#endif
    filenamePtr

-- | Creates a new 'Image' displaying a 'Pixbuf'.
--
-- Note that this function just creates an 'Image' from the pixbuf. The
-- 'Image' created will not react to state changes. Should you want that, you
-- should use 'imageNewFromIconSet'.
--
imageNewFromPixbuf :: Pixbuf -> IO Image
imageNewFromPixbuf pixbuf =
  makeNewObject mkImage $
  liftM (castPtr :: Ptr Widget -> Ptr Image) $
  {# call unsafe image_new_from_pixbuf #}
    pixbuf

-- | Creates a 'Image' displaying a stock icon. If the stock icon name isn't
-- known, a \"broken image\" icon will be displayed instead.
--
imageNewFromStock :: 
    String   -- ^ @stockId@ - a stock icon name
 -> IconSize -- ^ @size@ - a stock icon size
 -> IO Image
imageNewFromStock stockId size =
  makeNewObject mkImage $
  liftM (castPtr :: Ptr Widget -> Ptr Image) $
  withUTFString stockId $ \stockIdPtr ->
  {# call unsafe image_new_from_stock #}
    stockIdPtr
    (fromIntegral size)

--------------------
-- Methods

-- | Gets the 'Pixbuf' being displayed by the 'Image'. The storage type of the
-- image must be 'ImageEmpty' or 'ImagePixbuf' (see 'imageGetStorageType').
--
imageGetPixbuf :: Image -> IO Pixbuf
imageGetPixbuf self =
  makeNewGObject mkPixbuf $ liftM castPtr $
  throwIfNull "Image.imageGetPixbuf: The image contains no Pixbuf object." $
  {# call unsafe image_get_pixbuf #}
    self

-- | Overwrite the current content of the 'Image' with a new 'Pixbuf'.
--
imageSetFromPixbuf :: Image -> Pixbuf -> IO ()
imageSetFromPixbuf self pixbuf =
  {# call unsafe gtk_image_set_from_pixbuf #}
    self
    pixbuf
