-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Image
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
--
--  Version $Revision: 1.4 $ from $Date: 2005/02/25 01:11:32 $
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

-- | Create an image by loading a file.
--
imageNewFromFile :: FilePath -> IO Image
imageNewFromFile path = makeNewObject mkImage $ liftM castPtr $ 
#if defined (WIN32) && GTK_CHECK_VERSION(2,6,0)
  withUTFString path {#call unsafe image_new_from_file_utf8#}
#else
  withUTFString path {#call unsafe image_new_from_file#}
#endif

-- | Create an 'Image' from a 
-- 'Pixbuf'.
--
imageNewFromPixbuf :: Pixbuf -> IO Image
imageNewFromPixbuf pbuf = makeNewObject mkImage $ liftM castPtr $
  {#call unsafe image_new_from_pixbuf#} pbuf

-- | Create a set of images by specifying a stock
-- object.
--
imageNewFromStock :: String -> IconSize -> IO Image
imageNewFromStock stock ic = withUTFString stock $ \strPtr -> 
  makeNewObject mkImage $ liftM castPtr $ {#call unsafe image_new_from_stock#}
  strPtr (fromIntegral ic)

--------------------
-- Methods

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
