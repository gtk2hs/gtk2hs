{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Image
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
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
-- * Detail
--
-- | The 'Image' widget displays an image. Various kinds of object can be
-- displayed as an image; most typically, you would load a 'Pixbuf' (\"pixel
-- buffer\") from a file, and then display that. There's a convenience function
-- to do this, 'imageNewFromFile', used as follows: If the file isn't loaded
-- successfully, the image will contain a \"broken image\" icon similar to that
-- used in many web browsers. If you want to handle errors in loading the file
-- yourself, for example by displaying an error message, then load the image
-- with 'Graphics.UI.Gtk.Gdk.Pixbuf.pixbufNewFromFile', then create the
-- 'Image' with 'imageNewFromPixbuf'.
--
-- >   image <- imageNewFromFile "myfile.png"
--
-- The image file may contain an animation, if so the 'Image' will display
-- an animation ('PixbufAnimation') instead of a static image.
--
-- 'Image' is a subclass of 'Misc', which implies that you can align it
-- (center, left, right) and add padding to it, using 'Misc' methods.
--
-- 'Image' is a \"no window\" widget (has no 'DrawWindow' of its own), so by
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
-- 'Graphics.UI.Gtk.Gdk.Pixbuf.pixbufNewFromInline'.

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
  castToImage, gTypeImage,
  toImage,
  ImageType(..),

-- * Constructors
  imageNewFromFile,
  imageNewFromPixbuf,
  imageNewFromAnimation,
  imageNewFromStock,
  imageNew,
#if GTK_CHECK_VERSION(2,6,0)
  imageNewFromIconName,
#endif

-- * Methods
  imageGetPixbuf,
  imageSetFromPixbuf,
  imageSetFromAnimation,
  imageSetFromFile,
  imageSetFromStock,
#if GTK_CHECK_VERSION(2,6,0)
  imageSetFromIconName,
  imageSetPixelSize,
  imageGetPixelSize,
#endif
#if GTK_CHECK_VERSION(2,8,0)
  imageClear,
#endif

-- * Icon Sizes
  IconSize(..),

-- * Attributes
  imagePixbuf,
#if GTK_MAJOR_VERSION < 3
  imagePixmap,
  imageMask,
#endif
  imageAnimation,
  imageImage,
  imageFile,
  imageStock,
  imageIconSize,
#if GTK_CHECK_VERSION(2,6,0)
  imagePixelSize,
#endif
#if GTK_CHECK_VERSION(2,6,0)
  imageIconName,
#endif
  imageStorageType,
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}
import Graphics.UI.Gtk.General.StockItems
import Graphics.UI.Gtk.General.Structs  (IconSize(..))

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Types

-- | Describes the image data representation used by a 'Image'. If you want to
-- get the image from the widget, you can only get the currently-stored
-- representation. e.g. if the 'imageStorageType' is 'ImagePixbuf',
-- then you can call 'imageGetPixbuf' but not 'imageGetStock'. For empty
-- images, you can request any storage type (call any of the "get" functions),
-- but they will all return @Nothing@.
--
{# enum ImageType {underscoreToCase} deriving (Show, Eq) #}

--------------------
-- Constructors

-- | Creates a new 'Image' displaying the file @filename@. If the file isn't
-- found or can't be loaded, the resulting 'Image' will display a \"broken
-- image\" icon.
--
-- If the file contains an animation, the image will contain an animation.
--
-- If you need to detect failures to load the file, use
-- 'Graphics.UI.Gtk.Gdk.Pixbuf.pixbufNewFromFile'
-- to load the file yourself, then create the 'Image' from the pixbuf. (Or for
-- animations, use
-- 'Graphics.UI.Gtk.Gdk.Pixbuf.pixbufAnimationNewFromFile').
--
-- The storage type ('imageGetStorageType') of the returned image is not
-- defined, it will be whatever is appropriate for displaying the file.
--
imageNewFromFile :: GlibFilePath fp => fp -> IO Image
imageNewFromFile filename =
  makeNewObject mkImage $
  liftM (castPtr :: Ptr Widget -> Ptr Image) $
  withUTFFilePath filename $ \filenamePtr ->
#if defined (WIN32) && GTK_CHECK_VERSION(2,6,0) && GTK_MAJOR_VERSION < 3
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


imageNewFromAnimation :: (PixbufAnimationClass animation) => animation -> IO Image
imageNewFromAnimation pba = makeNewObject mkImage $
  liftM (castPtr :: Ptr Widget -> Ptr Image) $
  {# call unsafe image_new_from_animation #} (toPixbufAnimation pba)


-- | Creates a 'Image' displaying a stock icon. If the stock icon name isn't
-- known, the image will be empty.
--
imageNewFromStock ::
    StockId  -- ^ @stockId@ - a stock icon name
 -> IconSize -- ^ @size@ - a stock icon size
 -> IO Image
imageNewFromStock stockId size =
  makeNewObject mkImage $
  liftM (castPtr :: Ptr Widget -> Ptr Image) $
  withUTFString stockId $ \stockIdPtr ->
  {# call unsafe image_new_from_stock #}
    stockIdPtr
    ((fromIntegral . fromEnum) size)

-- | Creates a new empty 'Image' widget.
--
imageNew :: IO Image
imageNew =
  makeNewObject mkImage $
  liftM (castPtr :: Ptr Widget -> Ptr Image) $
  {# call gtk_image_new #}

#if GTK_CHECK_VERSION(2,6,0)
-- | Creates a 'Image' displaying an icon from the current icon theme. If the
-- icon name isn't known, a \"broken image\" icon will be displayed instead. If
-- the current icon theme is changed, the icon will be updated appropriately.
--
-- * Available since Gtk+ version 2.6
--
imageNewFromIconName :: GlibString string
 => string   -- ^ @iconName@ - an icon name
 -> IconSize -- ^ @size@ - a stock icon size
 -> IO Image
imageNewFromIconName iconName size =
  makeNewObject mkImage $
  liftM (castPtr :: Ptr Widget -> Ptr Image) $
  withUTFString iconName $ \iconNamePtr ->
  {# call gtk_image_new_from_icon_name #}
    iconNamePtr
    ((fromIntegral . fromEnum) size)
#endif

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


imageSetFromAnimation :: (PixbufAnimationClass animation) => Image -> animation -> IO ()
imageSetFromAnimation self pba =
  {# call unsafe gtk_image_set_from_animation #}
    self
    (toPixbufAnimation pba)

-- | See 'imageNewFromFile' for details.
--
imageSetFromFile :: GlibFilePath fp => Image -> fp -> IO ()
imageSetFromFile self filename =
  withUTFFilePath filename $ \filenamePtr ->
#if defined (WIN32) && GTK_CHECK_VERSION(2,6,0) && GTK_MAJOR_VERSION < 3
  {# call gtk_image_set_from_file_utf8 #}
#else
  {# call gtk_image_set_from_file #}
#endif
    self
    filenamePtr

-- | See 'imageNewFromStock' for details.
--
imageSetFromStock :: Image
 -> StockId  -- ^ @stockId@ - a stock icon name
 -> IconSize -- ^ @size@ - a stock icon size
 -> IO ()
imageSetFromStock self stockId size =
  withUTFString stockId $ \stockIdPtr ->
  {# call gtk_image_set_from_stock #}
    self
    stockIdPtr
    ((fromIntegral . fromEnum) size)

#if GTK_CHECK_VERSION(2,6,0)
-- | See 'imageNewFromIconName' for details.
--
-- * Available since Gtk+ version 2.6
--
imageSetFromIconName :: GlibString string => Image
 -> string   -- ^ @iconName@ - an icon name
 -> IconSize -- ^ @size@ - an icon size
 -> IO ()
imageSetFromIconName self iconName size =
  withUTFString iconName $ \iconNamePtr ->
  {# call gtk_image_set_from_icon_name #}
    self
    iconNamePtr
    ((fromIntegral . fromEnum) size)

-- | Sets the pixel size to use for named icons. If the pixel size is set to a
-- @value \/= -1@, it is used instead of the icon size set by
-- 'imageSetFromIconName'.
--
-- * Available since Gtk+ version 2.6
--
imageSetPixelSize :: Image
 -> Int   -- ^ @pixelSize@ - the new pixel size
 -> IO ()
imageSetPixelSize self pixelSize =
  {# call gtk_image_set_pixel_size #}
    self
    (fromIntegral pixelSize)

-- | Gets the pixel size used for named icons.
--
-- * Available since Gtk+ version 2.6
--
imageGetPixelSize :: Image -> IO Int
imageGetPixelSize self =
  liftM fromIntegral $
  {# call gtk_image_get_pixel_size #}
    self
#endif

#if GTK_CHECK_VERSION(2,8,0)
-- | Resets the image to be empty.
--
-- * Available since Gtk+ version 2.8
--
imageClear :: Image -> IO ()
imageClear self =
  {# call gtk_image_clear #}
    self
#endif

--------------------
-- Attributes

-- | A 'Pixbuf' to display.
--
imagePixbuf :: PixbufClass pixbuf => ReadWriteAttr Image Pixbuf pixbuf
imagePixbuf = newAttrFromObjectProperty "pixbuf"
  {# call pure unsafe gdk_pixbuf_get_type #}


imageAnimation :: (PixbufClass pixbuf, PixbufAnimationClass animation)  => ReadWriteAttr Image animation pixbuf
imageAnimation = newAttrFromObjectProperty "pixbuf-animation"
  {# call pure unsafe gdk_pixbuf_get_type #}

#if GTK_MAJOR_VERSION < 3
-- | A 'Pixmap' to display.
--
imagePixmap :: PixmapClass pixmap => ReadWriteAttr Image Pixmap pixmap
imagePixmap = newAttrFromObjectProperty "pixmap"
  {# call pure unsafe gdk_pixmap_get_type #}

-- | Mask bitmap to use with 'Image' or 'Pixmap'.
--
imageMask :: PixmapClass pixmap => ReadWriteAttr Image Pixmap pixmap
imageMask = newAttrFromObjectProperty "mask"
  {# call pure unsafe gdk_pixmap_get_type #}
#endif

-- | A 'Image' to display.
--
imageImage :: ImageClass image => ReadWriteAttr Image Image image
imageImage = newAttrFromObjectProperty "image"
  {# call pure unsafe gtk_image_get_type #}

-- | Filename to load and display.
--
-- Default value: \"\"
--
imageFile :: GlibString string => Attr Image string
imageFile = newAttrFromStringProperty "file"

-- | Stock ID for a stock image to display.
--
-- Default value: \"\"
--
imageStock :: GlibString string => Attr Image string
imageStock = newAttrFromStringProperty "stock"

-- | Symbolic size to use for stock icon, icon set or named icon.
--
-- Allowed values: >= 0
--
-- Default value: 4
--
imageIconSize :: Attr Image Int
imageIconSize = newAttrFromIntProperty "icon-size"

#if GTK_CHECK_VERSION(2,6,0)
-- | The pixel-size property can be used to specify a fixed size overriding
-- the icon-size property for images of type 'ImageIconName'.
--
-- Allowed values: >= -1
--
-- Default value: -1
--
imagePixelSize :: Attr Image Int
imagePixelSize = newAttr
  imageGetPixelSize
  imageSetPixelSize
#endif

#if GTK_CHECK_VERSION(2,6,0)
-- | The name of the icon in the icon theme. If the icon theme is changed, the
-- image will be updated automatically.
--
-- Default value: \"\"
--
imageIconName :: GlibString string => Attr Image string
imageIconName = newAttrFromStringProperty "icon-name"
#endif

-- | The representation being used for image data.
--
-- Default value: 'ImageEmpty'
--
imageStorageType :: ReadAttr Image ImageType
imageStorageType = readAttrFromEnumProperty "storage-type"
  {# call pure unsafe gtk_image_type_get_type #}
