-- -*-haskell-*-
--  GIMP Toolkit (GTK) Pixbuf
--
--  Author : Vincenzo Ciancia, Axel Simon
--
--  Created: 26 March 2002
--
--  Version $Revision: 1.5 $ from $Date: 2005/06/02 00:47:39 $
--
--  Copyright (C) 2002-2005 Axel Simon, Vincenzo Ciancia
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
-- 'Pixbuf's are bitmap images in memory.
--
-- * A Pixbuf is used to represent images. It contains information
--   about the image's pixel data, its color space, bits per sample, width
--   and height, and the rowstride or number of bytes between rows.
--
-- * This module contains functions to scale and crop
--   'Pixbuf's and to scale and crop a 'Pixbuf' and 
--   compose the result with an existing image.
--
-- * 'Pixbuf's can be displayed on screen by either creating an 'Image' that
--   from the 'Pixbuf' or by rendering (part of) the 'Pixbuf' into a
--   vanilla widget like 'DrawWindow' using 'drawPixbuf'.
--
-- TODO
--
-- * if there is a portable way of modifying external arrays in Haskell do:
--	gdk_pixbuf_get_pixels, gdk_pixbuf_new_from_data, everything in
--	Inline data, 
--
-- * if anybody writes an image manipulation program, do the checker board
--   functions: gdk_pixbuf_composite_color_simple and
--   gdk_pixbuf_composite_color. Moreover, do: pixbuf_saturate_and_pixelate
--
-- * the animation functions
--
-- * pixbuf loader
--
-- * module interface
--
-- * rendering function for Bitmaps and Pixmaps when the latter are added
--
module Graphics.UI.Gtk.Gdk.Pixbuf (
  Pixbuf,
  PixbufClass,
  PixbufError(..),
  Colorspace(..),
  pixbufGetColorSpace,
  pixbufGetNChannels,
  pixbufGetHasAlpha,
  pixbufGetBitsPerSample,
  PixbufData,
  pixbufGetPixels,
  pixbufGetWidth,
  pixbufGetHeight,
  pixbufGetRowstride,
  pixbufGetOption,
  pixbufNewFromFile,
  ImageType,
  pixbufGetFormats,
  pixbufSave,
  pixbufNew,
  pixbufNewFromXPMData,
  InlineImage,
  pixbufNewFromInline,
  pixbufNewSubpixbuf,
  pixbufCopy,
  InterpType(..),
  pixbufScaleSimple,
  pixbufScale,
  pixbufComposite,
  pixbufAddAlpha,
  pixbufCopyArea,
  pixbufFill,
  pixbufGetFromDrawable
  ) where

import Monad (liftM)
import Control.Exception(bracket)
import Data.Bits        ((.|.), shiftL)
import Data.Ix
import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GObject
{#import Graphics.UI.Gtk.Types#}
import Graphics.UI.Gtk.General.Structs		(Rectangle(..))
import System.Glib.GError	(GError(..), GErrorClass(..), GErrorDomain,
				checkGError, checkGErrorWithCont)
import Graphics.UI.Gtk.Gdk.PixbufData ( PixbufData(PixbufData),
					insertBounds )


{#context prefix="gdk" #}

-- | Error codes for loading image files.
--
{#enum PixbufError {underscoreToCase} #}


-- | Enumerate all supported color spaces.
--
-- * Only RGB is supported right now.
--
{#enum Colorspace {underscoreToCase} #}

-- | Queries the color space of a pixbuf.
--
pixbufGetColorSpace :: Pixbuf -> IO Colorspace
pixbufGetColorSpace pb = liftM (toEnum . fromIntegral) $
  {#call unsafe pixbuf_get_colorspace#} pb

-- | Queries the number of colors for each pixel.
--
-- * This function returns 3 for an RGB image without alpha (transparency)
--   channel, 4 for an RGB image with alpha channel.
--
pixbufGetNChannels :: Pixbuf -> IO Int
pixbufGetNChannels pb = liftM fromIntegral $
  {#call unsafe pixbuf_get_n_channels#} pb

-- | Query if the image has an alpha channel.
--
-- * The alpha channel determines the opaqueness of the pixel.
--
pixbufGetHasAlpha :: Pixbuf -> IO Bool
pixbufGetHasAlpha pb =
  liftM toBool $ {#call unsafe pixbuf_get_has_alpha#} pb

-- | Queries the number of bits for each color.
--
-- * Each pixel is has a number of cannels for each pixel, each channel
--   has this many bits.
--
pixbufGetBitsPerSample :: Pixbuf -> IO Int
pixbufGetBitsPerSample pb = liftM fromIntegral $
  {#call unsafe pixbuf_get_bits_per_sample#} pb

-- | Retrieve the internal array of raw image data.
--
-- * Image data in a pixbuf is stored in memory in uncompressed,
--   packed format. Rows in the image are stored top to bottom, and in each
--   row pixels are stored from left to right. There may be padding at the
--   end of a row. The "rowstride" value of a pixbuf, as returned by
--   'pixbufGetRowstride', indicates the number of bytes between rows.
--
-- * The returned array is a flat representation of a three dimensional
--   array: x-coordiante, y-coordinate and several channels for each color.
--   The number of channels is usually 3 for plain RGB data or 4 for
--   RGB data with an alpha channel. To read or write a specific pixel
--   use the formula: @p = y * rowstride + x * nChannels@ for the pixel.
--   If the array contains bytes (or 'Word8's), @p+0@ is the red value,
--   @p+1@ green, @p+2@ blue and @p+3@ the alpha (transparency) channel
--   if present. If the alpha channel is present, the array can accessed
--   as an array over 'Word32' to modify a whole pixel at a time. See also
--   'pixbufGetBitsPerSample' and 'pixbufGetNChannels'.
--
-- * Calling this function without explicitly giving it a type will often
--   lead to a compiler error since the type parameter @e@ is underspecified.
--   If this happens the function can be explicitly typed:
--   @pbData <- (pixbufGetPixels pb :: IO (PixbufData Int Word8))@
--
-- * If modifying an image through Haskell\'s array interface is not
--   fast enough, it is possible to use 'unsafeRead' and
--   'unsafeWrite' from Data.Array.Base which have the same type signatures
--   as 'readArray' and 'writeArray'. Note that these are internal
--   functions that might change with GHC.
--
pixbufGetPixels :: (Ix i, Num i, Storable e) => Pixbuf -> IO (PixbufData i e)
pixbufGetPixels pb = do
  pixPtr_ <- {#call unsafe pixbuf_get_pixels#} pb
  chan <- pixbufGetNChannels pb
  bits <- pixbufGetBitsPerSample pb
  w <- pixbufGetWidth pb
  h <- pixbufGetHeight pb
  r <- pixbufGetRowstride pb
  let pixPtr = castPtr pixPtr_
  let bytes = (h-1)*r+w*((chan*bits+7) `div` 8)
  return (insertBounds bytes (PixbufData pb pixPtr undefined))

-- | Queries the width of this image.
--
pixbufGetWidth :: Pixbuf -> IO Int
pixbufGetWidth pb = liftM fromIntegral $
  {#call unsafe pixbuf_get_width#} pb

-- | Queries the height of this image.
--
pixbufGetHeight :: Pixbuf -> IO Int
pixbufGetHeight pb = liftM fromIntegral $
  {#call unsafe pixbuf_get_height#} pb

-- | Queries the rowstride of this image.
--
-- * Queries the rowstride of a pixbuf, which is the number of bytes between
--   rows. Use this value to caculate the offset to a certain row.
--
pixbufGetRowstride :: Pixbuf -> IO Int
pixbufGetRowstride pb = liftM fromIntegral $
  {#call unsafe pixbuf_get_rowstride#} pb

-- | Returns an attribut of an image.
--
-- * Looks up if some information was stored under the @key@ when
--   this image was saved.
--
pixbufGetOption :: Pixbuf -> String -> IO (Maybe String)
pixbufGetOption pb key = withUTFString key $ \strPtr -> do
  resPtr <- {#call unsafe pixbuf_get_option#} pb strPtr
  if (resPtr==nullPtr) then return Nothing else
    liftM Just $ peekUTFString resPtr

-- helper functions
pixbufErrorDomain :: GErrorDomain
pixbufErrorDomain = unsafePerformIO {#call unsafe pixbuf_error_quark#}

instance GErrorClass PixbufError where
  gerrorDomain _ = pixbufErrorDomain

handlePixbufError :: GError -> IO (PixbufError,String)
handlePixbufError (GError dom code msg)
  | dom == pixbufErrorDomain = return (toEnum code, msg)
  | otherwise                = fail msg


-- | Load an image synchonously.
--
-- * Use this function to load only small images as this call will block.
--
-- * The function will return @Left (err,msg)@ where @err@
--   is the error code and @msg@ is a human readable description
--   of the error. If an error occurs which is not captured by any of
--   those in 'PixbufError', an exception is thrown.
--
pixbufNewFromFile :: FilePath -> IO (Either (PixbufError,String) Pixbuf)
pixbufNewFromFile fname = 
  checkGErrorWithCont
    (\errPtrPtr -> 
     withUTFString fname $ \strPtr ->
#if defined (WIN32) && GTK_CHECK_VERSION(2,6,0)
     {#call unsafe pixbuf_new_from_file_utf8#}
#else
     {#call unsafe pixbuf_new_from_file#}
#endif
    strPtr errPtrPtr)
    (\gerror -> liftM Left $ handlePixbufError gerror)
    (\pbPtr -> liftM Right $ makeNewGObject mkPixbuf (return pbPtr))

-- | A string representing an image file format.
--
type ImageType = String

-- constant pixbufGetFormats A list of valid image file formats.
--
pixbufGetFormats :: [ImageType]

pixbufGetFormats = ["png","bmp","wbmp", "gif","ico","ani","jpeg","pnm",
		    "ras","tiff","xpm","xbm","tga"]

-- | Save an image to disk.
--
-- * The function takes a list of key - value pairs to specify
--   either how an image is saved or to actually save this additional
--   data with the image. JPEG images can be saved with a \"quality\"
--   parameter; its value should be in the range [0,100]. Text chunks
--   can be attached to PNG images by specifying parameters of the form
--   \"tEXt::key\", where key is an ASCII string of length 1-79.
--   The values are Unicode strings.
--
-- * The function returns @Nothing@ if writing was successful.
--   Otherwise the error code and a description is returned or,
--   if the error is not captured by one of the error codes in
--   'PixbufError', an exception is thrown.
--
pixbufSave :: Pixbuf -> FilePath -> ImageType -> [(String, String)] ->
	      IO (Maybe (PixbufError, String))
pixbufSave pb fname iType options =
  let (keys, values) = unzip options in
  let optLen = length keys in
  checkGError (\errPtrPtr ->
    withUTFString fname $ \fnPtr ->
    withUTFString iType $ \tyPtr ->
    allocaArray0 optLen $ \keysPtr ->
    allocaArray optLen $ \valuesPtr -> do
      keyPtrs <- mapM newUTFString keys
      valuePtrs <- mapM newUTFString values
      pokeArray keysPtr keyPtrs
      pokeArray valuesPtr valuePtrs
#if defined (WIN32) && GTK_CHECK_VERSION(2,6,5)
      {# call unsafe pixbuf_savev_utf8 #}
#else
      {# call unsafe pixbuf_savev #}
#endif
        pb fnPtr tyPtr keysPtr valuesPtr errPtrPtr
      mapM_ free keyPtrs
      mapM_ free valuePtrs
      return Nothing)
  (\gerror -> liftM Just $ handlePixbufError gerror)

-- | Create a new image in memory.
--
-- * Creates a new pixbuf structure and allocates a buffer for
--   it. Note that the buffer is not cleared initially.
--
-- * The boolean flag is true if the pixbuf should have an alpha
--   (transparency) channel. The next integer denotes the bits per
--   color sample, e.g. 8 bits per color for 2^24 colors. The last
--   two integers denote the width and height, respectively.
--
pixbufNew :: Colorspace -> Bool -> Int -> Int -> Int -> IO Pixbuf
pixbufNew colorspace hasAlpha bitsPerSample width height =
  makeNewGObject mkPixbuf $ 
    {#call pixbuf_new#} ((fromIntegral . fromEnum) colorspace)
      (fromBool hasAlpha) (fromIntegral bitsPerSample) (fromIntegral width)
      (fromIntegral height)

-- | Create a new image from a String.
--
-- * Creates a new pixbuf from a string description.
--
pixbufNewFromXPMData :: [String] -> IO Pixbuf
pixbufNewFromXPMData s =
  bracket (mapM newUTFString s) (mapM free) $ \strPtrs ->
    withArray0 nullPtr strPtrs $ \strsPtr ->
      makeNewGObject mkPixbuf $ {#call pixbuf_new_from_xpm_data#} strsPtr

-- | A dymmy type for inline picture data.
--
-- * This dummy type is used to declare pointers to image data
--   that is embedded in the executable. See
--   'pixbufNewFromInline' for an example.
--
data InlineImage = InlineImage

-- | Create a new image from a static pointer.
--
-- * Like 'pixbufNewFromXPMData', this function allows to
--   include images in the final binary program. The method used by this
--   function uses a binary representation and therefore needs less space
--   in the final executable. Save the image you want to include as
--   @png@ and run: 
--   @echo #include \"my_image.h\" > my_image.c@
--   gdk-pixbuf-csource --raw --extern --name=my_image myimage.png >> my_image.c
--    on it. Write a header file @my_image.h@ containing:
--   @#include <gdk\gdk.h>
--   extern guint8 my_image\[\];@
--    and save it in the current directory.
--   The created file can be compiled with: 
--   @cc -c my_image.c \`pkg-config --cflags gdk-2.0\`@
--    into an object file which must be linked into your Haskell program by
--   specifying @my_image.o@ and @\"-#include my_image.h\"@ on
--   the command line of GHC.
--   Within you application you delcare a pointer to this image: 
--   @foreign label \"my_image\" myImage :: Ptr InlineImage@
--    Calling 'pixbufNewFromInline' with this pointer will
--   return the image in the object file. Creating the C file with
--   the @--raw@ flag will result in a non-compressed image in the
--   object file. The advantage is that the picture will not be
--   copied when this function is called.
--
--
pixbufNewFromInline :: Ptr InlineImage -> IO Pixbuf
pixbufNewFromInline iPtr = alloca $ \errPtrPtr -> do
  pbPtr <- {#call unsafe pixbuf_new_from_inline#} (-1) (castPtr iPtr)
    (fromBool False) (castPtr errPtrPtr)
  if pbPtr/=nullPtr then makeNewGObject mkPixbuf (return pbPtr)
    else do
      errPtr <- peek errPtrPtr
      (GError dom code msg) <- peek errPtr
      error msg

-- | Create a restricted view of an image.
--
-- * This function returns a 'Pixbuf' object which shares
--   the image of the original one but only shows a part of it.
--   Modifying either buffer will affect the other.
--
-- * This function throw an exception if the requested bounds are invalid.
--
pixbufNewSubpixbuf :: Pixbuf -> Int -> Int -> Int -> Int -> IO Pixbuf
pixbufNewSubpixbuf pb srcX srcY height width =
  makeNewGObject mkPixbuf $ do
    pbPtr <- {#call unsafe pixbuf_new_subpixbuf#} pb
      (fromIntegral srcX) (fromIntegral srcY)
      (fromIntegral height) (fromIntegral width)
    if pbPtr==nullPtr then error "pixbufNewSubpixbuf: invalid bounds"
      else return pbPtr

-- | Create a deep copy of an image.
--
pixbufCopy :: Pixbuf -> IO Pixbuf
pixbufCopy pb = makeNewGObject mkPixbuf $ {#call unsafe pixbuf_copy#} pb


-- | How an image is scaled.
--
-- [@InterpNearest@] Nearest neighbor sampling; this is the
--   fastest and lowest quality mode. Quality is normally unacceptable when
--   scaling down, but may be OK when scaling up.
--
-- [@InterpTiles@] This is an accurate simulation of the
--   PostScript image operator without any interpolation enabled. Each
--   pixel is rendered as a tiny parallelogram of solid color, the edges of
--   which are implemented with antialiasing. It resembles nearest neighbor
--   for enlargement, and bilinear for reduction.
--
-- [@InterpBilinear@] Best quality\/speed balance; use this
--   mode by default. Bilinear interpolation. For enlargement, it is
--   equivalent to point-sampling the ideal bilinear-interpolated
--   image. For reduction, it is equivalent to laying down small tiles and
--   integrating over the coverage area.
--
-- [@InterpHyper@] This is the slowest and highest quality
--   reconstruction function. It is derived from the hyperbolic filters in
--   Wolberg's \"Digital Image Warping\", and is formally defined as the
--   hyperbolic-filter sampling the ideal hyperbolic-filter interpolated
--   image (the filter is designed to be idempotent for 1:1 pixel mapping).
--
{#enum InterpType {underscoreToCase} #}

-- | Scale an image.
--
-- * Creates a new 'GdkPixbuf' containing a copy of 
--   @src@ scaled to the given measures. Leaves @src@
--   unaffected. 
--
-- * @interp@ affects the quality and speed of the scaling function.
--   'InterpNearest' is the fastest option but yields very poor quality
--   when scaling down. 'InterpBilinear' is a good trade-off between
--   speed and quality and should thus be used as a default.
--
pixbufScaleSimple :: Pixbuf -> Int -> Int -> InterpType -> IO Pixbuf
pixbufScaleSimple pb width height interp =
    makeNewGObject mkPixbuf $ liftM castPtr $ 
	{#call pixbuf_scale_simple#} (toPixbuf pb) 
	(fromIntegral width) (fromIntegral height)
	(fromIntegral $ fromEnum interp)

-- | Copy a scaled image part to another image.
--
-- * This function is the generic version of 'pixbufScaleSimple'.
--   It scales @src@ by @scaleX@ and @scaleY@ and
--   translate the image by @offsetX@ and @offsetY@. Whatever
--   is in the intersection with the rectangle @destX@,
--   @destY@, @destWidth@, @destHeight@ will be
--   rendered into @dest@.
--
-- * The rectangle in the destination is simply overwritten. Use
--   'pixbufComposite' if you need to blend the source
--   image onto the destination.
--
pixbufScale :: Pixbuf -> Pixbuf -> Int -> Int -> Int -> Int ->
	       Double -> Double -> Double -> Double -> InterpType -> IO ()
pixbufScale src dest destX destY destWidth destHeight offsetX offsetY
            scaleX scaleY interp = {#call unsafe pixbuf_scale#} src dest
  (fromIntegral destX) (fromIntegral destY) (fromIntegral destHeight)
  (fromIntegral destWidth) (realToFrac offsetX) (realToFrac offsetY)
  (realToFrac scaleX) (realToFrac scaleY)
  ((fromIntegral . fromEnum) interp)

-- | Blend a scaled image part onto another image.
--
-- * This function is similar to 'pixbufScale' but allows the
--   original image to \"shine through\". The @alpha@ value determines
--   how opaque the source image is. Passing @0@ is
--   equivalent to not calling this function at all, passing
--   @255@ has the
--   same effect as calling 'pixbufScale'.
--
pixbufComposite :: Pixbuf -> Pixbuf -> Int -> Int -> Int -> Int ->
		   Double -> Double -> Double -> Double -> InterpType ->
	       	   Word8 -> IO ()
pixbufComposite src dest destX destY destWidth destHeight
  offsetX offsetY scaleX scaleY interp alpha =
  {#call unsafe pixbuf_composite#} src dest
  (fromIntegral destX) (fromIntegral destY) (fromIntegral destHeight)
  (fromIntegral destWidth) (realToFrac offsetX) (realToFrac offsetY)
  (realToFrac scaleX) (realToFrac scaleY)
  ((fromIntegral . fromEnum) interp) (fromIntegral alpha)

-- | Add an opacity layer to the 'Pixbuf'.
--
-- * This function returns a copy of the given @src@
--   'Pixbuf', leaving @src@ unmodified.
--   The new 'Pixbuf' has an alpha (opacity)
--   channel which defaults to @255@ (fully opaque pixels)
--   unless @src@ already had an alpha channel in which case
--   the original values are kept.
--   Passing in a color triple @(r,g,b)@ makes all
--   pixels that have this color fully transparent
--   (opacity of @0@). The pixel color itself remains unchanged
--   during this substitution.
--
pixbufAddAlpha :: Pixbuf -> Maybe (Word8, Word8, Word8) -> IO Pixbuf
pixbufAddAlpha pb Nothing = makeNewGObject mkPixbuf $
  {#call unsafe pixbuf_add_alpha#} pb (fromBool False) 0 0 0
pixbufAddAlpha pb (Just (r,g,b)) = makeNewGObject mkPixbuf $
  {#call unsafe pixbuf_add_alpha#} pb (fromBool True)
    (fromIntegral r) (fromIntegral g) (fromIntegral b)

-- | Copy a rectangular portion into another
-- 'Pixbuf'.
--
-- * The source 'Pixbuf' remains unchanged. Converion between
--   different formats is done automatically.
--
pixbufCopyArea :: Pixbuf -> Int -> Int -> Int -> Int ->
		  Pixbuf -> Int -> Int -> IO ()
pixbufCopyArea src srcX srcY srcWidth srcHeight dest destX destY =
  {#call unsafe pixbuf_copy_area#} src (fromIntegral srcX)
    (fromIntegral srcY) (fromIntegral srcHeight) (fromIntegral srcWidth)
    dest (fromIntegral destX) (fromIntegral destY)

-- | Fills a 'Pixbuf' with a color.
--
-- * The passed-in color is a quadruple consisting of the red, green, blue
--   and alpha component of the pixel. If the 'Pixbuf' does not
--   have an alpha channel, the alpha value is ignored.
--
pixbufFill :: Pixbuf -> Word8 -> Word8 -> Word8 -> Word8 -> IO ()
pixbufFill pb red green blue alpha = {#call unsafe pixbuf_fill#} pb
  ((fromIntegral red)   `shiftL` 24 .|.
   (fromIntegral green) `shiftL` 16 .|.
   (fromIntegral blue)  `shiftL`  8 .|.
   (fromIntegral alpha))

-- | Take a screenshot of a 'Drawable'.
--
-- * This function creates a 'Pixbuf' and fills it with the image
--   currently in the 'Drawable' (which might be invalid if the
--   window is obscured or minimized). Note that this transfers data from
--   the server to the client on X Windows.
--
-- * This function will return a 'Pixbuf' with no alpha channel
--   containing the part of the 'Drawable' specified by the
--   rectangle. The function will return @Nothing@ if the window
--   is not currently visible.
--
pixbufGetFromDrawable :: DrawableClass d => d -> Rectangle -> IO (Maybe Pixbuf)
pixbufGetFromDrawable d (Rectangle x y width height) = do
  pbPtr <- {#call unsafe pixbuf_get_from_drawable#} 
    (mkPixbuf nullForeignPtr) (toDrawable d) (mkColormap nullForeignPtr)
    (fromIntegral x) (fromIntegral y) 0 0
    (fromIntegral width) (fromIntegral height)
  if pbPtr==nullPtr then return Nothing else liftM Just $
    makeNewGObject mkPixbuf (return pbPtr)
