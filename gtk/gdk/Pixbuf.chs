--  -*-haskell-*-
--  GIMP Toolkit (GTK) @Pixbuf@
--
--  Author : Vincenzo Ciancia, Axel Simon
--  Created: 26 March 2002
--
--  Version $Revision: 1.1 $ from $Date: 2003/05/16 18:45:23 $
--
--  Copyright (c) 2002 Axel Simon
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Library General Public
--  License as published by the Free Software Foundation; either
--  version 2 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Library General Public License for more details.
--
-- @description@ --------------------------------------------------------------
--
--  @ref date Pixbuf@s are bitmap images in memory.
--
-- @documentation@ ------------------------------------------------------------
--
-- * A Pixbuf is used to represent images. It contains information
--   about the image's pixel data, its color space, bits per sample, width
--   and height, and the rowstride or number of bytes between rows.
--
-- * This module contains functions to scale and crop @ref data
--   Pixbuf@s and to scale and crop a @ref data Pixbuf@ and compose the
--   result with an existing image.
--
-- @todo@ ---------------------------------------------------------------------
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
module Pixbuf(
  Pixbuf,
  PixbufClass,
  PixbufError(..),
  Colorspace(..),
  AlphaMode,
  pixbufGetColorSpace,
  pixbufGetNChannels,
  pixbufGetHasAlpha,
  pixbufGetBitsPerSample,
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

import Foreign
{#import Hierarchy#}
import GObject
import Monad
import UTFCForeign
import Structs		(GError(..), GQuark, nullForeignPtr, Rectangle(..))
import LocalData	(unsafePerformIO)
import Exception	(bracket)
import LocalData	((.|.), shiftL)

{#context prefix="gdk" #}

-- @data PixbufError@ Error codes for loading image files.
--
{#enum PixbufError {underscoreToCase} #}


-- @data Colorspace@ Enumerate all supported color spaces.
--
-- * Only RGB is supported right now.
--
{#enum Colorspace {underscoreToCase} #}

-- @data AlphaMode@ Specify how the alpha channel should be used.
--
-- * These values can be passed to @method
--   pixbufRenderToDrawableAlpha@ to control how the alpha chanel of an
--   image should be handled. This function can create a bilevel clipping
--   mask (black and white) and use it while painting the image. In the
--   future, when the X Window System gets an alpha channel extension, it
--   will be possible to do full alpha compositing onto arbitrary
--   drawables. For now both cases fall back to a bilevel clipping mask.
{#enum PixbufAlphaMode as AlphaMode {underscoreToCase} #}

-- @method pixbufGetColorSpace@ Queries the color space of a pixbuf.
--
pixbufGetColorSpace :: Pixbuf -> IO Colorspace
pixbufGetColorSpace pb = liftM (toEnum . fromIntegral) $
  {#call unsafe pixbuf_get_colorspace#} pb

-- @method pixbufGetNChannels@ Queries the number of colors for each pixel.
--
pixbufGetNChannels :: Pixbuf -> IO Int
pixbufGetNChannels pb = liftM fromIntegral $
  {#call unsafe pixbuf_get_n_channels#} pb

-- @method pixbufGetHasAlpha@ Query if the image has an alpha channel.
--
-- * The alpha channel determines the opaqueness of the pixel.
--
pixbufGetHasAlpha :: Pixbuf -> IO Bool
pixbufGetHasAlpha pb =
  liftM toBool $ {#call unsafe pixbuf_get_has_alpha#} pb

-- @method pixbufGetBitsPerSample@ Queries the number of bits for each color.
--
-- * Each pixel is has a number of cannels for each pixel, each channel
--   has this many bits.
--
pixbufGetBitsPerSample :: Pixbuf -> IO Int
pixbufGetBitsPerSample pb = liftM fromIntegral $
  {#call unsafe pixbuf_get_bits_per_sample#} pb

-- @method pixbufGetWidth@ Queries the width of this image.
--
pixbufGetWidth :: Pixbuf -> IO Int
pixbufGetWidth pb = liftM fromIntegral $
  {#call unsafe pixbuf_get_width#} pb

-- @method pixbufGetHeight@ Queries the height of this image.
--
pixbufGetHeight :: Pixbuf -> IO Int
pixbufGetHeight pb = liftM fromIntegral $
  {#call unsafe pixbuf_get_height#} pb

-- @method pixbufGetRowstride@ Queries the rowstride of this image.
--
-- * Queries the rowstride of a pixbuf, which is the number of bytes between
--   rows. Use this value to caculate the offset to a certain row.
--
pixbufGetRowstride :: Pixbuf -> IO Int
pixbufGetRowstride pb = liftM fromIntegral $
  {#call unsafe pixbuf_get_rowstride#} pb

-- @method pixbufGetOption@ Returns an attribut of an image.
--
-- * Looks up if some information was stored under the @ref arg key@ when
--   this image was saved.
--
pixbufGetOption :: Pixbuf -> String -> IO (Maybe String)
pixbufGetOption pb key = withCString key $ \strPtr -> do
  resPtr <- {#call unsafe pixbuf_get_option#} pb strPtr
  if (resPtr==nullPtr) then return Nothing else
    liftM Just $ peekCString resPtr

-- pixbufErrorDomain -- helper function
pixbufErrorDomain :: GQuark
pixbufErrorDomain = unsafePerformIO {#call unsafe pixbuf_error_quark#}

-- @constructor pixbufNewFromFile@ Load an image synchonously.
--
-- * Use this function to load only small images as this call will block.
--
-- * The function will return @literal Left (err,msg)@ where @ref arg err@
--   is the error code and @ref arg msg@ is a human readable description
--   of the error. If an error occurs which is not captured by any of
--   those in @ref data PixbufError@, an exception is thrown.
--
pixbufNewFromFile :: FilePath -> IO (Either (PixbufError,String) Pixbuf)
pixbufNewFromFile fname = withCString fname $ \strPtr ->
  alloca $ \errPtrPtr -> do
  pbPtr <- {#call unsafe pixbuf_new_from_file#} strPtr (castPtr errPtrPtr)
  if pbPtr/=nullPtr then liftM Right $ makeNewGObject mkPixbuf (return pbPtr)
    else do
      errPtr <- peek errPtrPtr
      (GError dom code msg) <- peek errPtr
      if dom==pixbufErrorDomain then
        return (Left (toEnum (fromIntegral code), msg))
       else
	error msg

-- @type ImageType@ A string representing a image file format.
--
type ImageType = String

-- @const pixbufGetFormats@ A list of valid image file formats.
--
pixbufGetFormats :: [ImageType]

pixbufGetFormats = ["png","bmp","wbmp", "gif","ico","ani","jpeg","pnm",
		    "ras","tiff","xpm","xbm","tga"]

-- @method pixbufSave@ Save an image to disk.
--
-- * The function takes a list of key - value pairs to specify
--   either how an image is saved or to actually save this additional
--   data with the image. JPEG images can be saved with a "quality"
--   parameter; its value should be in the range [0,100]. Text chunks
--   can be attached to PNG images by specifying parameters of the form
--   "tEXt::key", where key is an ASCII string of length 1-79.
--   The values are Unicode strings.
--
-- * The function returns @literal Nothing@ if writing was successful.
--   Otherwise the error code and a description is returned or,
--   if the error is not captured by one of the error codes in
--   @ref data PixbufError@, an exception is thrown.
--
pixbufSave :: Pixbuf -> FilePath -> ImageType -> [(String, String)] ->
	      IO (Maybe (PixbufError, String))
pixbufSave pb fname iType options =
  let (keys, values) = unzip options in
  let optLen = length keys in
  withCString fname $ \fnPtr ->
  withCString iType $ \tyPtr ->
  allocaArray0 optLen $ \keysPtr ->
  allocaArray optLen $ \valuesPtr ->
  alloca $ \errPtrPtr -> do
    keyPtrs <- mapM newCString keys
    valuePtrs <- mapM newCString values
    pokeArray keysPtr keyPtrs
    pokeArray valuesPtr valuePtrs
    res <- {#call unsafe pixbuf_savev#} pb fnPtr tyPtr keysPtr valuesPtr
					(castPtr errPtrPtr)
    mapM_ free keyPtrs
    mapM_ free valuePtrs
    if not (toBool res) then return Nothing else do
      errPtr <- peek errPtrPtr
      (GError dom code msg) <- peek errPtr
      if dom==pixbufErrorDomain then
        return (Just (toEnum (fromIntegral code), msg))
       else
	error msg

-- @constructor pixbufNew@ Create a new image in memory.
--
-- * Creates a new pixbuf structure and allocates a buffer for
--   it. Note that the buffer is not cleared initially.
--
pixbufNew :: Colorspace -> Bool -> Int -> Int -> Int -> IO Pixbuf
pixbufNew colorspace hasAlpha bitsPerSample width height =
  makeNewGObject mkPixbuf $ 
    {#call pixbuf_new#} ((fromIntegral . fromEnum) colorspace)
      (fromBool hasAlpha) (fromIntegral bitsPerSample) (fromIntegral width)
      (fromIntegral height)

-- @constructor pixbufNewFromXPMData@ Create a new image from a String.
--
-- * Creates a new pixbuf from a string description.
--
pixbufNewFromXPMData :: [String] -> IO Pixbuf
pixbufNewFromXPMData s =
  bracket (mapM newCString s) (mapM free) $ \strPtrs ->
    withArray0 nullPtr strPtrs $ \strsPtr ->
      makeNewGObject mkPixbuf $ {#call pixbuf_new_from_xpm_data#} strsPtr

-- @data InlineImage@ A dymmy type for inline picture data.
--
-- * This dummy type is used to declare pointers to image data
--   that is embedded in the executable. See
--   @ref constructor pixbufNewFromInline@ for an example.
--
data InlineImage = InlineImage

-- @constructor pixbufNewFromInline@ Create a new image from a static pointer.
--
-- * Like @ref constructor pixbufNewFromXPMData@, this function allows to
--   include images in the final binary program. The method used by this
--   function uses a binary representation and therefore needs less space
--   in the final executable. Save the image you want to include as
--   @literal png@ and run @verbatim
--   gdk-pixbuf-csource --raw --name=my_image myimage.png > my_image.c
--   @ it. The created file can be compiled with @verbatim
--   cc -c image.c -include <gdk/gdk.h> `pkg-config --cflags gdk-2.0`
--   @ into an object file which must be linked into your Haskell program.
--   Within you application you delcare a pointer to this image: @verbatim
--   foreign import ccall "my_image" myImage :: Ptr InlineImage
--   @ Call @ref constructor pixbufNewFromInline@ with this pointer will
--   return the image in the object file. Creating the C file with
--   the @literal --raw@ flag will result in a non-compressed image in the
--   object file. The advantage is that the picture does not need to be
--   copied (set @ref arg copyPixels@ to @literal False@).
--
--
pixbufNewFromInline :: Ptr InlineImage -> Bool -> IO Pixbuf
pixbufNewFromInline iPtr copyPixels = alloca $ \errPtrPtr -> do
  pbPtr <- {#call unsafe pixbuf_new_from_inline#} (-1) (castPtr iPtr)
    (fromBool copyPixels) (castPtr errPtrPtr)
  if pbPtr/=nullPtr then makeNewGObject mkPixbuf (return pbPtr)
    else do
      errPtr <- peek errPtrPtr
      (GError dom code msg) <- peek errPtr
      error msg

-- @method pixbufNewSubpixbuf@ Create a restricted view of an image.
--
-- * This function returns a @ref data Pixbuf@ object which shares
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

-- @constructor pixbufCopy@ Create a deep copy of an image.
--
pixbufCopy :: Pixbuf -> IO Pixbuf
pixbufCopy pb = makeNewGObject mkPixbuf $ {#call unsafe pixbuf_copy#} pb


-- @data InterpType@ How an image is scaled.
--
--
-- * @ref type InterpNearest@ Nearest neighbor sampling; this is the
--   fastest and lowest quality mode. Quality is normally unacceptable when
--   scaling down, but may be OK when scaling up.
--
-- * @ref type InterpTiles@ This is an accurate simulation of the
--   PostScript image operator without any interpolation enabled. Each
--   pixel is rendered as a tiny parallelogram of solid color, the edges of
--   which are implemented with antialiasing. It resembles nearest neighbor
--   for enlargement, and bilinear for reduction.
--
-- * @ref type InterpBilinear@ Best quality/speed balance; use this
--   mode by default. Bilinear interpolation. For enlargement, it is
--   equivalent to point-sampling the ideal bilinear-interpolated
--   image. For reduction, it is equivalent to laying down small tiles and
--   integrating over the coverage area.
--
-- * @ref type InterpHyper@ This is the slowest and highest quality
--   reconstruction function. It is derived from the hyperbolic filters in
--   Wolberg's "Digital Image Warping", and is formally defined as the
--   hyperbolic-filter sampling the ideal hyperbolic-filter interpolated
--   image (the filter is designed to be idempotent for 1:1 pixel mapping).
--
{#enum InterpType {underscoreToCase} #}

-- @method pixbufScaleSimple@ Scale an image.
--
-- * Creates a new @ref data GdkPixbuf@ containing a copy of 
--   @ref arg src@ scaled to the given measures. Leaves @ref arg src@
--   unaffected. 
--
-- * @ref arg interp@ affects the quality and speed of the scaling function.
--   @ref type InterpNearest@ is the fastest option but yields very poor
--   quality when scaling down. @ref type InterpBilinear@ is a good
--   trade-off between speed and quality and should thus be used as a
--   default.
--
pixbufScaleSimple :: Pixbuf -> Int -> Int -> InterpType -> IO Pixbuf
pixbufScaleSimple pb width height interp =
    makeNewGObject mkPixbuf $ liftM castPtr $ 
	{#call pixbuf_scale_simple#} (toPixbuf pb) 
	(fromIntegral width) (fromIntegral height)
	(fromIntegral $ fromEnum interp)

-- @method pixbufScale@ Copy a scaled image part to another image.
--
-- * This function is the generic version of @ref method pixbufScaleSimple@.
--   It scales @ref arg src@ by @ref arg scaleX@ and @ref arg scaleY@ and
--   translate the image by @ref arg offsetX@ and @ref offsetY@. Whatever
--   is in the intersection with the rectangle @ref arg destX@,
--   @ref arg destY@, @ref arg destWidth@, @ref arg destHeight@ will be
--   rendered into @ref arg dest@.
--
-- * The rectangle in the destination is simply overwritten. Use
--   @ref method pixbufComposite@ if you need to blend the source
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

-- @method pixbufComposite@ Blend a scaled image part onto another image.
--
-- * This function is similar to @ref method pixbufScale@ but allows the
--   original image to "shine through". The @ref arg alpha@ value determines
--   how opaque the source image is. Passing @literal 0@ is
--   equivalent to not calling this function at all, passing
--   @literal 255@ has the
--   same effect as calling @ref method pixbufScale@.
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

-- @method pixbufAddAlpha@ Add an opacity layer to the @ref data Pixbuf@.
--
-- * This function returns a copy of the given @ref arg src@
--   @ref data Pixbuf@, leaving @ref arg src@ unmodified.
--   The new @ref data Pixbuf@ has an alpha (opacity)
--   channel which defaults to @literal 255@ (fully opaque pixels)
--   unless @ref arg src@ already had an alpha channel in which case
--   the original values are kept.
--   Passing in a color triple @literal (r,g,b)@ makes all
--   pixels that have this color fully transparent
--   (opacity of @literal 0@). The pixel color itself remains unchanged
--   during this substitution.
--
pixbufAddAlpha :: Pixbuf -> Maybe (Word8, Word8, Word8) -> IO Pixbuf
pixbufAddAlpha pb Nothing = makeNewGObject mkPixbuf $
  {#call unsafe pixbuf_add_alpha#} pb (fromBool False) 0 0 0
pixbufAddAlpha pb (Just (r,g,b)) = makeNewGObject mkPixbuf $
  {#call unsafe pixbuf_add_alpha#} pb (fromBool True)
    (fromIntegral r) (fromIntegral g) (fromIntegral b)

-- @method pixbufCopyArea@ Copy a rectangular portion into another
-- @ref data Pixbuf@.
--
-- * The source @ref data Pixbuf@ remains unchanged. Converion between
--   different formats is done automatically.
--
pixbufCopyArea :: Pixbuf -> Int -> Int -> Int -> Int ->
		  Pixbuf -> Int -> Int -> IO ()
pixbufCopyArea src srcX srcY srcWidth srcHeight dest destX destY =
  {#call unsafe pixbuf_copy_area#} src (fromIntegral srcX)
    (fromIntegral srcY) (fromIntegral srcHeight) (fromIntegral srcWidth)
    dest (fromIntegral destX) (fromIntegral destY)

-- @method pixbufFill@ Fills a @ref data Pixbuf@ with a color.
--
-- * The passed-in color is a quadruple consisting of the red, green, blue
--   and alpha component of the pixel. If the @ref data Pixbuf@ does not
--   have an alpha channel, the alpha value is ignored.
--
pixbufFill :: Pixbuf -> Word8 -> Word8 -> Word8 -> Word8 -> IO ()
pixbufFill pb red green blue alpha = {#call unsafe pixbuf_fill#} pb
  ((fromIntegral red)   `shiftL` 24 .|.
   (fromIntegral green) `shiftL` 16 .|.
   (fromIntegral blue)  `shiftL`  8 .|.
   (fromIntegral alpha))

-- @method pixbufGetFromDrawable@ Take a screenshot of a @ref data Drawable@.
--
-- * This function creates a @ref data Pixbuf@ and fills it with the image
--   currently in the @ref data Drawable@ (which might be invalid if the
--   window is obscured or minimized). Note that this transfers data from
--   the server to the client on X Windows.
--
-- * This function will return a @ref data Pixbuf@ with no alpha channel
--   containing the part of the @ref data Drawable@ specified by the
--   rectangle. The function will return @literal Nothing@ if the window
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