{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget PrintSettings
--
--  Author : Andy Stewart
--
--  Created: 28 Mar 2010
--
--  Copyright (C) 2010 Andy Stewart
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
-- Stores print settings
--
-- * Module available since Gtk+ version 2.10
--
module Graphics.UI.Gtk.Printing.PrintSettings (

-- * Detail
--
-- | A 'PrintSettings' object represents the settings of a print dialog in a
-- system-independent way. The main use for this object is that once you\'ve
-- printed you can get a settings object that represents the settings the user
-- chose, and the next time you print you can pass that object in so that the
-- user doesn't have to re-set all his settings.
--
-- Its also possible to enumerate the settings so that you can easily save
-- the settings for the next time your app runs, or even store them in a
-- document. The predefined keys try to use shared values as much as possible
-- so that moving such a document between systems still works.
--
-- Printing support was added in Gtk+ 2.10.
--

-- * Class Hierarchy
--
-- |
-- @
-- |  'GObject'
-- |   +----PrintSettings
-- @

#if GTK_CHECK_VERSION(2,10,0)
-- * Types
  PrintSettings,
  PrintSettingsClass,
  castToPrintSettings,
  toPrintSettings,
  -- PageRange,

-- * Enums
  PageOrientation(..),
#if GTK_CHECK_VERSION(2,14,0)
  NumberUpLayout(..),
#endif
  PrintQuality(..),
  PrintDuplex(..),
  PrintPages(..),
  PageSet(..),

-- * Constructors
  printSettingsNew,
#if GTK_CHECK_VERSION(2,12,0)
  printSettingsNewFromFile,
#endif

-- * Methods
  printSettingsCopy,
  printSettingsHasKey,
  printSettingsGet,
  printSettingsSet,
  printSettingsUnset,
  printSettingsForeach,
  printSettingsGetBool,
  printSettingsSetBool,
  printSettingsGetDouble,
  printSettingsGetDoubleWithDefault,
  printSettingsSetDouble,
  printSettingsGetLength,
  printSettingsSetLength,
  printSettingsGetInt,
  printSettingsGetIntWithDefault,
  printSettingsSetInt,
  printSettingsGetPaperWidth,
  printSettingsSetPaperWidth,
  printSettingsGetPaperHeight,
  printSettingsSetPaperHeight,
#if GTK_CHECK_VERSION(2,16,0)
  printSettingsSetResolutionXy,
  printSettingsGetResolutionX,
  printSettingsGetResolutionY,
#endif
  -- printSettingsGetPageRanges,
  -- printSettingsSetPageRanges,
#if GTK_CHECK_VERSION(2,14,0)
  printSettingsLoadFile,
#endif
#if GTK_CHECK_VERSION(2,12,0)
  printSettingsToFile,
#endif

-- * Attributes
  printSettingsPrinter,
  printSettingsOrientation,
  printSettingsPaperSize,
  printSettingsUseColor,
  printSettingsCollate,
  printSettingsReverse,
  printSettingsDuplex,
  printSettingsQuality,
  printSettingsNCopies,
  printSettingsNumberUp,
  printSettingsResolution,
  printSettingsScale,
  printSettingsPrintPages,
  printSettingsPageSet,
  printSettingsDefaultSource,
  printSettingsMediaType,
  printSettingsDither,
  printSettingsFinishings,
  printSettingsOutputBin,
#if GTK_CHECK_VERSION(2,14,0)
  printSettingsNumberUpLayout,
#endif
#if GTK_CHECK_VERSION(2,16,0)
  printSettingsPrinterLpi,
#endif
#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.Attributes
import System.Glib.UTFString
import System.Glib.GError
{#import Graphics.UI.Gtk.Types#}
#if GTK_CHECK_VERSION(2,10,0)
import Graphics.UI.Gtk.Printing.PaperSize (PaperSize(PaperSize), mkPaperSize, Unit(..))
#endif

{# context lib="gtk" prefix="gtk" #}

#if GTK_CHECK_VERSION(2,10,0)
--------------------
-- Enums
{#enum PageOrientation {underscoreToCase} deriving (Bounded,Eq,Show)#}

{#enum PrintQuality {underscoreToCase} deriving (Bounded,Eq,Show)#}

{#enum PrintDuplex {underscoreToCase} deriving (Bounded,Eq,Show)#}

{#enum PrintPages {underscoreToCase} deriving (Bounded,Eq,Show)#}

{#enum PageSet {underscoreToCase} deriving (Bounded,Eq,Show)#}

#if GTK_CHECK_VERSION(2,14,0)
-- | Used to determine the layout of pages on a sheet when printing multiple pages per sheet.
{#enum NumberUpLayout {underscoreToCase} deriving (Bounded,Eq,Show)#}
#endif

--------------------
-- Constructors

-- | Creates a new 'PrintSettings' object.
--
printSettingsNew :: IO PrintSettings
printSettingsNew =
  wrapNewGObject mkPrintSettings $
  {# call gtk_print_settings_new #}

#if GTK_CHECK_VERSION(2,12,0)
-- | Reads the print settings from @fileName@. Returns a new 'PrintSettings'
-- object with the restored settings.
--
-- * Available since Gtk+ version 2.12
--
printSettingsNewFromFile :: GlibFilePath fp
 => fp -- ^ @fileName@ - the filename to read the settings from
 -> IO PrintSettings
printSettingsNewFromFile fileName =
  wrapNewGObject mkPrintSettings $
  propagateGError $ \errorPtr ->
  withUTFFilePath fileName $ \fileNamePtr ->
  {# call gtk_print_settings_new_from_file #}
        fileNamePtr
        errorPtr

#endif

--------------------
-- Methods

-- | Copies a 'PrintSettings' object.
--
printSettingsCopy :: PrintSettingsClass self => self
 -> IO PrintSettings -- ^ returns a newly allocated copy of @other@
printSettingsCopy self =
  wrapNewGObject mkPrintSettings $
  {# call gtk_print_settings_copy #}
    (toPrintSettings self)

-- | Returns @True@, if a value is associated with @key@.
--
printSettingsHasKey :: (PrintSettingsClass self, GlibString string) => self
 -> string  -- ^ @key@ - a key
 -> IO Bool -- ^ returns @True@, if @key@ has a value
printSettingsHasKey self key =
  liftM toBool $
  withUTFString key $ \keyPtr ->
  {# call gtk_print_settings_has_key #}
    (toPrintSettings self)
    keyPtr

-- | Looks up the string value associated with @key@.
--
printSettingsGet :: (PrintSettingsClass self, GlibString string) => self
 -> string    -- ^ @key@ - a key
 -> IO string -- ^ returns the string value for @key@
printSettingsGet self key =
  withUTFString key $ \keyPtr ->
  {# call gtk_print_settings_get #}
    (toPrintSettings self)
    keyPtr
  >>= peekUTFString

-- | Associates @value@ with @key@.
--
printSettingsSet :: (PrintSettingsClass self, GlibString string) => self
 -> string -- ^ @key@ - a key
 -> string -- ^ @value@ - a string value
 -> IO ()
printSettingsSet self key value =
  withUTFString value $ \valuePtr ->
  withUTFString key $ \keyPtr ->
  {# call gtk_print_settings_set #}
    (toPrintSettings self)
    keyPtr
    valuePtr

-- | Removes any value associated with @key@
--
printSettingsUnset :: (PrintSettingsClass self, GlibString string) => self
 -> string -- ^ @key@ - a key
 -> IO ()
printSettingsUnset self key =
  withUTFString key $ \keyPtr ->
  {# call gtk_print_settings_unset #}
    (toPrintSettings self)
    keyPtr

-- | Calls @func@ for each key-value pair of @settings@.
--
printSettingsForeach :: PrintSettingsClass self => self
 -> (String -> IO ()) -- ^ @func@ - the function to call
 -> IO ()
printSettingsForeach self func = do
  funcPtr <- mkPrintSettingsFunc $ \_ strPtr _ -> do
    str <- peekCString strPtr
    func str
  {# call gtk_print_settings_foreach #}
    (toPrintSettings self)
    funcPtr
    (castFunPtrToPtr funcPtr)

{#pointer PrintSettingsFunc#}

foreign import ccall "wrapper" mkPrintSettingsFunc ::
  (CString -> CString -> Ptr () -> IO ())
  -> IO PrintSettingsFunc

-- | Returns the boolean represented by the value that is associated with
-- @key@.
--
-- The string \"true\" represents @True@, any other string @False@.
--
printSettingsGetBool :: (PrintSettingsClass self, GlibString string) => self
 -> string  -- ^ @key@ - a key
 -> IO Bool -- ^ returns @True@, if @key@ maps to a true value.
printSettingsGetBool self key =
  liftM toBool $
  withUTFString key $ \keyPtr ->
  {# call gtk_print_settings_get_bool #}
    (toPrintSettings self)
    keyPtr

-- | Sets @key@ to a boolean value.
--
printSettingsSetBool :: (PrintSettingsClass self, GlibString string) => self
 -> string -- ^ @key@ - a key
 -> Bool   -- ^ @value@ - a boolean
 -> IO ()
printSettingsSetBool self key value =
  withUTFString key $ \keyPtr ->
  {# call gtk_print_settings_set_bool #}
    (toPrintSettings self)
    keyPtr
    (fromBool value)

-- | Returns the double value associated with @key@, or 0.
--
printSettingsGetDouble :: (PrintSettingsClass self, GlibString string) => self
 -> string    -- ^ @key@ - a key
 -> IO Double -- ^ returns the double value of @key@
printSettingsGetDouble self key =
  liftM realToFrac $
  withUTFString key $ \keyPtr ->
  {# call gtk_print_settings_get_double #}
    (toPrintSettings self)
    keyPtr

-- | Returns the floating point number represented by the value that is
-- associated with @key@, or @defaultVal@ if the value does not represent a
-- floating point number.
--
-- Floating point numbers are parsed with 'gAsciiStrtod'.
--
printSettingsGetDoubleWithDefault :: (PrintSettingsClass self, GlibString string) => self
 -> string    -- ^ @key@ - a key
 -> Double    -- ^ @def@ - the default value
 -> IO Double -- ^ returns the floating point number associated with @key@
printSettingsGetDoubleWithDefault self key def =
  liftM realToFrac $
  withUTFString key $ \keyPtr ->
  {# call gtk_print_settings_get_double_with_default #}
    (toPrintSettings self)
    keyPtr
    (realToFrac def)

-- | Sets @key@ to a double value.
--
printSettingsSetDouble :: (PrintSettingsClass self, GlibString string) => self
 -> string -- ^ @key@ - a key
 -> Double -- ^ @value@ - a double value
 -> IO ()
printSettingsSetDouble self key value =
  withUTFString key $ \keyPtr ->
  {# call gtk_print_settings_set_double #}
    (toPrintSettings self)
    keyPtr
    (realToFrac value)

-- | Returns the value associated with @key@, interpreted as a length. The
-- returned value is converted to @units@.
--
printSettingsGetLength :: (PrintSettingsClass self, GlibString string) => self
 -> string    -- ^ @key@ - a key
 -> Unit      -- ^ @unit@ - the unit of the return value
 -> IO Double -- ^ returns the length value of @key@, converted to @unit@
printSettingsGetLength self key unit =
  liftM realToFrac $
  withUTFString key $ \keyPtr ->
  {# call gtk_print_settings_get_length #}
    (toPrintSettings self)
    keyPtr
    ((fromIntegral . fromEnum) unit)

-- | Associates a length in units of @unit@ with @key@.
--
printSettingsSetLength :: (PrintSettingsClass self, GlibString string) => self
 -> string -- ^ @key@ - a key
 -> Double -- ^ @value@ - a length
 -> Unit   -- ^ @unit@ - the unit of @length@
 -> IO ()
printSettingsSetLength self key value unit =
  withUTFString key $ \keyPtr ->
  {# call gtk_print_settings_set_length #}
    (toPrintSettings self)
    keyPtr
    (realToFrac value)
    ((fromIntegral . fromEnum) unit)

-- | Returns the integer value of @key@, or 0.
--
printSettingsGetInt :: (PrintSettingsClass self, GlibString string) => self
 -> string -- ^ @key@ - a key
 -> IO Int -- ^ returns the integer value of @key@
printSettingsGetInt self key =
  liftM fromIntegral $
  withUTFString key $ \keyPtr ->
  {# call gtk_print_settings_get_int #}
    (toPrintSettings self)
    keyPtr

-- | Returns the value of @key@, interpreted as an integer, or the default
-- value.
--
printSettingsGetIntWithDefault :: (PrintSettingsClass self, GlibString string) => self
 -> string -- ^ @key@ - a key
 -> Int    -- ^ @def@ - the default value
 -> IO Int -- ^ returns the integer value of @key@
printSettingsGetIntWithDefault self key def =
  liftM fromIntegral $
  withUTFString key $ \keyPtr ->
  {# call gtk_print_settings_get_int_with_default #}
    (toPrintSettings self)
    keyPtr
    (fromIntegral def)

-- | Sets @key@ to an integer value.
--
printSettingsSetInt :: (PrintSettingsClass self, GlibString string) => self
 -> string -- ^ @key@ - a key
 -> Int    -- ^ @value@ - an integer
 -> IO ()
printSettingsSetInt self key value =
  withUTFString key $ \keyPtr ->
  {# call gtk_print_settings_set_int #}
    (toPrintSettings self)
    keyPtr
    (fromIntegral value)

-- | Convenience function to obtain the value of ''PrintSettingsPrinter''.
printSettingsGetPrinter :: (PrintSettingsClass self, GlibString string) => self
 -> IO string -- ^ returns the printer name
printSettingsGetPrinter self =
  {# call gtk_print_settings_get_printer #}
    (toPrintSettings self)
  >>= peekUTFString

-- | Convenience function to obtain the value of ''PrintSettingsPrinter''.
printSettingsSetPrinter :: (PrintSettingsClass self, GlibString string) => self
 -> string -- ^ @printer@ - the printer name
 -> IO ()
printSettingsSetPrinter self printer =
  withUTFString printer $ \printerPtr ->
  {# call gtk_print_settings_set_printer #}
    (toPrintSettings self)
    printerPtr

-- | Get the value of ''PrintSettingsOrientation'', converted to a 'PageOrientation'.
printSettingsGetOrientation :: PrintSettingsClass self => self
 -> IO PageOrientation -- ^ returns the orientation
printSettingsGetOrientation self =
  liftM (toEnum . fromIntegral) $
  {# call gtk_print_settings_get_orientation #}
    (toPrintSettings self)

-- | Sets the value of ''PrintSettingsOrientation''.
printSettingsSetOrientation :: PrintSettingsClass self => self
 -> PageOrientation -- ^ @orientation@ - a page orientation
 -> IO ()
printSettingsSetOrientation self orientation =
  {# call gtk_print_settings_set_orientation #}
    (toPrintSettings self)
    ((fromIntegral . fromEnum) orientation)

-- | Gets the value of 'PrintSettingsPaperFormat', converted to a 'PaperSize'.
printSettingsGetPaperSize :: PrintSettingsClass self => self
 -> IO PaperSize -- ^ returns the paper size
printSettingsGetPaperSize self =
  {# call gtk_print_settings_get_paper_size #}
            (toPrintSettings self)
  >>= mkPaperSize . castPtr

-- | Sets the value of 'PrintSettingsPaperFormat', 'PrintSettingsPaperWidth' and
-- 'PrintSettingsPaperHeight'.
printSettingsSetPaperSize :: PrintSettingsClass self => self
 -> PaperSize -- ^ @paperSize@ - a paper size
 -> IO ()
printSettingsSetPaperSize self (PaperSize paperSize) =
  {# call gtk_print_settings_set_paper_size #}
    (toPrintSettings self)
    (castPtr $ unsafeForeignPtrToPtr $ paperSize)

-- | Gets the value of 'PrintSettingsPaperWidth', converted to unit.
--
printSettingsGetPaperWidth :: PrintSettingsClass self => self
 -> Unit      -- ^ @unit@ - the unit for the return value
 -> IO Double -- ^ returns the paper width, in units of @unit@
printSettingsGetPaperWidth self unit =
  liftM realToFrac $
  {# call gtk_print_settings_get_paper_width #}
    (toPrintSettings self)
    ((fromIntegral . fromEnum) unit)

-- | Sets the value of 'PrintSettingsPaperWidth'.
--
printSettingsSetPaperWidth :: PrintSettingsClass self => self
 -> Double -- ^ @width@ - the paper width
 -> Unit   -- ^ @unit@ - the units of @width@
 -> IO ()
printSettingsSetPaperWidth self width unit =
  {# call gtk_print_settings_set_paper_width #}
    (toPrintSettings self)
    (realToFrac width)
    ((fromIntegral . fromEnum) unit)

-- | Gets the value of 'PrintSettingsPaperHeight', converted to unit.
--
printSettingsGetPaperHeight :: PrintSettingsClass self => self
 -> Unit      -- ^ @unit@ - the unit for the return value
 -> IO Double -- ^ returns the paper height, in units of @unit@
printSettingsGetPaperHeight self unit =
  liftM realToFrac $
  {# call gtk_print_settings_get_paper_height #}
    (toPrintSettings self)
    ((fromIntegral . fromEnum) unit)

-- | Sets the value of 'PrintSettingsPaperHeight'.
--
printSettingsSetPaperHeight :: PrintSettingsClass self => self
 -> Double -- ^ @height@ - the paper height
 -> Unit   -- ^ @unit@ - the units of @height@
 -> IO ()
printSettingsSetPaperHeight self height unit =
  {# call gtk_print_settings_set_paper_height #}
    (toPrintSettings self)
    (realToFrac height)
    ((fromIntegral . fromEnum) unit)

-- | Gets the value of ''PrintSettingsUseColor''.
printSettingsGetUseColor :: PrintSettingsClass self => self
 -> IO Bool -- ^ returns whether to use color
printSettingsGetUseColor self =
  liftM toBool $
  {# call gtk_print_settings_get_use_color #}
    (toPrintSettings self)

-- | Sets the value of ''PrintSettingsUseColor''.
printSettingsSetUseColor :: PrintSettingsClass self => self
 -> Bool -- ^ @useColor@ - whether to use color
 -> IO ()
printSettingsSetUseColor self useColor =
  {# call gtk_print_settings_set_use_color #}
    (toPrintSettings self)
    (fromBool useColor)

-- | Gets the value of ''PrintSettingsCollate''.
printSettingsGetCollate :: PrintSettingsClass self => self
 -> IO Bool -- ^ returns whether to collate the printed pages
printSettingsGetCollate self =
  liftM toBool $
  {# call gtk_print_settings_get_collate #}
    (toPrintSettings self)

-- | Sets the value of ''PrintSettingsCollate''.
printSettingsSetCollate :: PrintSettingsClass self => self
 -> Bool -- ^ @collate@ - whether to collate the output
 -> IO ()
printSettingsSetCollate self collate =
  {# call gtk_print_settings_set_collate #}
    (toPrintSettings self)
    (fromBool collate)

-- | Gets the value of ''PrintSettingsReverse''.
printSettingsGetReverse :: PrintSettingsClass self => self
 -> IO Bool -- ^ returns whether to reverse the order of the printed pages
printSettingsGetReverse self =
  liftM toBool $
  {# call gtk_print_settings_get_reverse #}
    (toPrintSettings self)

-- | Sets the value of ''PrintSettingsReverse''.
printSettingsSetReverse :: PrintSettingsClass self => self
 -> Bool -- ^ @reverse@ - whether to reverse the output
 -> IO ()
printSettingsSetReverse self reverse =
  {# call gtk_print_settings_set_reverse #}
    (toPrintSettings self)
    (fromBool reverse)

-- | Gets the value of ''PrintSettingsDuplex''.
printSettingsGetDuplex :: PrintSettingsClass self => self
 -> IO PrintDuplex -- ^ returns whether to print the output in duplex.
printSettingsGetDuplex self =
  liftM (toEnum . fromIntegral) $
  {# call gtk_print_settings_get_duplex #}
    (toPrintSettings self)

-- | Sets the value of 'PrintSettingsDuplex'.
printSettingsSetDuplex :: PrintSettingsClass self => self
 -> PrintDuplex -- ^ @duplex@ - a 'PrintDuplex' value
 -> IO ()
printSettingsSetDuplex self duplex =
  {# call gtk_print_settings_set_duplex #}
    (toPrintSettings self)
    ((fromIntegral . fromEnum) duplex)

-- | Gets the value of 'PrintSettingsQuality'.
printSettingsGetQuality :: PrintSettingsClass self => self
 -> IO PrintQuality -- ^ returns the print quality
printSettingsGetQuality self =
  liftM (toEnum . fromIntegral) $
  {# call gtk_print_settings_get_quality #}
    (toPrintSettings self)

-- | Sets the value of 'PrintSettingsQuality'.
printSettingsSetQuality :: PrintSettingsClass self => self
 -> PrintQuality -- ^ @quality@ - a 'PrintQuality' value
 -> IO ()
printSettingsSetQuality self quality =
  {# call gtk_print_settings_set_quality #}
    (toPrintSettings self)
    ((fromIntegral . fromEnum) quality)

-- | Gets the value of 'PrintSettingsNCopies'.
printSettingsGetNCopies :: PrintSettingsClass self => self
 -> IO Int -- ^ returns the number of copies to print
printSettingsGetNCopies self =
  liftM fromIntegral $
  {# call gtk_print_settings_get_n_copies #}
    (toPrintSettings self)

-- | Sets the value of 'PrintSettingsNCopies'.
printSettingsSetNCopies :: PrintSettingsClass self => self
 -> Int -- ^ @numCopies@ - the number of copies
 -> IO ()
printSettingsSetNCopies self numCopies =
  {# call gtk_print_settings_set_n_copies #}
    (toPrintSettings self)
    (fromIntegral numCopies)

-- | Gets the value of 'PrintSettingsNumberUp'.
printSettingsGetNumberUp :: PrintSettingsClass self => self
 -> IO Int -- ^ returns the number of pages per sheet
printSettingsGetNumberUp self =
  liftM fromIntegral $
  {# call gtk_print_settings_get_number_up #}
    (toPrintSettings self)

-- | Sets the value of 'PrintSettingsNumberUp'.
printSettingsSetNumberUp :: PrintSettingsClass self => self
 -> Int -- ^ @numberUp@ - the number of pages per sheet
 -> IO ()
printSettingsSetNumberUp self numberUp =
  {# call gtk_print_settings_set_number_up #}
    (toPrintSettings self)
    (fromIntegral numberUp)

#if GTK_CHECK_VERSION(2,14,0)
-- | Gets the value of 'PrintSettingsNumberUpLayout'.
printSettingsGetNumberUpLayout :: PrintSettingsClass self => self
 -> IO NumberUpLayout -- ^ returns layout of page in number-up mode
printSettingsGetNumberUpLayout self =
  liftM (toEnum . fromIntegral) $
  {# call gtk_print_settings_get_number_up_layout #}
    (toPrintSettings self)

-- | Sets the value of 'PrintSettingsNumberUpLayout'.
printSettingsSetNumberUpLayout :: PrintSettingsClass self => self
 -> NumberUpLayout -- ^ @numberUpLayout@ - a 'NumberUpLayout' value
 -> IO ()
printSettingsSetNumberUpLayout self numberUpLayout =
  {# call gtk_print_settings_set_number_up_layout #}
    (toPrintSettings self)
    ((fromIntegral . fromEnum) numberUpLayout)
#endif

-- | Gets the value of 'PrintSettingsResolution'.
printSettingsGetResolution :: PrintSettingsClass self => self
 -> IO Int -- ^ returns the resolution in dpi
printSettingsGetResolution self =
  liftM fromIntegral $
  {# call gtk_print_settings_get_resolution #}
    (toPrintSettings self)

-- | Sets the values of 'PrintSettingsResolution', 'PrintSettingsResolutionX' and
-- 'PrintSettingsResolutionY'.
printSettingsSetResolution :: PrintSettingsClass self => self
 -> Int -- ^ @resolution@ - the resolution in dpi
 -> IO ()
printSettingsSetResolution self resolution =
  {# call gtk_print_settings_set_resolution #}
    (toPrintSettings self)
    (fromIntegral resolution)

#if GTK_CHECK_VERSION(2,16,0)
-- | Sets the values of 'PrintSettingsResolution', 'PrintSettingsResolutionX' and
-- 'PrintSettingsResolutionY'.
--
-- * Available since Gtk+ version 2.16
--
printSettingsSetResolutionXy :: PrintSettingsClass self => self
 -> Int -- ^ @resolutionX@ - the horizontal resolution in dpi
 -> Int -- ^ @resolutionY@ - the vertical resolution in dpi
 -> IO ()
printSettingsSetResolutionXy self resolutionX resolutionY =
  {# call gtk_print_settings_set_resolution_xy #}
    (toPrintSettings self)
    (fromIntegral resolutionX)
    (fromIntegral resolutionY)

-- | Gets the value of @GTK_PRINT_SETTINGS_RESOLUTION_X@.
--
-- * Available since Gtk+ version 2.16
--
printSettingsGetResolutionX :: PrintSettingsClass self => self
 -> IO Int -- ^ returns the horizontal resolution in dpi
printSettingsGetResolutionX self =
  liftM fromIntegral $
  {# call gtk_print_settings_get_resolution_x #}
    (toPrintSettings self)

-- | Gets the value of @GTK_PRINT_SETTINGS_RESOLUTION_Y@.
--
-- * Available since Gtk+ version 2.16
--
printSettingsGetResolutionY :: PrintSettingsClass self => self
 -> IO Int -- ^ returns the vertical resolution in dpi
printSettingsGetResolutionY self =
  liftM fromIntegral $
  {# call gtk_print_settings_get_resolution_y #}
    (toPrintSettings self)

-- | Gets the value of 'PrintSettingsPrinterLpi'.
printSettingsGetPrinterLpi :: PrintSettingsClass self => self
 -> IO Double -- ^ returns the resolution in lpi (lines per inch)
printSettingsGetPrinterLpi self =
  liftM realToFrac $
  {# call gtk_print_settings_get_printer_lpi #}
    (toPrintSettings self)

-- | Sets the value of 'PrintSettingsPrinterLpi'.
printSettingsSetPrinterLpi :: PrintSettingsClass self => self
 -> Double -- ^ @lpi@ - the resolution in lpi (lines per inch)
 -> IO ()
printSettingsSetPrinterLpi self lpi =
  {# call gtk_print_settings_set_printer_lpi #}
    (toPrintSettings self)
    (realToFrac lpi)
#endif

-- | Gets the value of 'PrintSettingsScale'.
printSettingsGetScale :: PrintSettingsClass self => self
 -> IO Double -- ^ returns the scale in percent
printSettingsGetScale self =
  liftM realToFrac $
  {# call gtk_print_settings_get_scale #}
    (toPrintSettings self)

-- | Sets the value of 'PrintSettingsScale'.
printSettingsSetScale :: PrintSettingsClass self => self
 -> Double -- ^ @scale@ - the scale in percent
 -> IO ()
printSettingsSetScale self scale =
  {# call gtk_print_settings_set_scale #}
    (toPrintSettings self)
    (realToFrac scale)

-- | Gets the value of 'PrintSettingsPrintPages'.
printSettingsGetPrintPages :: PrintSettingsClass self => self
 -> IO PrintPages -- ^ returns which pages to print
printSettingsGetPrintPages self =
  liftM (toEnum . fromIntegral) $
  {# call gtk_print_settings_get_print_pages #}
    (toPrintSettings self)

-- | Sets the value of 'PrintSettingsPrintPages'.
printSettingsSetPrintPages :: PrintSettingsClass self => self
 -> PrintPages -- ^ @pages@ - a 'PrintPages' value
 -> IO ()
printSettingsSetPrintPages self pages =
  {# call gtk_print_settings_set_print_pages #}
    (toPrintSettings self)
    ((fromIntegral . fromEnum) pages)

-- | Gets the value of 'PrintSettingsPageRanges'.
--
-- printSettingsGetPageRanges :: PrintSettingsClass self => self
--  -> IO [PageRange]               -- ^ returns an array of 'PageRange'.
-- printSettingsGetPageRanges self =
--   alloca $ \numRangesPtr -> do
--   rangeListPtr <- {# call gtk_print_settings_get_page_ranges #}
--                    (toPrintSettings self)
--                    numRangesPtr
--   rangeLen <- peek numRangesPtr
--   ptrList <- peekArray (fromIntegral rangeLen) (castPtr rangeListPtr)
--   rangeList <- mapM peek ptrList
--   {#call unsafe g_free#} (castPtr rangeListPtr)
--   return rangeList

-- | Sets the value of @GTK_PRINT_SETTINGS_PAGE_RANGES@.
--
-- printSettingsSetPageRanges :: PrintSettingsClass self => self
--  -> [PageRange]                  -- ^ @pageRanges@ - an array of 'PageRange'
--  -> IO ()
-- printSettingsSetPageRanges self rangeList =
--   withArrayLen (concatMap (\(PageRange x y) -> [fromIntegral x, fromIntegral y]) rangeList)
--       $ \rangeLen rangeListPtr ->
--           {# call gtk_print_settings_set_page_ranges #}
--              (toPrintSettings self)
--              (castPtr rangeListPtr)
--              (fromIntegral rangeLen)

-- | Gets the value of 'PrintSettingsPageSet'.
printSettingsGetPageSet :: PrintSettingsClass self => self
 -> IO PageSet -- ^ returns the set of pages to print
printSettingsGetPageSet self =
  liftM (toEnum . fromIntegral) $
  {# call gtk_print_settings_get_page_set #}
    (toPrintSettings self)

-- | Sets the value of 'PrintSettingsPageSet'.
printSettingsSetPageSet :: PrintSettingsClass self => self
 -> PageSet -- ^ @pageSet@ - a 'PageSet' value
 -> IO ()
printSettingsSetPageSet self pageSet =
  {# call gtk_print_settings_set_page_set #}
    (toPrintSettings self)
    ((fromIntegral . fromEnum) pageSet)

-- | Gets the value of 'PrintSettingsDefaultSource'.
printSettingsGetDefaultSource :: (PrintSettingsClass self, GlibString string) => self
 -> IO string -- ^ returns the default source
printSettingsGetDefaultSource self =
  {# call gtk_print_settings_get_default_source #}
    (toPrintSettings self)
  >>= peekUTFString

-- | Sets the value of 'PrintSettingsDefaultSource'.
printSettingsSetDefaultSource :: (PrintSettingsClass self, GlibString string) => self
 -> string -- ^ @defaultSource@ - the default source
 -> IO ()
printSettingsSetDefaultSource self defaultSource =
  withUTFString defaultSource $ \defaultSourcePtr ->
  {# call gtk_print_settings_set_default_source #}
    (toPrintSettings self)
    defaultSourcePtr

-- | Gets the value of 'PrintSettingsMediaType'.
printSettingsGetMediaType :: (PrintSettingsClass self, GlibString string) => self
 -> IO string -- ^ returns the media type
printSettingsGetMediaType self =
  {# call gtk_print_settings_get_media_type #}
    (toPrintSettings self)
  >>= peekUTFString

-- | Sets the value of 'PrintSettingsMediaType'.
printSettingsSetMediaType :: (PrintSettingsClass self, GlibString string) => self
 -> string -- ^ @mediaType@ - the media type
 -> IO ()
printSettingsSetMediaType self mediaType =
  withUTFString mediaType $ \mediaTypePtr ->
  {# call gtk_print_settings_set_media_type #}
    (toPrintSettings self)
    mediaTypePtr

-- | Gets the value of 'PrintSettingsDither'.
printSettingsGetDither :: (PrintSettingsClass self, GlibString string) => self
 -> IO string -- ^ returns the dithering that is used
printSettingsGetDither self =
  {# call gtk_print_settings_get_dither #}
    (toPrintSettings self)
  >>= peekUTFString

-- | Sets the value of 'PrintSettingsDither'.
printSettingsSetDither :: (PrintSettingsClass self, GlibString string) => self
 -> string -- ^ @dither@ - the dithering that is used
 -> IO ()
printSettingsSetDither self dither =
  withUTFString dither $ \ditherPtr ->
  {# call gtk_print_settings_set_dither #}
    (toPrintSettings self)
    ditherPtr

-- | Gets the value of 'PrintSettingsFinishings'.
printSettingsGetFinishings :: (PrintSettingsClass self, GlibString string) => self
 -> IO string -- ^ returns the finishings
printSettingsGetFinishings self =
  {# call gtk_print_settings_get_finishings #}
    (toPrintSettings self)
  >>= peekUTFString

-- | Sets the value of 'PrintSettingsFinishings'.
printSettingsSetFinishings :: (PrintSettingsClass self, GlibString string) => self
 -> string -- ^ @finishings@ - the finishings
 -> IO ()
printSettingsSetFinishings self finishings =
  withUTFString finishings $ \finishingsPtr ->
  {# call gtk_print_settings_set_finishings #}
    (toPrintSettings self)
    finishingsPtr

-- | Gets the value of 'PrintSettingsOutputBin'.
printSettingsGetOutputBin :: (PrintSettingsClass self, GlibString string) => self
 -> IO string -- ^ returns the output bin
printSettingsGetOutputBin self =
  {# call gtk_print_settings_get_output_bin #}
    (toPrintSettings self)
  >>= peekUTFString

-- | Sets the value of 'PrintSettingsOutputBin'.
printSettingsSetOutputBin :: (PrintSettingsClass self, GlibString string) => self
 -> string -- ^ @outputBin@ - the output bin
 -> IO ()
printSettingsSetOutputBin self outputBin =
  withUTFString outputBin $ \outputBinPtr ->
  {# call gtk_print_settings_set_output_bin #}
    (toPrintSettings self)
    outputBinPtr


#if GTK_CHECK_VERSION(2,14,0)
-- | Reads the print settings from @fileName@. See 'printSettingsToFile'.
--
-- * Available since Gtk+ version 2.14
--
printSettingsLoadFile :: (PrintSettingsClass self, GlibString string) => self
 -> string  -- ^ @fileName@ - the filename to read the settings from
 -> IO Bool -- ^ returns @True@ on success
printSettingsLoadFile self fileName =
  liftM toBool $
  propagateGError $ \errorPtr ->
  withUTFString fileName $ \fileNamePtr ->
  {# call gtk_print_settings_load_file #}
    (toPrintSettings self)
    fileNamePtr
    errorPtr

#endif

#if GTK_CHECK_VERSION(2,12,0)
-- | This function saves the print settings from @settings@ to @fileName@.
--
-- * Available since Gtk+ version 2.12
--
printSettingsToFile :: (PrintSettingsClass self, GlibString string) => self
 -> string  -- ^ @fileName@ - the file to save to
 -> IO Bool -- ^ returns @True@ on success
printSettingsToFile self fileName =
  liftM toBool $
  propagateGError $ \errorPtr ->
  withUTFString fileName $ \fileNamePtr ->
  {# call gtk_print_settings_to_file #}
    (toPrintSettings self)
    fileNamePtr
    errorPtr
#endif

-- | Obtain the value of 'PrintSettingsPrinter'.
printSettingsPrinter :: (PrintSettingsClass self, GlibString string) => Attr self string
printSettingsPrinter = newAttr
  printSettingsGetPrinter
  printSettingsSetPrinter

-- | The value of ''PrintSettingsOrientation'', converted to a 'PageOrientation'.
printSettingsOrientation :: PrintSettingsClass self => Attr self PageOrientation
printSettingsOrientation = newAttr
  printSettingsGetOrientation
  printSettingsSetOrientation

-- | The value of 'PrintSettingsPaperFormat', converted to a 'PaperSize'.
printSettingsPaperSize :: PrintSettingsClass self => Attr self PaperSize
printSettingsPaperSize = newAttr
  printSettingsGetPaperSize
  printSettingsSetPaperSize

-- | The value of ''PrintSettingsUseColor''.
printSettingsUseColor :: PrintSettingsClass self => Attr self Bool
printSettingsUseColor = newAttr
  printSettingsGetUseColor
  printSettingsSetUseColor

-- | The value of ''PrintSettingsCollate''.
printSettingsCollate :: PrintSettingsClass self => Attr self Bool
printSettingsCollate = newAttr
  printSettingsGetCollate
  printSettingsSetCollate

-- | The value of ''PrintSettingsReverse''.
printSettingsReverse :: PrintSettingsClass self => Attr self Bool
printSettingsReverse = newAttr
  printSettingsGetReverse
  printSettingsSetReverse

-- | The value of ''PrintSettingsDuplex''.
printSettingsDuplex :: PrintSettingsClass self => Attr self PrintDuplex
printSettingsDuplex = newAttr
  printSettingsGetDuplex
  printSettingsSetDuplex

-- | The value of ''PrintSettingsQuality''.
printSettingsQuality :: PrintSettingsClass self => Attr self PrintQuality
printSettingsQuality = newAttr
  printSettingsGetQuality
  printSettingsSetQuality

-- | The value of 'PrintSettingsNCopies'.
printSettingsNCopies :: PrintSettingsClass self => Attr self Int
printSettingsNCopies = newAttr
  printSettingsGetNCopies
  printSettingsSetNCopies

-- | The value of 'PrintSettingsNumberUp'.
printSettingsNumberUp :: PrintSettingsClass self => Attr self Int
printSettingsNumberUp = newAttr
  printSettingsGetNumberUp
  printSettingsSetNumberUp

-- | The value of 'PrintSettingsResolution'.
printSettingsResolution :: PrintSettingsClass self => Attr self Int
printSettingsResolution = newAttr
  printSettingsGetResolution
  printSettingsSetResolution

-- | The value of 'PrintSettingsScale'.
printSettingsScale :: PrintSettingsClass self => Attr self Double
printSettingsScale = newAttr
  printSettingsGetScale
  printSettingsSetScale

-- | The value of 'PrintSettingsPrintPages'.
printSettingsPrintPages :: PrintSettingsClass self => Attr self PrintPages
printSettingsPrintPages = newAttr
  printSettingsGetPrintPages
  printSettingsSetPrintPages

-- | The value of 'PrintSettingsPageSet'.
printSettingsPageSet :: PrintSettingsClass self => Attr self PageSet
printSettingsPageSet = newAttr
  printSettingsGetPageSet
  printSettingsSetPageSet

-- | The value of 'PrintSettingsDefaultSource'.
printSettingsDefaultSource :: (PrintSettingsClass self, GlibString string) => Attr self string
printSettingsDefaultSource = newAttr
  printSettingsGetDefaultSource
  printSettingsSetDefaultSource

-- | The value of 'PrintSettingsMediaType'.
printSettingsMediaType :: (PrintSettingsClass self, GlibString string) => Attr self string
printSettingsMediaType = newAttr
  printSettingsGetMediaType
  printSettingsSetMediaType

-- | The value of 'PrintSettingsDither'.
printSettingsDither :: (PrintSettingsClass self, GlibString string) => Attr self string
printSettingsDither = newAttr
  printSettingsGetDither
  printSettingsSetDither

-- | The value of 'PrintSettingsFinishings'.
printSettingsFinishings :: (PrintSettingsClass self, GlibString string) => Attr self string
printSettingsFinishings = newAttr
  printSettingsGetFinishings
  printSettingsSetFinishings

-- | The value of 'PrintSettingsOutputBin'.
printSettingsOutputBin :: (PrintSettingsClass self, GlibString string) => Attr self string
printSettingsOutputBin = newAttr
  printSettingsGetOutputBin
  printSettingsSetOutputBin

#if GTK_CHECK_VERSION(2,14,0)
-- | The value of 'PrintSettingsNumberUpLayout'.
printSettingsNumberUpLayout :: PrintSettingsClass self => Attr self NumberUpLayout
printSettingsNumberUpLayout = newAttr
  printSettingsGetNumberUpLayout
  printSettingsSetNumberUpLayout
#endif

#if GTK_CHECK_VERSION(2,16,0)
-- | The value of 'PrintSettingsPrinterLpi'.
printSettingsPrinterLpi :: PrintSettingsClass self => Attr self Double
printSettingsPrinterLpi = newAttr
  printSettingsGetPrinterLpi
  printSettingsSetPrinterLpi
#endif
#endif

