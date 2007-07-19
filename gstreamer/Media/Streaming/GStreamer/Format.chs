-- GIMP Toolkit (GTK) Binding for Haskell: binding to gstreamer   -*-haskell-*-
--
--  Author : Peter Gavin
--  Created: 1-Apr-2007
--
--  Version $Revision$ from $Date$
--
--  Copyright (c) 2007 Peter Gavin
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
module Media.Streaming.GStreamer.Format (
  
  Format(..),
  formatGetName,
  formatToQuark,
  formatRegister,
  formatGetByNick,
  formatsContains,
  formatGetDetails,
  formatIterateDefinitions
  
  ) where

{#import Media.Streaming.GStreamer.Types#}

{# context lib = "gstreamer" prefix = "gst" #}

import Control.Monad (liftM)
import System.Glib.FFI
import System.Glib.UTFString
{#import System.Glib.GObject#}

formatGetName :: Format
              -> String
formatGetName format =
    unsafePerformIO $ peekUTFString $
        ({# call fun format_get_name #} $
             fromIntegral $ fromEnum format)

formatToQuark :: Format
              -> Quark
formatToQuark =
    {# call fun format_to_quark #} . fromIntegral . fromEnum

formatRegister :: String
               -> String
               -> IO Format
formatRegister nick description =
    liftM (toEnum . fromIntegral) $
        withUTFString nick $ \cNick ->
            (withUTFString description $
                 {# call format_register #} cNick)

formatGetByNick :: String
                -> Format
formatGetByNick nick =
    toEnum $ fromIntegral $ unsafePerformIO $
        withUTFString nick {# call format_get_by_nick #}

formatsContains :: [Format]
                -> Format
                -> Bool
formatsContains formats format =
    toBool $ unsafePerformIO $
        withArray0 0 (map fromFormat formats) $ \cFormats ->
            {# call formats_contains #} cFormats $ fromFormat format

formatGetDetails :: Format
                 -> FormatDefinition
formatGetDetails format =
    unsafePerformIO $
        ({# call format_get_details #} $ fromFormat format) >>=
            peek . castPtr

formatIterateDefinitions :: IO (Iterator FormatDefinition)
formatIterateDefinitions =
    {# call format_iterate_definitions #} >>= newIterator
