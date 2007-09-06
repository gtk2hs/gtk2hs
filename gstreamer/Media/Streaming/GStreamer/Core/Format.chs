--  GIMP Toolkit (GTK) Binding for Haskell: binding to gstreamer -*-haskell-*-
--
--  Author : Peter Gavin
--  Created: 1-Apr-2007
--
--  Copyright (c) 2007 Peter Gavin
--
--  This library is free software: you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public License
--  as published by the Free Software Foundation, either version 3 of
--  the License, or (at your option) any later version.
--  
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--  
--  You should have received a copy of the GNU Lesser General Public
--  License along with this program.  If not, see
--  <http://www.gnu.org/licenses/>.
--  
--  GStreamer, the C library which this Haskell library depends on, is
--  available under LGPL Version 2. The documentation included with
--  this library is based on the original GStreamer documentation.
--  
-- | Maintainer  : gtk2hs-devel@lists.sourceforge.net
--   Stability   : alpha
--   Portability : portable (depends on GHC)
module Media.Streaming.GStreamer.Core.Format (
  
  Format(..),
  formatGetName,
  formatToQuark,
  formatRegister,
  formatGetByNick,
  formatsContains,
  formatGetDetails,
  formatIterateDefinitions
  
  ) where

{#import Media.Streaming.GStreamer.Core.Types#}

{# context lib = "gstreamer" prefix = "gst" #}

import Control.Monad (liftM)
import System.Glib.FFI
import System.Glib.UTFString
{#import System.Glib.GObject#}

formatGetName :: Format
              -> IO String
formatGetName format =
    peekUTFString $
        ({# call fun format_get_name #} $
             fromIntegral $ fromEnum format)

formatToQuark :: Format
              -> IO Quark
formatToQuark =
    {# call format_to_quark #} . fromIntegral . fromEnum

formatRegister :: String
               -> String
               -> IO Format
formatRegister nick description =
    liftM (toEnum . fromIntegral) $
        withUTFString nick $ \cNick ->
            (withUTFString description $
                 {# call format_register #} cNick)

formatGetByNick :: String
                -> IO Format
formatGetByNick nick =
    liftM cToEnum $
        withUTFString nick {# call format_get_by_nick #}

formatsContains :: [Format]
                -> Format
                -> IO Bool
formatsContains formats format =
    liftM toBool $ 
        withArray0 0 (map cFromEnum formats) $ \cFormats ->
            {# call formats_contains #} cFormats $ cFromEnum format

formatGetDetails :: Format
                 -> IO FormatDefinition
formatGetDetails format =
    ({# call format_get_details #} $ cFromEnum format) >>=
        peek . castPtr

formatIterateDefinitions :: IO (Iterator FormatDefinition)
formatIterateDefinitions =
    {# call format_iterate_definitions #} >>= takeIterator
