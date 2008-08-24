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
  
  -- * Types
  Format(..),
  FormatDefinition(..),
  FormatId,
  
  -- * Format Operations
  formatPercentMax,
  formatPercentScale,
  formatGetName,
  formatToQuark,
  formatRegister,
  formatGetByNick,
  formatGetDetails,
  formatIterateDefinitions
  
  ) where

{#import Media.Streaming.GStreamer.Core.Types#}

{# context lib = "gstreamer" prefix = "gst" #}

import Control.Monad (liftM)
import System.Glib.FFI
import System.Glib.UTFString
{#import System.Glib.GObject#}

-- | Get a printable name for the given format.
formatGetName :: Format    -- ^ @format@ - a format
              -> IO String -- ^ the name of the format
formatGetName format =
    peekUTFString $
        ({# call fun format_get_name #} $
             fromIntegral $ fromFormat format)

-- | Get the unique quark for the given format.
formatToQuark :: Format   -- ^ @format@ - a format
              -> IO Quark -- ^ the unique quark for the format
formatToQuark =
    {# call format_to_quark #} . fromIntegral . fromFormat

-- | Create a new format based on the given nickname, or register a new format with that nickname.
formatRegister :: String    -- ^ @nick@ - the nickname for the format
               -> String    -- ^ @description@ - the description for the format
               -> IO Format -- ^ the format with the given nickname
formatRegister nick description =
    liftM (toFormat . fromIntegral) $
        withUTFString nick $ \cNick ->
            (withUTFString description $
                 {# call format_register #} cNick)

-- | Get the format with the given nickname, or 'FormatUndefined' if
--   no format by that nickname was found.
formatGetByNick :: String    -- ^ @nick@ - the nickname for the format
                -> IO Format -- ^ the format with the given nickname,
                             --   or 'FormatUndefined' if it was not found
formatGetByNick nick =
    liftM (toFormat . fromIntegral) $
        withUTFString nick {# call format_get_by_nick #}

-- | Get the given format's definition.
formatGetDetails :: Format                      -- ^ @format@ - a format
                 -> IO (Maybe FormatDefinition) -- ^ the definition for the given format, or 
                                                --   'Nothing' if the format was not found
formatGetDetails format =
    ({# call format_get_details #} $ fromIntegral $ fromFormat format) >>=
        maybePeek peek . castPtr

-- | Get an Iterator over all registered formats.
formatIterateDefinitions :: IO (Iterator FormatDefinition)
formatIterateDefinitions =
    {# call format_iterate_definitions #} >>= takeIterator
