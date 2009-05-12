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
module Media.Streaming.GStreamer.Core.Query (
  
  Query,
  QueryClass,
  toQuery,
  castToQuery,
  gTypeQuery,
  
  ) where

{#import Media.Streaming.GStreamer.Core.Types#}
import System.Glib.FFI
{#import System.Glib.GObject#}
import System.Glib.UTFString

{# context lib = "gstreamer" prefix = "gst" #}

queryNone, queryPosition, queryDuration,
           queryLatency,  queryJitter,
           queryRate,     querySeeking,
           querySegment,  queryConvert,
           queryFormats :: QueryType
queryNone     = 0
queryPosition = 1
queryDuration = 2
queryLatency  = 3
queryJitter   = 4
queryRate     = 5
querySeeking  = 6
querySegment  = 7
queryConvert  = 8
queryFormats  = 9

queryTypeGetName :: QueryType
                 -> IO String
queryTypeGetName queryType =
    {# call query_type_get_name #} (fromIntegral queryType) >>=
        peekUTFString

queryType :: Query
          -> QueryType
queryType query =
    unsafePerformIO $ withMiniObject query cQueryType
foreign import ccall unsafe "_hs_gst_query_type"
    cQueryType :: Ptr Query
               -> IO {# type GstQueryType #}

queryTypeName :: Query
              -> IO String
queryTypeName =
    queryTypeGetName . queryType

queryTypeToQuark :: QueryType
                 -> IO Quark
queryTypeToQuark = {# call query_type_to_quark #}

queryTypeRegister :: String
                  -> String
                  -> IO QueryType
queryTypeRegister nick description =
    withUTFString nick $ \cNick ->
        withUTFString description $
            {# call query_type_register #} cNick

queryTypeGetByNick :: String
                   -> IO QueryType
queryTypeGetByNick nick =
    withUTFString nick {# call query_type_get_by_nick #}

queryTypeGetDetails :: QueryType
                    -> IO (Maybe QueryTypeDefinition)
queryTypeGetDetails queryType =
    {# call query_type_get_details #} queryType >>=
        maybePeek (peek . castPtr)

queryTypeIterateDefinitions :: IO (Iterator QueryTypeDefinition)
queryTypeIterateDefinitions =
    {# call query_type_iterate_definitions #} >>= takeIterator

queryNewConvert :: Format
                -> Word64
                -> Format
                -> IO Query
queryNewConvert srcFormat value destFormat =
    {# call query_new_convert #} (fromIntegral $ fromFormat srcFormat)
                                 (fromIntegral value)
                                 (fromIntegral $ fromFormat destFormat) >>=
        takeMiniObject
