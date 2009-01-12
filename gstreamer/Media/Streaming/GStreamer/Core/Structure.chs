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
module Media.Streaming.GStreamer.Core.Structure (
  Structure,
  structureEmpty,
  structureToString,
  structureFromString,
  structureName,
  structureHasName,
  structureGetBool,
  structureGetInt,
  structureGetFourCC,
  structureGetDouble, 
  structureGetString,
  structureGetDate, 
  structureGetClockTime,
  structureGetFraction,
  
  StructureM,
  structureCreate,
  structureModify,
  structureSetNameM,
  structureRemoveFieldM,
  structureSetBoolM,
  structureSetIntM,
  structureSetFourCCM,
  structureSetDoubleM,
  structureSetStringM,
  structureSetDateM,
  structureSetClockTimeM,
  structureSetFractionM,
  structureFixateFieldNearestIntM,
  structureFixateFieldNearestDoubleM,
  structureFixateFieldNearestFractionM,
  structureFixateFieldBoolM
  ) where

import Data.Ratio ( (%)
                  , numerator
                  , denominator )
import Control.Monad (liftM)
{#import Media.Streaming.GStreamer.Core.Types#}
import System.Glib.UTFString
import System.Glib.FFI
import System.Glib.GTypeConstants
{#import System.Glib.GDateTime#}
{#import System.Glib.GType#}
{#import System.Glib.GValue#}
{#import System.Glib.GValueTypes#}

{# context lib = "gstreamer" prefix = "gst" #}

structureEmpty :: String
               -> Structure
structureEmpty name =
    unsafePerformIO $
        withUTFString name {# call structure_empty_new #} >>=
            takeStructure

structureToString :: Structure
                  -> String
structureToString structure =
    unsafePerformIO $
        {# call structure_to_string #} structure >>=
            readUTFString

structureFromString :: String
                    -> (Maybe Structure, Int)
structureFromString string =
    unsafePerformIO $
        withUTFString string $ \cString ->
            alloca $ \endPtr ->
                do structure <- {# call structure_from_string #} cString endPtr >>=
                                    maybePeek takeStructure
                   end <- peek endPtr
                   offset <- {# call g_utf8_pointer_to_offset #} cString end
                   return (structure, fromIntegral offset)

structureName :: Structure
              -> String
structureName structure =
    unsafePerformIO $
        {# call structure_get_name #} structure >>=
            peekUTFString

structureHasName :: Structure
                 -> String
                 -> Bool
structureHasName structure name =
    toBool $ unsafePerformIO $
        withUTFString name $
             {# call structure_has_name #} structure

marshalStructureGet :: Storable a
                    => (Structure -> CString -> Ptr a -> IO {# type gboolean #})
                    -> (a -> IO b)
                    -> Structure
                    -> String
                    -> Maybe b
marshalStructureGet getAction convert structure fieldname =
    unsafePerformIO $
        alloca $ \ptr ->
            withUTFString fieldname $ \cFieldname ->
                do result <- getAction structure cFieldname ptr
                   if toBool result
                       then liftM Just $ peek (castPtr ptr) >>= convert
                       else return Nothing

structureGetBool :: Structure
                 -> String
                 -> Maybe Bool
structureGetBool =
    marshalStructureGet {# call structure_get_boolean #} $
        return . toBool

structureGetInt :: Structure
                -> String
                -> Maybe Int
structureGetInt =
    marshalStructureGet {# call structure_get_int #} $
        return . fromIntegral

structureGetFourCC :: Structure
                   -> String
                   -> Maybe FourCC
structureGetFourCC =
    marshalStructureGet {# call structure_get_fourcc #} $
        return . fromIntegral

structureGetDouble :: Structure
                   -> String
                   -> Maybe Double
structureGetDouble =
    marshalStructureGet {# call structure_get_double #} $
        return . realToFrac

structureGetString :: Structure
                   -> String
                   -> Maybe String
structureGetString structure fieldname =
    unsafePerformIO $
        (withUTFString fieldname $ {# call structure_get_string #} structure) >>=
            maybePeek peekUTFString

structureGetDate :: Structure
                 -> String
                 -> Maybe GDate
structureGetDate =
    marshalStructureGet {# call structure_get_date #} $
        peek . castPtr

structureGetClockTime :: Structure
                      -> String
                      -> Maybe ClockTime
structureGetClockTime =
    marshalStructureGet {# call structure_get_clock_time #} $
        return . fromIntegral

structureGetFraction :: Structure
                     -> String
                     -> Maybe Fraction
structureGetFraction structure fieldname =
    unsafePerformIO $
        alloca $ \numPtr -> alloca $ \denPtr ->
            withUTFString fieldname $ \cFieldname ->
                do result <- {# call structure_get_fraction #} structure cFieldname numPtr denPtr
                   if toBool result
                       then do num <- peek numPtr
                               den <- peek denPtr
                               return $ Just $ (fromIntegral num) % (fromIntegral den)
                       else return Nothing

marshalStructureModify :: IO (Ptr Structure)
                       -> StructureM a
                       -> (Structure, a)
marshalStructureModify mkStructure (StructureM action) =
    unsafePerformIO $
        do ptr <- mkStructure
           structure <- liftM Structure $ newForeignPtr_ ptr
           result <- action structure
           structure' <- takeStructure ptr
           return (structure', result)

structureCreate :: String
                -> StructureM a
                -> (Structure, a)
structureCreate name action =
    marshalStructureModify
        (withUTFString name {# call structure_empty_new #})
        action

structureModify :: Structure
                -> StructureM a
                -> (Structure, a)
structureModify structure action =
    marshalStructureModify
        ({# call structure_copy #} structure)
        action

structureSetNameM :: String
                  -> StructureM ()
structureSetNameM name =
    StructureM $ \structure ->
        withUTFString name $ {# call structure_set_name #} structure

structureRemoveFieldM :: String
                      -> StructureM ()
structureRemoveFieldM name =
    StructureM $ \structure ->
        withUTFString name $ {# call structure_remove_field #} structure

marshalStructureSetM :: GType
                     -> (GValue -> a -> IO ())
                     -> String
                     -> a
                     -> StructureM ()
marshalStructureSetM valueType setGValue fieldname value =
    StructureM $ \structure ->
        withUTFString fieldname $ \cFieldname ->
        allocaGValue $ \gValue ->
            do valueInit gValue valueType
               setGValue gValue value
               {# call structure_set_value #} structure cFieldname gValue

structureSetBoolM :: String
                  -> Bool
                  -> StructureM ()
structureSetBoolM =
    marshalStructureSetM bool valueSetBool

structureSetIntM :: String
                 -> Int
                 -> StructureM ()
structureSetIntM =
    marshalStructureSetM int valueSetInt

structureSetFourCCM :: String
                    -> FourCC
                    -> StructureM ()
structureSetFourCCM =
    marshalStructureSetM fourcc $ \gValue fourcc ->
        {# call value_set_fourcc #} gValue $ fromIntegral fourcc

structureSetDoubleM :: String
                    -> Double
                    -> StructureM ()
structureSetDoubleM =
    marshalStructureSetM double valueSetDouble

structureSetStringM :: String
                    -> String
                    -> StructureM ()
structureSetStringM =
    marshalStructureSetM string valueSetString

structureSetDateM :: String
                  -> GDate
                  -> StructureM ()
structureSetDateM =
    marshalStructureSetM date $ \gValue date ->
        with date $ ({# call value_set_date #} gValue) . castPtr

structureSetClockTimeM :: String
                       -> ClockTime
                       -> StructureM ()
structureSetClockTimeM =
    marshalStructureSetM uint64 $ \gValue clockTime ->
        {# call g_value_set_uint64 #} gValue $ fromIntegral clockTime

structureSetFractionM :: String
                      -> Fraction
                      -> StructureM ()
structureSetFractionM =
    marshalStructureSetM fraction $ \gValue fraction ->
        {# call value_set_fraction #} gValue
                                      (fromIntegral $ numerator fraction)
                                      (fromIntegral $ denominator fraction)

marshalStructureFixateM :: (Structure -> CString -> a -> IO {# type gboolean #})
                        -> String
                        -> a
                        -> StructureM Bool
marshalStructureFixateM fixate fieldname target =
    StructureM $ \structure ->
        withUTFString fieldname $ \cFieldname ->
            liftM toBool $
                fixate structure cFieldname target

structureFixateFieldNearestIntM :: String
                                -> Int
                                -> StructureM Bool
structureFixateFieldNearestIntM =
    marshalStructureFixateM $ \structure cFieldname target ->
        {# call structure_fixate_field_nearest_int #}
            structure
            cFieldname
            (fromIntegral target)

structureFixateFieldNearestDoubleM :: String
                                   -> Double
                                   -> StructureM Bool
structureFixateFieldNearestDoubleM =
    marshalStructureFixateM $ \structure cFieldname target ->
        {# call structure_fixate_field_nearest_double #}
            structure
            cFieldname
            (realToFrac target)

structureFixateFieldNearestFractionM :: String
                                     -> Fraction
                                     -> StructureM Bool
structureFixateFieldNearestFractionM =
    marshalStructureFixateM $ \structure cFieldname target ->
                {# call structure_fixate_field_nearest_fraction #}
                    structure
                    cFieldname
                    (fromIntegral $ numerator target)
                    (fromIntegral $ denominator target)

structureFixateFieldBoolM :: String
                          -> Bool
                          -> StructureM Bool
structureFixateFieldBoolM =
    marshalStructureFixateM $ \structure cFieldname target ->
                {# call structure_fixate_field_boolean #}
                    structure
                    cFieldname
                    (fromBool target)


fourcc = {# call fun fourcc_get_type #}
date = {# call fun date_get_type #}         
fraction = {# call fun fraction_get_type #}         
