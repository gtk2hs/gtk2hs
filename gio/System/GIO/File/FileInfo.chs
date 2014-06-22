{-# LANGUAGE CPP #-}
--  GIMP Toolkit (GTK) Binding for Haskell: binding to gio -*-haskell-*-
--
--  Author : Andy Stewart
--  Created: 30-Apirl-2010
--
--  Copyright (c) 2010 Andy Stewart
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
--  GIO, the C library which this Haskell library depends on, is
--  available under LGPL Version 2. The documentation included with
--  this library is based on the original GIO documentation.
--
-- | Maintainer  : gtk2hs-devel@lists.sourceforge.net
--   Stability   : alpha
--   Portability : portable (depends on GHC)
module System.GIO.File.FileInfo (
-- * Details
--
-- | Functionality for manipulating basic metadata for files. 'FileInfo' implements methods for getting
-- information that all files should contain, and allows for manipulation of extended attributes.
--
-- See 'FileAttribute' for more information on how GIO handles file attributes.
--
-- To obtain a 'FileInfo' for a 'File', use 'fileQueryInfo'. To obtain a
-- 'FileInfo' for a file input or output stream, use 'fileInputStreamQueryInfo' or
-- 'fileOutputStreamQueryInfo'.
--
-- To change the actual attributes of a file, you should then set the attribute in the 'FileInfo' and
-- call 'fileSetAttributesFromInfo' on a 'File'.
--
-- However, not all attributes can be changed in the file. For instance, the actual size of a file
-- cannot be changed via 'fileInfoSetSize' and
-- 'fileQueryWritableNamespaces' to discover the settable attributes of a particular file at
-- runtime.
--
-- 'FileAttributeMatcher' allows for searching through a 'FileInfo' for attributes.

-- * Types
    FileInfo(..),
    FileInfoClass,

-- * Enums,
    FileAttributeStatus (..),

-- * Methods
    fileInfoNew,
    fileInfoDup,
    fileInfoCopyInto,
    fileInfoHasAttribute,
#if GLIB_CHECK_VERSION(2,22,0)
    fileInfoHasNamespace,
#endif
    fileInfoListAttributes,
    fileInfoGetAttributeType,
    fileInfoRemoveAttribute,
    fileInfoGetAttributeAsString,
    fileInfoGetAttributeStatus,
    fileInfoGetAttributeString,
#if GLIB_CHECK_VERSION(2,22,0)
    fileInfoGetAttributeStringList,
#endif
    fileInfoGetAttributeByteString,
    fileInfoGetAttributeBool,
    fileInfoGetAttributeWord32,
    fileInfoGetAttributeInt32,
    fileInfoGetAttributeWord64,
    fileInfoGetAttributeInt64,
    fileInfoGetAttributeObject,
#if GLIB_CHECK_VERSION(2,22,0)
    fileInfoSetAttributeStatus,
#endif
    fileInfoSetAttributeString,
#if GLIB_CHECK_VERSION(2,22,0)
    fileInfoSetAttributeStringList,
#endif
    fileInfoSetAttributeByteString,
    fileInfoSetAttributeBool,
    fileInfoSetAttributeWord32,
    fileInfoSetAttributeInt32,
    fileInfoSetAttributeWord64,
    fileInfoSetAttributeInt64,
    fileInfoSetAttributeObject,

    fileInfoClearStatus,
    fileInfoGetFileType,
    fileInfoGetIsHidden,
    fileInfoGetIsBackup,
    fileInfoGetIsSymlink,
    fileInfoGetName,
    fileInfoGetDisplayName,
    fileInfoGetEditName,
    fileInfoGetIcon,
    fileInfoGetContentType,
    fileInfoGetModificationTime,
    fileInfoGetSize,
    fileInfoGetSymlinkTarget,
    fileInfoGetEtag,
    fileInfoGetSortOrder,
    fileInfoUnsetAttributeMask,
    fileInfoSetFileType,
    fileInfoSetIsHidden,
    fileInfoSetIsSymlink,
    fileInfoSetName,
    fileInfoSetDisplayName,
    fileInfoSetEditName,
    fileInfoSetIcon,
    fileInfoSetContentType,
    fileInfoSetModificationTime,
    fileInfoSetSize,

    -- 'fileInfoSetSymlinkTarget' use *static string* that we don't understand the purpose of this function.
    -- If someone know how to use it, uncomment it and fix documentation.
    -- fileInfoSetSymlinkTarget,

    fileInfoSetSortOrder,
    ) where

import Control.Monad

import Data.ByteString (ByteString)
import Data.ByteString (useAsCString)
import Data.ByteString.Unsafe (unsafePackCStringFinalizer)
import System.Glib.FFI
import System.Glib.Flags
import System.Glib.GError
import System.Glib.UTFString
import System.Glib.GObject
import System.Glib.GDateTime

import System.GIO.Enums
import System.GIO.File.FileAttribute
{#import System.GIO.Types#}

{# context lib = "gio" prefix = "g" #}

-- | Creates a new file info structure.
fileInfoNew :: IO FileInfo
fileInfoNew =
    wrapNewGObject mkFileInfo $
    {#call g_file_info_new#}

-- | Duplicates a file info structure.
fileInfoDup :: FileInfoClass info => info -> IO FileInfo
fileInfoDup info =
    wrapNewGObject mkFileInfo $
    {#call g_file_info_dup#} (toFileInfo info)

-- | Copies all of the 'FileAttributes' from @srcInfo@ to @destInfo@.
fileInfoCopyInto :: (FileInfoClass srcInfo, FileInfoClass destInfo)
 => srcInfo  -- ^ @srcInfo@  source to copy attributes from.
 -> destInfo  -- ^ @destInfo@ destination to copy attributes to.
 -> IO ()
fileInfoCopyInto srcInfo destInfo =
  {#call g_file_info_copy_into#} (toFileInfo srcInfo) (toFileInfo destInfo)

-- | Checks if a file info structure has an attribute named attribute.
fileInfoHasAttribute :: (FileInfoClass info, GlibString string) => info
 -> string  -- ^ @attribute@ a file attribute key.
 -> Bool  -- ^ returns   'True' if Ginfo has an attribute named attribute, 'False' otherwise.
fileInfoHasAttribute info attribute =
  toBool $ unsafePerformIO $
  withUTFString attribute $ \ attributePtr ->
  {#call g_file_info_has_attribute#} (toFileInfo info) attributePtr

#if GLIB_CHECK_VERSION(2,22,0)
-- | Checks if a file info structure has an attribute in the specified @nameSpace@.
fileInfoHasNamespace :: (FileInfoClass info, GlibString string) => info
 -> string  -- ^ @namespace@ a file namespace key.
 -> Bool  -- ^ returns   'True' if Ginfo has an namespace named namespace, 'False' otherwise.
fileInfoHasNamespace info namespace =
  toBool $ unsafePerformIO $
  withUTFString namespace $ \ namespacePtr ->
  {#call g_file_info_has_namespace#} (toFileInfo info) namespacePtr
#endif

-- | Lists the file info structure's attributes.
fileInfoListAttributes :: (FileInfoClass info, GlibString string) => info
 -> string -- ^ @nameSpace@ a file attribute key's namespace.
 -> IO [string]-- ^ returns a array of strings of all of the possible attribute types for the given @nameSpace@
fileInfoListAttributes info nameSpace =
  withUTFString nameSpace $ \ nameSpacePtr ->
  {#call g_file_info_list_attributes#} (toFileInfo info) nameSpacePtr
  >>= readUTFStringArray0

-- | Gets the attribute type for an attribute key.
fileInfoGetAttributeType :: (FileInfoClass info, GlibString string) => info
 -> string -- ^ @attribute@ a file attribute key.
 -> IO FileAttributeType -- ^ returns a 'FileAttributeType' for the given attribute, or 'FileAttributeTypeInvalid' if the key is not set.
fileInfoGetAttributeType info attribute =
  liftM (toEnum . fromIntegral) $
  withUTFString attribute $ \ attributePtr ->
  {#call g_file_info_get_attribute_type#} (toFileInfo info) attributePtr

-- | Removes all cases of attribute from info if it exists.
fileInfoRemoveAttribute :: (FileInfoClass info, GlibString string) => info
 -> string -- ^ @attribute@ a file attribute key.
 -> IO ()
fileInfoRemoveAttribute info attribute =
  withUTFString attribute $ \ attributePtr ->
  {#call g_file_info_remove_attribute#} (toFileInfo info) attributePtr

-- | Gets the value of a attribute, formated as a string. This escapes things as needed to make the
-- string valid utf8.
fileInfoGetAttributeAsString :: (FileInfoClass info, GlibString string) => info
 -> string -- ^ @attribute@ a file attribute key.
 -> IO string -- ^ returns   a UTF-8 string associated with the given attribute.
fileInfoGetAttributeAsString info attribute =
  withUTFString attribute $ \ attributePtr ->
  {#call g_file_info_get_attribute_as_string#} (toFileInfo info) attributePtr
  >>= readUTFString

-- | Gets the attribute status for an attribute key.
fileInfoGetAttributeStatus :: (FileInfoClass info, GlibString string) => info
 -> string -- ^ @attribute@ a file attribute key.
 -> IO FileAttributeStatus -- ^ returns   a 'FileAttributeStatus' for the given attribute, or 'FileAttributeStatusUnset' if the key is invalid.
fileInfoGetAttributeStatus info attribute =
  liftM (toEnum . fromIntegral) $
  withUTFString attribute $ \ attributePtr ->
  {#call g_file_info_get_attribute_status#} (toFileInfo info) attributePtr

-- | Gets the value of a string attribute.
fileInfoGetAttributeString :: (FileInfoClass info, GlibString string) => info
 -> string -- ^ @attribute@ a file attribute key.
 -> IO string -- ^ returns   the contents of the attribute value as a string
fileInfoGetAttributeString info attribute =
  withUTFString attribute $ \ attributePtr ->
  {#call g_file_info_get_attribute_string#} (toFileInfo info) attributePtr
  >>= readUTFString

#if GLIB_CHECK_VERSION(2,22,0)
-- | Gets the value of a stringv attribute. If the attribute does not contain a stringv
fileInfoGetAttributeStringList :: (FileInfoClass info, GlibString string) => info
 -> string -- ^ @attribute@ a file attribute key.
 -> IO [string] -- ^ returns   the contents of the attribute value as a string list
fileInfoGetAttributeStringList info attribute =
  withUTFString attribute $ \ attributePtr ->
  {#call g_file_info_get_attribute_stringv#} (toFileInfo info) attributePtr
  >>= readUTFStringArray0
#endif

-- | Gets the value of a byte string attribute.
fileInfoGetAttributeByteString :: (FileInfoClass info, GlibString string) => info
 -> string -- ^ @attribute@ a file attribute key.
 -> IO ByteString -- ^ returns   the contents of the attribute value as a ByteString
fileInfoGetAttributeByteString info attribute =
  withUTFString attribute $ \ attributePtr -> do
  sPtr <- {#call g_file_info_get_attribute_byte_string#} (toFileInfo info) attributePtr
  sLen <- lengthArray0 0 sPtr
  unsafePackCStringFinalizer (castPtr sPtr) (fromIntegral sLen)
        ({#call unsafe g_free#} (castPtr sPtr))

-- | Gets the value of a boolean attribute.
fileInfoGetAttributeBool :: (FileInfoClass info, GlibString string) => info
 -> string -- ^ @attribute@ a file attribute key.
 -> IO Bool -- ^ returns   the contents of the attribute value as a bool
fileInfoGetAttributeBool info attribute =
  liftM toBool $
  withUTFString attribute $ \ attributePtr ->
  {#call g_file_info_get_attribute_boolean#} (toFileInfo info) attributePtr

-- | Gets an Word32 contained within the attribute.
fileInfoGetAttributeWord32 :: (FileInfoClass info, GlibString string) => info
 -> string -- ^ @attribute@ a file attribute key.
 -> IO Word32 -- ^ returns   the contents of the attribute value as a bool
fileInfoGetAttributeWord32 info attribute =
  liftM fromIntegral $
  withUTFString attribute $ \ attributePtr -> do
  {#call g_file_info_get_attribute_uint32#} (toFileInfo info) attributePtr

-- | Gets an Int32 contained within the attribute.
fileInfoGetAttributeInt32 :: (FileInfoClass info, GlibString string) => info
 -> string -- ^ @attribute@ a file attribute key.
 -> IO Int32 -- ^ returns   the contents of the attribute value as a bool
fileInfoGetAttributeInt32 info attribute =
  liftM fromIntegral $
  withUTFString attribute $ \ attributePtr -> do
  {#call g_file_info_get_attribute_int32#} (toFileInfo info) attributePtr

-- | Gets an Word64 contained within the attribute.
fileInfoGetAttributeWord64 :: (FileInfoClass info, GlibString string) => info
 -> string -- ^ @attribute@ a file attribute key.
 -> IO Word64 -- ^ returns   the contents of the attribute value as a bool
fileInfoGetAttributeWord64 info attribute =
  liftM fromIntegral $
  withUTFString attribute $ \ attributePtr -> do
  {#call g_file_info_get_attribute_uint64#} (toFileInfo info) attributePtr

-- | Gets an Int64 contained within the attribute.
fileInfoGetAttributeInt64 :: (FileInfoClass info, GlibString string) => info
 -> string -- ^ @attribute@ a file attribute key.
 -> IO Int64 -- ^ returns   the contents of the attribute value as a bool
fileInfoGetAttributeInt64 info attribute =
  liftM fromIntegral $
  withUTFString attribute $ \ attributePtr -> do
  {#call g_file_info_get_attribute_int64#} (toFileInfo info) attributePtr

-- | Gets the value of a GObject attribute.
fileInfoGetAttributeObject :: (FileInfoClass info, GlibString string) => info
 -> string -- ^ @attribute@ a file attribute key.
 -> IO (Maybe GObject)                 -- ^ returns   the contents of the attribute value as a object
fileInfoGetAttributeObject info attribute =
  withUTFString attribute $ \ attributePtr ->
    {#call g_file_info_get_attribute_object#} (toFileInfo info) attributePtr
    >>= \x -> maybeNull (makeNewGObject mkGObject) (return $ castPtr x)

#if GLIB_CHECK_VERSION(2,22,0)
-- | Sets the attribute status for an attribute key. This is only needed by external code that implement
-- 'fileSetAttributesFromInfo' or similar functions.
--
-- The attribute must exist in info for this to work. Otherwise 'False' is returned and info is
-- unchanged.
fileInfoSetAttributeStatus :: (FileInfoClass info, GlibString string) => info
 -> string -- ^ @attribute@ a file attribute key
 -> FileAttributeStatus -- ^ @status@    a 'FileAttributeStatus'
 -> IO Bool -- ^ returns   'True' if the status was changed, 'False' if the key was not set.
fileInfoSetAttributeStatus info attribute status =
   liftM toBool $
   withUTFString attribute $ \ attributePtr ->
   {#call g_file_info_set_attribute_status#}
     (toFileInfo info)
     attributePtr
     ((fromIntegral . fromEnum) status)
#endif

-- | Sets the attribute to contain the given @attrValue@, if possible.
fileInfoSetAttributeString :: (FileInfoClass info, GlibString string) => info
 -> string -- ^ @attribute@  a file attribute key.
 -> string -- ^ @attrValue@ a string.
 -> IO ()
fileInfoSetAttributeString info attribute attrValue =
  withUTFString attribute $ \ attributePtr ->
  withUTFString attrValue $ \ attrValuePtr ->
  {#call g_file_info_set_attribute_string#} (toFileInfo info) attributePtr attrValuePtr

#if GLIB_CHECK_VERSION(2,22,0)
-- | Sets the attribute to contain the given @attrValue@, if possible.
fileInfoSetAttributeStringList :: (FileInfoClass info, GlibString string) => info
 -> string -- ^ @attribute@  a file attribute key.
 -> [string] -- ^ @attrValue@ a string.
 -> IO ()
fileInfoSetAttributeStringList info attribute attrValue =
  withUTFString attribute $ \ attributePtr ->
  withUTFStringArray0 attrValue $ \ attrValuePtr ->
  {#call g_file_info_set_attribute_stringv#} (toFileInfo info) attributePtr attrValuePtr
#endif

-- | Sets the attribute to contain the given @attrValue@, if possible.
fileInfoSetAttributeByteString :: (FileInfoClass info, GlibString string) => info
 -> string -- ^ @attribute@  a file attribute key.
 -> ByteString -- ^ @attrValue@ a string.
 -> IO ()
fileInfoSetAttributeByteString info attribute attrValue =
  withUTFString attribute $ \ attributePtr ->
  useAsCString attrValue $ \ attrValuePtr ->
  {#call g_file_info_set_attribute_byte_string#} (toFileInfo info) attributePtr attrValuePtr

-- | Sets the attribute to contain the given @attrValue@, if possible.
fileInfoSetAttributeBool :: (FileInfoClass info, GlibString string) => info
 -> string -- ^ @attribute@  a file attribute key.
 -> Bool -- ^ @attrValue@ a string.
 -> IO ()
fileInfoSetAttributeBool info attribute attrValue =
  withUTFString attribute $ \ attributePtr ->
  {#call g_file_info_set_attribute_boolean#} (toFileInfo info) attributePtr (fromBool attrValue)

-- | Sets the attribute to contain the given @attrValue@, if possible.
fileInfoSetAttributeWord32 :: (FileInfoClass info, GlibString string) => info
 -> string -- ^ @attribute@  a file attribute key.
 -> Word32  -- ^ @attrValue@ an Word32
 -> IO ()
fileInfoSetAttributeWord32 info attribute attrValue =
  withUTFString attribute $ \ attributePtr ->
  {#call g_file_info_set_attribute_uint32#} (toFileInfo info) attributePtr (fromIntegral attrValue)

-- | Sets the attribute to contain the given @attrValue@, if possible.
fileInfoSetAttributeInt32 :: (FileInfoClass info, GlibString string) => info
 -> string -- ^ @attribute@  a file attribute key.
 -> Int32  -- ^ @attrValue@ an Int32
 -> IO ()
fileInfoSetAttributeInt32 info attribute attrValue =
  withUTFString attribute $ \ attributePtr ->
  {#call g_file_info_set_attribute_int32#} (toFileInfo info) attributePtr (fromIntegral attrValue)

-- | Sets the attribute to contain the given @attrValue@, if possible.
fileInfoSetAttributeWord64 :: (FileInfoClass info, GlibString string) => info
 -> string -- ^ @attribute@  a file attribute key.
 -> Word64  -- ^ @attrValue@ an Word64
 -> IO ()
fileInfoSetAttributeWord64 info attribute attrValue =
  withUTFString attribute $ \ attributePtr ->
  {#call g_file_info_set_attribute_uint64#} (toFileInfo info) attributePtr (fromIntegral attrValue)

-- | Sets the attribute to contain the given @attrValue@, if possible.
fileInfoSetAttributeInt64 :: (FileInfoClass info, GlibString string) => info
 -> string -- ^ @attribute@  a file attribute key.
 -> Int64  -- ^ @attrValue@ an Int64
 -> IO ()
fileInfoSetAttributeInt64 info attribute attrValue =
  withUTFString attribute $ \ attributePtr ->
  {#call g_file_info_set_attribute_int64#} (toFileInfo info) attributePtr (fromIntegral attrValue)

-- | Sets the attribute to contain the given @attrValue@, if possible.
fileInfoSetAttributeObject :: (FileInfoClass info, GlibString string) => info
 -> string
 -> GObject
 -> IO ()
fileInfoSetAttributeObject info attribute (GObject attrValue) =
  withUTFString attribute $ \ attributePtr ->
  withForeignPtr attrValue $ \attrValuePtr ->
  {#call g_file_info_set_attribute_object#} (toFileInfo info) attributePtr (castPtr attrValuePtr)

-- | Clears the status information from info.
--
fileInfoClearStatus :: FileInfoClass info => info -> IO ()
fileInfoClearStatus info =
  {#call g_file_info_clear_status #} (toFileInfo info)

-- | Gets a file's type (whether it is a regular file, symlink, etc). This is different from the file's
-- content type, see 'fileInfoGetContentType'.
--
fileInfoGetFileType :: FileInfoClass info => info
                    -> FileType  -- ^ returns a 'FileType' for the given file.
fileInfoGetFileType info =
    (toEnum . fromIntegral) $ unsafePerformIO $
        {#call g_file_info_get_file_type#} (toFileInfo info)

-- | Checks if a file is hidden.
fileInfoGetIsHidden :: FileInfoClass info => info
                    -> Bool -- ^ returns 'True' if the file is a hidden file, 'False' otherwise.
fileInfoGetIsHidden info =
    toBool $ unsafePerformIO $
          {#call g_file_info_get_is_hidden#} (toFileInfo info)

-- | Checks if a file is a backup file.
fileInfoGetIsBackup :: FileInfoClass info => info
                    -> Bool -- ^ returns 'True' if the file is a backup file, 'False' otherwise.
fileInfoGetIsBackup info =
    toBool $ unsafePerformIO $
          {#call g_file_info_get_is_backup#} (toFileInfo info)

-- | Checks if a file is a symlink file.
fileInfoGetIsSymlink :: FileInfoClass info => info
                    -> Bool -- ^ returns 'True' if the file is a symlink file, 'False' otherwise.
fileInfoGetIsSymlink info =
    toBool $ unsafePerformIO $
          {#call g_file_info_get_is_symlink#} (toFileInfo info)

-- | Gets the name for a file.
fileInfoGetName :: FileInfoClass info => info
                -> Maybe ByteString   -- ^ returns a string containing the file name.
fileInfoGetName info =
    unsafePerformIO $ do
    sPtr <- {#call g_file_info_get_name#} (toFileInfo info)
    if sPtr == nullPtr
       then return Nothing
       else do
         sLen <- lengthArray0 0 sPtr
         liftM Just $ unsafePackCStringFinalizer (castPtr sPtr) (fromIntegral sLen)
                          ({#call unsafe g_free#} (castPtr sPtr))

-- | Gets the display name for a file.
fileInfoGetDisplayName :: (FileInfoClass info, GlibString string) => info
                -> Maybe string   -- ^ returns a string containing the display name.
fileInfoGetDisplayName info =
    unsafePerformIO $
    {#call g_file_info_get_display_name#} (toFileInfo info)
    >>= maybePeek readUTFString

-- | Gets the edit name for a file.
fileInfoGetEditName :: (FileInfoClass info, GlibString string) => info
                -> Maybe string   -- ^ returns a string containing the edit name.
fileInfoGetEditName info =
    unsafePerformIO $
    {#call g_file_info_get_edit_name#} (toFileInfo info)
    >>= maybePeek readUTFString

-- | Gets the icon for a file.
fileInfoGetIcon :: FileInfoClass info => info
                -> IO Icon   -- ^ returns 'Icon' for the given info.
fileInfoGetIcon info =
    makeNewGObject mkIcon $
    {#call g_file_info_get_icon#} (toFileInfo info)

-- | Gets the file's content type.
fileInfoGetContentType :: (FileInfoClass info, GlibString string) => info
                       -> Maybe string -- ^ returns a string containing the file's content type.
fileInfoGetContentType info =
  unsafePerformIO $
  {#call g_file_info_get_content_type#} (toFileInfo info)
  >>= maybePeek peekUTFString

-- | Gets the modification time of the current info and sets it in result.
fileInfoGetModificationTime :: FileInfoClass info => info
 -> GTimeVal
fileInfoGetModificationTime info =
  unsafePerformIO $
  alloca $ \ timeValPtr -> do
     {#call g_file_info_get_modification_time#} (toFileInfo info) (castPtr timeValPtr)
     peek timeValPtr

-- | Gets the file's size.
fileInfoGetSize :: FileInfoClass info => info
                -> Int64  -- ^ returns a 'Offset' containing the file's size.
fileInfoGetSize info =
  fromIntegral $ unsafePerformIO $
  {#call g_file_info_get_size#} (toFileInfo info)

-- | Gets the symlink target for a given 'FileInfo'.
fileInfoGetSymlinkTarget :: (FileInfoClass info, GlibString string) => info
                         -> Maybe string -- ^ returns a string containing the symlink target.
fileInfoGetSymlinkTarget info =
  unsafePerformIO $
  {#call g_file_info_get_symlink_target#} (toFileInfo info)
  >>= maybePeek readUTFString

-- | Gets the entity tag for a given 'FileInfo'. See 'FileAttributeEtagValue'.
fileInfoGetEtag :: (FileInfoClass info, GlibString string) => info
                -> Maybe string -- ^ returns a string containing the value of the "etag:value" attribute.
fileInfoGetEtag info =
  unsafePerformIO $
  {#call g_file_info_get_etag#} (toFileInfo info)
  >>= maybePeek readUTFString

-- | Gets the value of the @sortOrder@ attribute from the 'FileInfo'. See
-- 'FileAttributeStandardSortOrder'.
fileInfoGetSortOrder :: FileInfoClass info => info
                     -> Int  -- ^ returns the value of the \"standard::sort_order\" attribute.
fileInfoGetSortOrder info =
  fromIntegral $ unsafePerformIO $
  {#call g_file_info_get_sort_order#} (toFileInfo info)

-- | Unsets a mask set by 'fileInfoSetAttributeMask', if one is set.
fileInfoUnsetAttributeMask :: FileInfoClass info => info -> IO ()
fileInfoUnsetAttributeMask info =
  {#call g_file_info_unset_attribute_mask#} (toFileInfo info)

-- | Sets the file type in a 'FileInfo' to type. See 'FileAttributeStandardType'.
fileInfoSetFileType :: FileInfoClass info => info -> FileType -> IO ()
fileInfoSetFileType info fType =
  {#call g_file_info_set_file_type#}
  (toFileInfo info)
  ((fromIntegral . fromEnum) fType)

-- | Sets the @isHidden@ attribute in a 'FileInfo' according to @isSymlink@. See
-- 'FileAttributeStandardIsHidden'.
fileInfoSetIsHidden :: FileInfoClass info => info -> Bool -> IO ()
fileInfoSetIsHidden info isHidden =
  {#call g_file_info_set_is_hidden#} (toFileInfo info) (fromBool isHidden)

-- | Sets the @isSymlink@ attribute in a 'FileInfo' according to @isSymlink@. See
-- 'FileAttributeStandardIsSymlink'.
fileInfoSetIsSymlink :: FileInfoClass info => info -> Bool -> IO ()
fileInfoSetIsSymlink info isSymlink =
  {#call g_file_info_set_is_symlink#} (toFileInfo info) (fromBool isSymlink)

-- | Sets the name attribute for the current 'FileInfo'. See 'FileAttributeStandardName'.
fileInfoSetName :: FileInfoClass info => info -> ByteString -> IO ()
fileInfoSetName info name =
  useAsCString name $ \ namePtr ->
  {#call g_file_info_set_name#} (toFileInfo info) namePtr

-- | Sets the display name for the current 'FileInfo'. See 'FileAttributeStandardDisplayName'.
fileInfoSetDisplayName :: (FileInfoClass info, GlibString string) => info -> string -> IO ()
fileInfoSetDisplayName info displayName =
  withUTFString displayName $ \ displayNamePtr ->
  {#call g_file_info_set_display_name#} (toFileInfo info) displayNamePtr

-- | Sets the edit name for the current 'FileInfo'. See 'FileAttributeStandardEditName'.
fileInfoSetEditName :: (FileInfoClass info, GlibString string) => info -> string -> IO ()
fileInfoSetEditName info editName =
  withUTFString editName $ \ editNamePtr ->
  {#call g_file_info_set_edit_name#} (toFileInfo info) editNamePtr

-- | Sets the icon for a given 'FileInfo'. See 'FileAttributeStandardIcon'.
fileInfoSetIcon :: FileInfoClass info => info -> Icon -> IO ()
fileInfoSetIcon info icon =
    {#call g_file_info_set_icon#} (toFileInfo info) (toIcon icon)

-- | Sets the content type attribute for a given 'FileInfo'. See 'FileAttributeStandardContentType'.
fileInfoSetContentType :: (FileInfoClass info, GlibString string) => info
                       -> string   -- ^ @contentType@ a content type. See GContentType.
                       -> IO ()
fileInfoSetContentType info contentType =
  withUTFString contentType $ \ contentTypePtr ->
  {#call g_file_info_set_content_type#} (toFileInfo info) contentTypePtr

-- | Sets the 'FileAttributeTimeModified' attribute in the file info to the given time value.
fileInfoSetModificationTime :: FileInfoClass info => info
 -> GTimeVal
 -> IO ()
fileInfoSetModificationTime info mtime =
  with mtime $ \ mtimePtr ->
  {#call g_file_info_set_modification_time#} (toFileInfo info) (castPtr mtimePtr)

-- | Sets the 'FileAttributeStandardSize' attribute in the file info to the given size.
fileInfoSetSize :: FileInfoClass info => info
                -> Int64  -- ^ @size@ a goffset containing the file's size.
                -> IO ()
fileInfoSetSize info size =
  {#call g_file_info_set_size#} (toFileInfo info) (fromIntegral size)

-- | Sets the 'FileAttributeStandardSymlinkTarget' attribute in the file info to the given symlink
-- target.
-- fileInfoSetSymlinkTarget :: FileInfoClass info => info
--                          -> String  -- ^ @symlinkTarget@ a static string containing a path to a symlink target.
--                          -> IO ()
-- fileInfoSetSymlinkTarget info symlinkTarget =
--   withUTFString symlinkTarget $ \ symlinkTargetPtr ->
--   {#call g_file_info_set_symlink_target#} (toFileInfo info) symlinkTargetPtr

-- | Sets the sort order attribute in the file info structure. See 'FileAttributeStandardSortOrder'.
fileInfoSetSortOrder :: FileInfoClass info => info
                     -> Int   -- ^ @sortOrder@ a sort order integer.
                     -> IO ()
fileInfoSetSortOrder info sortOrder =
  {#call g_file_info_set_sort_order#} (toFileInfo info) (fromIntegral sortOrder)
