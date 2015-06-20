{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Selection support
--
--  Author : Axel Simon
--
--  Created: 26 March 2007
--
--  Copyright (C) 2007 Axel Simon
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
-- functions that seem to be internal: gtk_selection_convert
-- functions that relate to target tables are not bound since they seem
-- superfluous: targets_*, selection_data_copy, selection_data_free
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Functions for handling inter-process communication via selections.
--
module Graphics.UI.Gtk.General.Selection (

-- * Types
  InfoId,
  Atom,
  TargetTag,
  SelectionTag,
  SelectionTypeTag,
  TargetList,
  SelectionDataM,
  TargetFlags(..),

-- * Constants
  targetString,
  selectionTypeAtom,
  selectionTypeInteger,
  selectionTypeString,

-- * Constructors
  atomNew,
  targetListNew,

-- * Methods
  targetListAdd,
#if GTK_CHECK_VERSION(2,6,0)
  targetListAddTextTargets,
  targetListAddImageTargets,
  targetListAddUriTargets,
#endif
#if GTK_CHECK_VERSION(2,10,0)
  targetListAddRichTextTargets,
#endif
  targetListRemove,

  selectionAddTarget,
  selectionClearTargets,
  selectionOwnerSet,
  selectionOwnerSetForDisplay,
  selectionRemoveAll,

  selectionDataSet,
#if GTK_MAJOR_VERSION < 3
  selectionDataGet,
#endif
  selectionDataIsValid,
  selectionDataSetText,
  selectionDataGetText,
#if GTK_CHECK_VERSION(2,6,0)
  selectionDataSetPixbuf,
  selectionDataGetPixbuf,
  selectionDataSetURIs,
  selectionDataGetURIs,
  selectionDataTargetsIncludeImage,
#endif
  selectionDataGetTarget,
#if GTK_MAJOR_VERSION < 3
  selectionDataSetTarget,
#endif
  selectionDataGetTargets,
  selectionDataTargetsIncludeText,
#if GTK_CHECK_VERSION(2,10,0)
  selectionDataTargetsIncludeUri,
  selectionDataTargetsIncludeRichText,
#endif

-- * Signals
  selectionGet,
  selectionReceived

  ) where

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Flags        (fromFlags)
import System.Glib.Signals
import System.Glib.GObject
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.General.DNDTypes#}
import Graphics.UI.Gtk.Gdk.Events (TimeStamp)
import Graphics.UI.Gtk.General.Enums (TargetFlags(..))
import Graphics.UI.Gtk.General.Structs (
  targetString,
  selectionTypeAtom,
  selectionTypeInteger,
  selectionTypeString,
#if GTK_MAJOR_VERSION < 3
  selectionDataGetType
#endif
  )

import Graphics.UI.Gtk.Signals
import Control.Monad ( liftM )
import Control.Monad.Trans ( liftIO )
import Control.Monad.Reader (runReaderT, ask)

{# context lib="gtk" prefix="gtk" #}


--------------------
-- Methods

-- | Append another target to the given 'TargetList'.
--
-- * Note that the 'TargetFlags' are only used for drag and drop, not in normal
--   selection handling.
--
targetListAdd :: TargetList -> TargetTag -> [TargetFlags] -> InfoId -> IO ()
targetListAdd tl (Atom tagPtr) flags info = do
  {#call unsafe target_list_add#} tl tagPtr (fromIntegral (fromFlags flags)) info

#if GTK_CHECK_VERSION(2,6,0)

-- | Append all text targets supported by the selection mechanism to the
-- target list. All targets are added with the same 'InfoId'.
--
-- * Since Gtk 2.6.
--
targetListAddTextTargets :: TargetList -> InfoId -> IO ()
targetListAddTextTargets = {#call unsafe target_list_add_text_targets#}

-- | Append all image targets supported by the selection mechanism to the
-- target list. All targets are added with the same 'InfoId'. If the boolean
-- flag is set, only targets will be added which Gtk+ knows how to convert
-- into a 'Graphics.UI.Gtk.Pixbuf.Pixbuf'.
--
-- * Since Gtk 2.6.
--
targetListAddImageTargets :: TargetList -> InfoId -> Bool -> IO ()
targetListAddImageTargets tl info writable =
  {#call unsafe target_list_add_image_targets#} tl info (fromBool writable)

-- | Append all URI (universal resource indicator, fomerly URL) targets
-- supported by the selection mechanism to the target list. All targets are
-- added with the same 'InfoId'.
--
-- * Since Gtk 2.6.
--
targetListAddUriTargets :: TargetList -> InfoId -> IO ()
targetListAddUriTargets = {#call unsafe target_list_add_uri_targets#}

#endif
#if GTK_CHECK_VERSION(2,10,0)

-- | Append all rich text targets registered with
-- 'Graphics.UI.Gtk.TextBuffer.textBufferRegisterSerializeFormat' to the
-- target list. All targets are added with the same 'InfoId'. If the boolean
-- flag is @True@ then deserializable rich text formats will be added,
-- serializable formats otherwise.
--
-- * Since Gtk 2.10.
--
targetListAddRichTextTargets :: TextBufferClass tb =>
  TargetList -> InfoId -> Bool -> tb -> IO ()
targetListAddRichTextTargets tl info deser tb =
  {#call unsafe target_list_add_rich_text_targets#} tl info
    (fromBool deser) (toTextBuffer tb)

#endif

-- | Remove a target from a target list.
--
targetListRemove :: TargetList -> TargetTag -> IO ()
targetListRemove tl (Atom t)= {#call unsafe target_list_remove#} tl t


-- %hash c:9971 d:af3f
-- | Appends a specified target to the list of supported targets for a given
-- widget and selection.
--
selectionAddTarget :: WidgetClass widget => widget -> SelectionTag ->
                      TargetTag -> InfoId -> IO ()
selectionAddTarget widget (Atom selection) (Atom target) info =
  {#call unsafe gtk_selection_add_target #}
    (toWidget widget)
    selection
    target
    (fromIntegral info)

-- %hash c:d523 d:af3f
-- | Remove all targets registered for the given selection for the widget.
--
selectionClearTargets :: WidgetClass widget => widget -> SelectionTag -> IO ()
selectionClearTargets widget (Atom selection) =
  {#call unsafe gtk_selection_clear_targets #}
    (toWidget widget)
    selection

-- %hash c:85a8 d:af3f
-- | Claims ownership of a given selection for a particular widget, or, if
-- widget is 'Nothing', release ownership of the selection.
--
selectionOwnerSet :: WidgetClass widget => Maybe widget -> SelectionTag ->
  TimeStamp -> IO Bool
selectionOwnerSet widget (Atom selection) time =
  liftM toBool $
  {#call unsafe gtk_selection_owner_set #}
    (maybe (Widget nullForeignPtr) toWidget widget)
    selection
    (fromIntegral time)

-- %hash c:174 d:af3f
-- | Set the ownership of a given selection and display.
--
selectionOwnerSetForDisplay :: WidgetClass widget => Display -> Maybe widget ->
  SelectionTag -> TimeStamp -> IO Bool
selectionOwnerSetForDisplay display widget (Atom selection) time =
 liftM toBool $
  {#call unsafe gtk_selection_owner_set_for_display #}
    display
    (maybe (Widget nullForeignPtr) toWidget widget)
    selection
    (fromIntegral time)

-- %hash c:c29 d:af3f
-- | Removes all handlers and unsets ownership of all selections for a widget.
-- Called when widget is being destroyed. This function will not generally be
-- called by applications.
--
selectionRemoveAll :: WidgetClass widget => widget -> IO ()
selectionRemoveAll widget =
  {#call unsafe gtk_selection_remove_all #}
    (toWidget widget)

-- %hash c:7662 d:af3f
-- | Stores new data in the 'SelectionDataM' monad. The stored data may only
-- be an array of integer types that are no larger than 32 bits.
--
selectionDataSet :: (Integral a, Storable a) => SelectionTypeTag -> [a] ->
                                                SelectionDataM ()
selectionDataSet (Atom tagPtr) values@(~(v:_)) = ask >>= \selPtr ->
  liftIO $ withArrayLen values $ \arrayLen arrayPtr ->
  {#call unsafe gtk_selection_data_set #} selPtr tagPtr (fromIntegral (8*sizeOf v))
    (castPtr arrayPtr) (fromIntegral (arrayLen*sizeOf v))

-- The GtkSelectionData struct was made opaque in Gtk3, but the accessor routines
-- where introduced in 2.14.
#if GTK_CHECK_VERSION(2,14,0)
#if GTK_MAJOR_VERSION < 3
selectionDataGet_format selPtr = {#call gtk_selection_data_get_format#} selPtr
#endif
selectionDataGet_length selPtr = {#call gtk_selection_data_get_length#} selPtr
#if GTK_MAJOR_VERSION < 3
selectionDataGet_data selPtr = {#call gtk_selection_data_get_data#} selPtr
#endif
selectionDataGet_target selPtr = {#call gtk_selection_data_get_target#} selPtr
#else
selectionDataGet_format selPtr = {#get SelectionData -> format#} selPtr
selectionDataGet_length selPtr = {#get SelectionData -> length#} selPtr
selectionDataGet_data selPtr = {#get SelectionData -> data#} selPtr
selectionDataGet_target selPtr = {#get SelectionData -> target#} selPtr
#endif

#if GTK_MAJOR_VERSION < 3
-- | Retreives the data in the 'SelectionDataM' monad. The returned array
--   must have elements of the size that were used to set this data. If
--   the size or the type tag does not match, @Nothing@ is returned.
--
-- Removed in Gtk3.
selectionDataGet :: (Integral a, Storable a) =>
                    SelectionTypeTag -> SelectionDataM (Maybe [a])
selectionDataGet tagPtr = do
  selPtr <- ask
  liftIO $ do
    typeTag <- selectionDataGetType selPtr
    if typeTag/=tagPtr then return Nothing else do
    bitSize <- liftM fromIntegral $ selectionDataGet_format selPtr
    lenBytes <- liftM fromIntegral $ selectionDataGet_length selPtr
    dataPtr <- liftM castPtr $ selectionDataGet_data selPtr
    if lenBytes<=0 || bitSize/=sizeOf (unsafePerformIO (peek dataPtr))*8
      then return Nothing
      else liftM Just $ do
        peekArray (fromIntegral (lenBytes `quot` (bitSize `quot` 8))) dataPtr
#endif

selectionDataGetLength :: SelectionDataM Int
selectionDataGetLength = do
  selPtr <- ask
  liftIO $ liftM fromIntegral $ selectionDataGet_length selPtr

-- | Check if the currently stored data is valid.
--
-- * If this function returns @False@, no data is set in this selection
--   and 'selectionDataGet' will return @Nothing@ no matter what type
--   is requested.
--
selectionDataIsValid :: SelectionDataM Bool
selectionDataIsValid = do
  len <- selectionDataGetLength
  return (len>=0)

-- %hash c:9bdf d:af3f
-- | Sets the contents of the selection from a string. The
-- string is converted to the form determined by the allowed targets of the
-- selection.
--
-- * Returns @True@ if setting the text was successful.
--
selectionDataSetText :: GlibString string => string -> SelectionDataM Bool
selectionDataSetText str = do
  selPtr <- ask
  liftM toBool $ liftIO $ withUTFStringLen str $ \(strPtr,len) ->
    {#call unsafe gtk_selection_data_set_text #} selPtr strPtr (fromIntegral len)

-- %hash c:90e0 d:af3f
-- | Gets the contents of the selection data as a string.
--
selectionDataGetText :: GlibString string => SelectionDataM (Maybe string)
selectionDataGetText = do
  selPtr <- ask
  liftIO $ do
    strPtr <- {#call unsafe gtk_selection_data_get_text #} selPtr
    if strPtr==nullPtr then return Nothing else do
      str <- peekUTFString (castPtr strPtr)
      {#call unsafe g_free#} (castPtr strPtr)
      return (Just str)

#if GTK_CHECK_VERSION(2,6,0)
-- %hash c:ed8d d:af3f
-- | Sets the contents of the selection from a 'Pixbuf'. The pixbuf is
-- converted to the form determined by the allowed targets of the selection.
--
-- * Returns @True@ if setting the 'Pixbuf' was successful. Since Gtk 2.6.
--
selectionDataSetPixbuf :: Pixbuf -> SelectionDataM Bool
selectionDataSetPixbuf pixbuf = do
  selPtr <- ask
  liftM toBool $ liftIO $
    {#call unsafe gtk_selection_data_set_pixbuf #} selPtr pixbuf

-- %hash c:52cd d:af3f
-- | Gets the contents of the selection data as a 'Pixbuf'.
--
-- * Since Gtk 2.6.
--
selectionDataGetPixbuf :: SelectionDataM (Maybe Pixbuf)
selectionDataGetPixbuf = do
  selPtr <- ask
  liftIO $ maybeNull (wrapNewGObject mkPixbuf) $
    {#call unsafe gtk_selection_data_get_pixbuf #} selPtr

-- %hash c:d222 d:af3f
-- | Sets the contents of the selection from a list of URIs. The string is
-- converted to the form determined by the possible targets of the selection.
--
-- * Returns @True@ if setting the URIs was successful. Since Gtk 2.6.
--
selectionDataSetURIs :: GlibString string => [string] -> SelectionDataM Bool
selectionDataSetURIs uris = do
  selPtr <- ask
  liftIO $ liftM toBool $ withUTFStringArray0 uris $ \strPtrPtr ->
      {#call unsafe gtk_selection_data_set_uris #} selPtr strPtrPtr

-- %hash c:472f d:af3f
-- | Gets the contents of the selection data as list of URIs. Returns
-- @Nothing@ if the selection did not contain any URIs.
--
-- * Since Gtk 2.6.
--
selectionDataGetURIs :: GlibString string => SelectionDataM (Maybe [string])
selectionDataGetURIs = do
  selPtr <- ask
  liftIO $ do
    strPtrPtr <- {#call unsafe gtk_selection_data_get_uris #} selPtr
    if strPtrPtr==nullPtr then return Nothing else do
      uris <- peekUTFStringArray0 strPtrPtr
      {#call unsafe g_strfreev#} strPtrPtr
      return (Just uris)
#endif

-- | Retrieve the currently set 'TargetTag' in the selection.
selectionDataGetTarget :: SelectionDataM TargetTag
selectionDataGetTarget = do
  selPtr <- ask
  liftM Atom $ liftIO $ selectionDataGet_target selPtr

#if GTK_MAJOR_VERSION < 3
-- | Set the selection to the given 'TargetTag'.
--
-- Removed in Gtk3.
selectionDataSetTarget :: TargetTag -> SelectionDataM ()
selectionDataSetTarget (Atom targetTag) = do
  selPtr <- ask
  liftIO $ {#set SelectionData -> target#} selPtr targetTag
#endif

-- %hash c:e659 d:af3f
-- | Queries the content type of the selection data as a list of targets.
--   Whenever the application is asked whether certain targets are acceptable,
--   it is handed a selection that contains a list of 'TargetTag's as payload.
--   A similar result could be achieved using 'selectionDataGet
--   selectionTypeAtom'.
--
selectionDataGetTargets :: SelectionDataM [TargetTag]
selectionDataGetTargets = do
  selPtr <- ask
  liftIO $ alloca $ \nAtomsPtr -> alloca $ \targetPtrPtr -> do
    valid <- liftM toBool $
      {#call unsafe gtk_selection_data_get_targets #} selPtr targetPtrPtr nAtomsPtr
    if not valid then return [] else do
      len <- peek nAtomsPtr
      targetPtr <- peek targetPtrPtr
      targetPtrs <- peekArray (fromIntegral len) targetPtr
      {#call unsafe g_free#} (castPtr targetPtr)
      return (map Atom targetPtrs)

#if GTK_CHECK_VERSION(2,6,0)
-- %hash c:5a8 d:af3f
-- | Given a 'SelectionDataM' holding a list of targets, determines if any of
-- the targets in targets can be used to provide a 'Pixbuf'.
--
-- * Since Gtk 2.6
--
selectionDataTargetsIncludeImage ::
  Bool -- ^ whether to accept only targets for which GTK+ knows how to convert a
       -- pixbuf into the format
  -> SelectionDataM Bool
selectionDataTargetsIncludeImage writable = do
  selPtr <- ask
  liftM toBool $ liftIO $
    {#call unsafe gtk_selection_data_targets_include_image #}
    selPtr
    (fromBool writable)
#endif

-- %hash c:abe8 d:af3f
-- | Given a 'SelectionDataM' holding a list of targets, determines if any of
-- the targets in targets can be used to provide text.
--
selectionDataTargetsIncludeText :: SelectionDataM Bool
selectionDataTargetsIncludeText = do
  selPtr <- ask
  liftM toBool $ liftIO $
    {#call unsafe gtk_selection_data_targets_include_text #}
    selPtr

#if GTK_CHECK_VERSION(2,10,0)
-- | Given a 'SelectionDataM' holding a list of targets, determines if any of
-- the targets in targets can be used to provide URIs.
--
-- * Since Gtk 2.10
--
selectionDataTargetsIncludeUri :: SelectionDataM Bool
selectionDataTargetsIncludeUri = do
  selPtr <- ask
  liftM toBool $ liftIO $
    {#call unsafe gtk_selection_data_targets_include_uri #}
    selPtr

-- | Given a 'SelectionDataM' holding a list of targets, check if,
--   well, dunno really. FIXME: what does the 'TextBuffer' do?
--
-- * Since Gtk 2.10
--
selectionDataTargetsIncludeRichText :: TextBufferClass tb => tb ->
                                       SelectionDataM Bool
selectionDataTargetsIncludeRichText tb = do
  selPtr <- ask
  liftM toBool $ liftIO $
    {#call unsafe gtk_selection_data_targets_include_rich_text #}
    selPtr (toTextBuffer tb)
#endif

--------------------
-- Signals

-- %hash c:f7c3 d:af3f
-- | Pass the supplied selection data to the application. The application is
-- expected to read the data using 'selectionDataGet' or one of its
-- derivatives.
--
selectionReceived :: WidgetClass self => Signal self (TimeStamp -> SelectionDataM ())
selectionReceived = Signal (\after object handler -> do
    connect_PTR_WORD__NONE "selection-received" after object $ \dataPtr time -> do
      runReaderT (handler (fromIntegral time)) dataPtr >> return ())

-- %hash c:c3 d:af3f
-- | Emitted in order to ask the application for selection data. Within the
-- handler the function 'selectionDataSet' or one of its derivatives should be
-- called.
--
selectionGet :: WidgetClass self =>
                Signal self (InfoId -> TimeStamp -> SelectionDataM ())
selectionGet = Signal (\after object handler -> do
    connect_PTR_WORD_WORD__NONE "selection-get" after object $
      \dataPtr info time -> do
      runReaderT (handler (fromIntegral info) (fromIntegral time)) dataPtr >>
                  return ())
