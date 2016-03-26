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

{#import Graphics.UI.Gtk.General.DNDTypes#}

import Control.Monad ( liftM )
import Control.Monad.Trans ( liftIO )
import Control.Monad.Reader (runReaderT, ask)

{# context lib="gtk" prefix="gtk" #}

-- %hash c:7662 d:af3f
-- | Stores new data in the 'SelectionDataM' monad. The stored data may only
-- be an array of integer types that are no larger than 32 bits.
--
selectionDataSet :: (Integral a, Storable a) => SelectionTypeTag -> [a] ->
                                                SelectionDataM ()
selectionDataSet (Atom tagPtr) values@(~(v:_)) = ask >>= \sel ->
  liftIO $ withArrayLen values $ \arrayLen arrayPtr ->
  selectionDataSet
  {#call unsafe gtk_selection_data_set #} selPtr tagPtr (fromIntegral (8*sizeOf v))
    (castPtr arrayPtr) (fromIntegral (arrayLen*sizeOf v))

selectionDataGetLength :: SelectionDataM Int32
selectionDataGetLength = do
  sel <- ask
  liftIO $ G.selectionDataGetLength sel

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
selectionDataSetText :: Text -> SelectionDataM Bool
selectionDataSetText str = do
  sel <- ask
  G.selectionData sel str

-- %hash c:90e0 d:af3f
-- | Gets the contents of the selection data as a string.
--
selectionDataGetText :: SelectionDataM Text
selectionDataGetText = do
  sel <- ask
  G.selectionDataGetText sel

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
