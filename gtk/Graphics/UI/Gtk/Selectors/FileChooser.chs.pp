-- -*-haskell-*-
--  GIMP Toolkit (GTK) FileChooser
--
--  Author : Duncan Coutts
--
--  Created: 24 April 2004
--
--  Version $Revision: 1.3 $ from $Date: 2005/02/13 16:25:57 $
--
--  Copyright (C) 2004-2005 Duncan Coutts
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
--  The file chooser dialog and widget is a replacement
--  for the old "FileSel"ection dialog. It provides a better user
--  interface and an improved API.
--
--  The FileChooser (as opposed to the dialog or widget) is the interface that
--  the "FileChooserDialog" and "FileChooserWidget" implement, all the operations
--  except construction are on this interface.
--
-- * Added in GTK+ 2.4
--
module Graphics.UI.Gtk.Selectors.FileChooser (
#if GTK_CHECK_VERSION(2,4,0)
  FileChooserClass,
  FileChooser,
  FileChooserAction(..),
  fileChooserSetAction,
  fileChooserGetAction,
  fileChooserSetLocalOnly,
  fileChooserGetLocalOnly,
  fileChooserSetSelectMultiple,
  fileChooserGetSelectMultiple,
  fileChooserSetCurrentName,
  fileChooserGetFilename,
  fileChooserSetFilename,
  fileChooserSelectFilename,
  fileChooserUnselectFilename,
  fileChooserSelectAll,
  fileChooserUnselectAll,
  fileChooserGetFilenames,
  fileChooserSetCurrentFolder,
  fileChooserGetCurrentFolder,
  fileChooserGetURI,
  fileChooserSetURI,
  fileChooserSelectURI,
  fileChooserUnselectURI,
  fileChooserGetURIs,
  fileChooserSetCurrentFolderURI,
  fileChooserGetCurrentFolderURI,
  fileChooserSetPreviewWidget,
  fileChooserGetPreviewWidget,
  fileChooserSetPreviewWidgetActive,
  fileChooserGetPreviewWidgetActive,
  fileChooserSetUsePreviewLabel,
  fileChooserGetUsePreviewLabel,
  fileChooserGetPreviewFilename,
  fileChooserGetPreviewURI,
  fileChooserSetExtraWidget,
  fileChooserGetExtraWidget,
  fileChooserAddFilter,
  fileChooserRemoveFilter,
  fileChooserListFilters,
  fileChooserSetFilter,
  fileChooserGetFilter,
  fileChooserAddShortcutFolder,
  fileChooserRemoveShortcutFolder,
  fileChooserlistShortcutFolders,
  fileChooserAddShortcutFolderURI,
  fileChooserRemoveShortcutFolderURI,
  fileChooserListShortcutFolderURIs,
  onCurrentFolderChanged,
  afterCurrentFolderChanged,
  onFileActivated,
  afterFileActivated,
--  onSelectionChanged,
--  afterSelectionChanged,
  onUpdatePreview,
  afterUpdatePreview
#endif
) where

#if GTK_CHECK_VERSION(2,4,0)

import Monad (liftM, when)

import System.Glib.FFI
import System.Glib.UTFString		(readCString)
{#import Graphics.UI.Gtk.Types#}
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
import Graphics.UI.Gtk.Signals
{#import System.Glib.GList#}
import System.Glib.GError		(propagateGError, GErrorDomain, GErrorClass(..))

{# context lib="gtk" prefix ="gtk" #}

{# enum FileChooserAction {underscoreToCase} #}
{# enum FileChooserError {underscoreToCase} #}

fileChooserErrorDomain :: GErrorDomain
fileChooserErrorDomain = unsafePerformIO {#call unsafe file_chooser_error_quark#}
                                                                                                         
instance GErrorClass FileChooserError where
  gerrorDomain _ = fileChooserErrorDomain

fileChooserSetAction :: FileChooserClass chooser => chooser -> FileChooserAction -> IO ()
fileChooserSetAction chooser action =
  {# call gtk_file_chooser_set_action #} (toFileChooser chooser)
    (fromIntegral $ fromEnum action)

fileChooserGetAction :: FileChooserClass chooser => chooser ->  IO FileChooserAction
fileChooserGetAction chooser = liftM (toEnum . fromIntegral) $
  {# call gtk_file_chooser_get_action #} (toFileChooser chooser)

fileChooserSetLocalOnly :: FileChooserClass chooser => chooser -> Bool -> IO ()
fileChooserSetLocalOnly chooser localOnly = 
  {# call gtk_file_chooser_set_local_only #} (toFileChooser chooser)
    (fromBool localOnly)

fileChooserGetLocalOnly :: FileChooserClass chooser => chooser -> IO Bool
fileChooserGetLocalOnly chooser = liftM toBool $
  {# call gtk_file_chooser_get_local_only #} (toFileChooser chooser)

fileChooserSetSelectMultiple :: FileChooserClass chooser => chooser -> Bool -> IO ()
fileChooserSetSelectMultiple chooser selectMultiple = 
  {# call gtk_file_chooser_set_select_multiple #} (toFileChooser chooser)
    (fromBool selectMultiple)

fileChooserGetSelectMultiple :: FileChooserClass chooser => chooser -> IO Bool
fileChooserGetSelectMultiple chooser = liftM toBool $
  {# call gtk_file_chooser_get_select_multiple #} (toFileChooser chooser)

fileChooserSetCurrentName :: FileChooserClass chooser => chooser -> String -> IO ()
fileChooserSetCurrentName chooser name =
  withCString name $ \strPtr ->
  {# call gtk_file_chooser_set_current_name #} (toFileChooser chooser) strPtr

fileChooserGetFilename :: FileChooserClass chooser => chooser -> IO (Maybe String)
fileChooserGetFilename chooser = do
#if defined (WIN32) && GTK_CHECK_VERSION(2,6,0)
  strPtr <- {# call gtk_file_chooser_get_filename_utf8 #}
#else
  strPtr <- {# call gtk_file_chooser_get_filename #} 
#endif
   (toFileChooser chooser)
  maybePeek readCString strPtr

fileChooserSetFilename :: FileChooserClass chooser => chooser -> String -> IO Bool
fileChooserSetFilename chooser filename = liftM toBool $
  withCString filename $ \strPtr ->
#if defined (WIN32) && GTK_CHECK_VERSION(2,6,0)
  {# call gtk_file_chooser_set_filename_utf8 #} (toFileChooser chooser) strPtr
#else
  {# call gtk_file_chooser_set_filename #} (toFileChooser chooser) strPtr
#endif

fileChooserSelectFilename :: FileChooserClass chooser => chooser -> String -> IO Bool
fileChooserSelectFilename chooser filename = liftM toBool $
  withCString filename $ \strPtr ->
#if defined (WIN32) && GTK_CHECK_VERSION(2,6,0)
  {# call gtk_file_chooser_select_filename_utf8 #}
#else
  {# call gtk_file_chooser_select_filename #}
#endif
    (toFileChooser chooser) strPtr

fileChooserUnselectFilename :: FileChooserClass chooser => chooser -> String -> IO ()
fileChooserUnselectFilename chooser filename = 
  withCString filename $ \strPtr ->
#if defined (WIN32) && GTK_CHECK_VERSION(2,6,0)
  {# call gtk_file_chooser_unselect_filename_utf8 #}
#else
  {# call gtk_file_chooser_unselect_filename #}
#endif
    (toFileChooser chooser) strPtr

fileChooserSelectAll :: FileChooserClass chooser => chooser -> IO ()
fileChooserSelectAll chooser = 
  {# call gtk_file_chooser_select_all #} (toFileChooser chooser)

fileChooserUnselectAll :: FileChooserClass chooser => chooser -> IO ()
fileChooserUnselectAll chooser = 
  {# call gtk_file_chooser_unselect_all #} (toFileChooser chooser)

fileChooserGetFilenames :: FileChooserClass chooser => chooser -> IO [String]
fileChooserGetFilenames chooser = do
#if defined (WIN32) && GTK_CHECK_VERSION(2,6,0)
  strList <- {# call gtk_file_chooser_get_filenames_utf8 #}
#else
  strList <- {# call gtk_file_chooser_get_filenames #}
#endif
    (toFileChooser chooser)
  fromStringGSList strList

fileChooserSetCurrentFolder :: FileChooserClass chooser => chooser -> String -> IO Bool
fileChooserSetCurrentFolder chooser foldername = liftM toBool $
  withCString foldername $ \strPtr ->
#if defined (WIN32) && GTK_CHECK_VERSION(2,6,0)
  {# call gtk_file_chooser_set_current_folder_utf8 #}
#else
  {# call gtk_file_chooser_set_current_folder #}
#endif
    (toFileChooser chooser) strPtr

fileChooserGetCurrentFolder :: FileChooserClass chooser => chooser -> IO (Maybe String)
fileChooserGetCurrentFolder chooser = do
#if defined (WIN32) && GTK_CHECK_VERSION(2,6,0)
  strPtr <- {# call gtk_file_chooser_get_current_folder_utf8 #}
#else
  strPtr <- {# call gtk_file_chooser_get_current_folder #}
#endif
    (toFileChooser chooser)
  maybePeek readCString strPtr

fileChooserGetURI :: FileChooserClass chooser => chooser -> IO (Maybe String)
fileChooserGetURI chooser = do
  strPtr <- {# call gtk_file_chooser_get_uri #} (toFileChooser chooser)
  maybePeek readCString strPtr

fileChooserSetURI :: FileChooserClass chooser => chooser -> String -> IO Bool
fileChooserSetURI chooser uri = liftM toBool $
  withCString uri $ \strPtr ->
  {# call gtk_file_chooser_set_uri #} (toFileChooser chooser) strPtr

fileChooserSelectURI :: FileChooserClass chooser => chooser -> String -> IO Bool
fileChooserSelectURI chooser uri = liftM toBool $
  withCString uri $ \strPtr ->
  {# call gtk_file_chooser_select_uri #} (toFileChooser chooser) strPtr

fileChooserUnselectURI :: FileChooserClass chooser => chooser -> String -> IO ()
fileChooserUnselectURI chooser uri = 
  withCString uri $ \strPtr ->
  {# call gtk_file_chooser_unselect_uri #} (toFileChooser chooser) strPtr

fileChooserGetURIs :: FileChooserClass chooser => chooser -> IO [String]
fileChooserGetURIs chooser = do
  strList <- {# call gtk_file_chooser_get_uris #} (toFileChooser chooser)
  fromStringGSList strList

fileChooserSetCurrentFolderURI :: FileChooserClass chooser => chooser -> String -> IO Bool
fileChooserSetCurrentFolderURI chooser uri = liftM toBool $
  withCString uri $ \strPtr ->
  {# call gtk_file_chooser_set_current_folder_uri #} (toFileChooser chooser) strPtr

fileChooserGetCurrentFolderURI :: FileChooserClass chooser => chooser -> IO String
fileChooserGetCurrentFolderURI chooser = do
  strPtr <- {# call gtk_file_chooser_get_current_folder_uri #} (toFileChooser chooser)
  readCString strPtr

fileChooserSetPreviewWidget :: (FileChooserClass chooser, WidgetClass widget) =>
                               chooser -> widget -> IO ()
fileChooserSetPreviewWidget chooser widget = 
  {# call gtk_file_chooser_set_preview_widget #} (toFileChooser chooser)
    (toWidget widget)

fileChooserGetPreviewWidget :: FileChooserClass chooser => chooser -> IO (Maybe Widget)
fileChooserGetPreviewWidget chooser = do
  ptr <- {# call gtk_file_chooser_get_preview_widget #} (toFileChooser chooser)
  maybePeek (makeNewObject mkWidget . return) ptr

fileChooserSetPreviewWidgetActive :: FileChooserClass chooser => chooser -> Bool -> IO ()
fileChooserSetPreviewWidgetActive chooser active = 
  {# call gtk_file_chooser_set_preview_widget_active #} (toFileChooser chooser)
    (fromBool active)

fileChooserGetPreviewWidgetActive :: FileChooserClass chooser => chooser -> IO Bool
fileChooserGetPreviewWidgetActive chooser = liftM toBool $
  {# call gtk_file_chooser_get_preview_widget_active #} (toFileChooser chooser)

fileChooserSetUsePreviewLabel :: FileChooserClass chooser => chooser -> Bool -> IO ()
fileChooserSetUsePreviewLabel chooser usePreview = 
  {# call gtk_file_chooser_set_use_preview_label #} (toFileChooser chooser)
    (fromBool usePreview)

fileChooserGetUsePreviewLabel :: FileChooserClass chooser => chooser -> IO Bool
fileChooserGetUsePreviewLabel chooser = liftM toBool $
  {# call gtk_file_chooser_get_use_preview_label #} (toFileChooser chooser)

fileChooserGetPreviewFilename :: FileChooserClass chooser => chooser -> IO (Maybe String)
fileChooserGetPreviewFilename chooser = do
#if defined (WIN32) && GTK_CHECK_VERSION(2,6,0)
  strPtr <- {# call gtk_file_chooser_get_preview_filename_utf8 #}
#else
  strPtr <- {# call gtk_file_chooser_get_preview_filename #}
#endif
    (toFileChooser chooser)
  maybePeek readCString strPtr

fileChooserGetPreviewURI :: FileChooserClass chooser => chooser -> IO (Maybe String)
fileChooserGetPreviewURI chooser = do
  strPtr <- {# call gtk_file_chooser_get_preview_uri #} (toFileChooser chooser)
  maybePeek readCString strPtr

fileChooserSetExtraWidget :: (FileChooserClass chooser, WidgetClass widget) =>
                             chooser -> widget -> IO ()
fileChooserSetExtraWidget chooser widget = 
  {# call gtk_file_chooser_set_extra_widget #} (toFileChooser chooser)
    (toWidget widget)

fileChooserGetExtraWidget :: FileChooserClass chooser => chooser -> IO (Maybe Widget)
fileChooserGetExtraWidget chooser = do
  ptr <- {# call gtk_file_chooser_get_extra_widget #} (toFileChooser chooser)
  maybePeek (makeNewObject mkWidget . return) ptr

fileChooserAddFilter :: FileChooserClass chooser => chooser -> FileFilter -> IO ()
fileChooserAddFilter chooser filter = 
  {# call gtk_file_chooser_add_filter #} (toFileChooser chooser) filter

fileChooserRemoveFilter :: FileChooserClass chooser => chooser -> FileFilter -> IO ()
fileChooserRemoveFilter chooser filter = 
  {# call gtk_file_chooser_remove_filter #} (toFileChooser chooser) filter

fileChooserListFilters :: FileChooserClass chooser => chooser -> IO [FileFilter]
fileChooserListFilters chooser = do
  filterList <- {# call gtk_file_chooser_list_filters #} (toFileChooser chooser)
  filterPtrs <- fromGSList filterList
  mapM (makeNewObject mkFileFilter . return) filterPtrs

fileChooserSetFilter :: FileChooserClass chooser => chooser -> FileFilter -> IO ()
fileChooserSetFilter chooser filter = 
  {# call gtk_file_chooser_set_filter #} (toFileChooser chooser) filter

fileChooserGetFilter :: FileChooserClass chooser => chooser -> IO (Maybe FileFilter)
fileChooserGetFilter chooser = do
  ptr <- {# call gtk_file_chooser_get_filter #} (toFileChooser chooser)
  maybePeek (makeNewObject mkFileFilter . return) ptr

fileChooserAddShortcutFolder :: FileChooserClass chooser => chooser -> String -> IO ()
fileChooserAddShortcutFolder chooser foldername =
  propagateGError $ \gerrorPtr ->
  withCString foldername $ \strPtr -> do
#if defined (WIN32) && GTK_CHECK_VERSION(2,6,0)
  {# call gtk_file_chooser_add_shortcut_folder_utf8 #}
#else
  {# call gtk_file_chooser_add_shortcut_folder #}
#endif
   (toFileChooser chooser) strPtr gerrorPtr
  return ()

fileChooserRemoveShortcutFolder :: FileChooserClass chooser => chooser -> String -> IO ()
fileChooserRemoveShortcutFolder chooser foldername =
  propagateGError $ \gerrorPtr ->
  withCString foldername $ \strPtr -> do
#if defined (WIN32) && GTK_CHECK_VERSION(2,6,0)
  {# call gtk_file_chooser_remove_shortcut_folder_utf8 #}
#else
  {# call gtk_file_chooser_remove_shortcut_folder #}
#endif
    (toFileChooser chooser) strPtr gerrorPtr
  return ()

fileChooserlistShortcutFolders :: FileChooserClass chooser => chooser -> IO [String]
fileChooserlistShortcutFolders chooser = do
#if defined (WIN32) && GTK_CHECK_VERSION(2,6,0)
  strList <- {# call gtk_file_chooser_list_shortcut_folders_utf8 #}
#else
  strList <- {# call gtk_file_chooser_list_shortcut_folders #}
#endif
    (toFileChooser chooser)
  fromStringGSList strList

fileChooserAddShortcutFolderURI :: FileChooserClass chooser => chooser -> String -> IO ()
fileChooserAddShortcutFolderURI chooser folderuri =
  propagateGError $ \gerrorPtr ->
  withCString folderuri $ \strPtr -> do
  {# call gtk_file_chooser_add_shortcut_folder_uri #} (toFileChooser chooser)
    strPtr gerrorPtr
  return ()

fileChooserRemoveShortcutFolderURI :: FileChooserClass chooser => chooser -> String -> IO ()
fileChooserRemoveShortcutFolderURI chooser folderuri =
  propagateGError $ \gerrorPtr ->
  withCString folderuri $ \strPtr -> do
  {# call gtk_file_chooser_remove_shortcut_folder_uri #}
    (toFileChooser chooser) strPtr gerrorPtr
  return ()

fileChooserListShortcutFolderURIs :: FileChooserClass chooser => chooser -> IO [String]
fileChooserListShortcutFolderURIs chooser = do
  strList <- {# call gtk_file_chooser_list_shortcut_folder_uris #}
    (toFileChooser chooser)
  fromStringGSList strList

onCurrentFolderChanged, afterCurrentFolderChanged :: FileChooserClass c => c -> IO () -> IO (ConnectId c)
onCurrentFolderChanged = connect_NONE__NONE "current-folder-changed" False
afterCurrentFolderChanged = connect_NONE__NONE "current-folder-changed" True

onFileActivated, afterFileActivated :: FileChooserClass c => c -> IO () -> IO (ConnectId c)
onFileActivated = connect_NONE__NONE "file-activated" False
afterFileActivated = connect_NONE__NONE "file-activated" True

--onSelectionChanged, afterSelectionChanged :: FileChooserClass c => c -> IO () -> IO (ConnectId c)
--onSelectionChanged = connect_NONE__NONE "selection-changed" False
--afterSelectionChanged = connect_NONE__NONE "selection-changed" True

onUpdatePreview, afterUpdatePreview :: FileChooserClass c => c -> IO () -> IO (ConnectId c)
onUpdatePreview = connect_NONE__NONE "update-preview" False
afterUpdatePreview = connect_NONE__NONE "update-preview" True


------------------------------------------------------
-- Utility functions that really ought to go elsewhere

-- convenience functions for GSlists of strings
fromStringGSList :: GSList -> IO [String]
fromStringGSList strList = do
  strPtrs <- fromGSList strList
  mapM readCString strPtrs

toStringGSList :: [String] -> IO GSList
toStringGSList strs = do
  strPtrs <- mapM newCString strs
  toGSList strPtrs

#endif

