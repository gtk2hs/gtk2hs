-- -*-haskell-*-
--  GIMP Toolkit (GTK) Interface FileChooser
--
--  Author : Duncan Coutts
--
--  Created: 24 April 2004
--
--  Version $Revision: 1.8 $ from $Date: 2005/04/03 14:15:35 $
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
-- File chooser interface used by 'FileChooserWidget' and
-- 'FileChooserDialog'
--
-- * Module available since Gtk+ version 2.4
--
module Graphics.UI.Gtk.Selectors.FileChooser (
-- * Detail
-- 
-- | 'FileChooser' is an interface that can be implemented by file selection
-- widgets. In Gtk+, the main objects that implement this interface are
-- 'FileChooserWidget', 'FileChooserDialog', and 'FileChooserButton'. You do
-- not need to write an object that implements the 'FileChooser' interface
-- unless you are trying to adapt an existing file selector to expose a
-- standard programming interface.

-- ** File Names and Encodings
-- 
-- | When the user is finished selecting files in a 'FileChooser', your
-- program can get the selected names either as filenames or as URIs. For URIs,
-- the normal escaping rules are applied if the URI contains non-ASCII
-- characters. However, filenames are /always/ returned in the character set
-- specified by the G_FILENAME_ENCODING environment variable. Please see the
-- Glib documentation for more details about this variable.

-- ** Adding a Preview Widget
-- 
-- | You can add a custom preview widget to a file chooser and then get
-- notification about when the preview needs to be updated. To install a
-- preview widget, use 'fileChooserSetPreviewWidget'. Then, connect to the
-- updatePreview signal to get notified when you need to update
-- the contents of the preview.
--
-- Your callback should use 'fileChooserGetPreviewFilename' to see what
-- needs previewing. Once you have generated the preview for the corresponding
-- file, you must call 'fileChooserSetPreviewWidgetActive' with a boolean flag
-- that indicates whether your callback could successfully generate a preview.

-- ** Adding Extra Widgets
-- 
-- | You can add extra widgets to a file chooser to provide options that are
-- not present in the default design. For example, you can add a toggle button
-- to give the user the option to open a file in read-only mode. You can use
-- 'fileChooserSetExtraWidget' to insert additional widgets in a file chooser.

-- ** Key Bindings
-- 
-- | Internally, Gtk+ implements a file chooser's graphical user interface
-- with the private GtkFileChooserDefaultClass. This widget has several key
-- bindings and their associated signals. This section describes the available
-- key binding signals.
--
-- * GtkFileChooser key binding example
--
-- The default keys that activate the key-binding signals in
-- GtkFileChooserDefaultClass are as follows:
--
-- [Signal name] Key
--
-- [location-popup] Control-L
--
-- [up-folder] Alt-Up
--
-- [down-folder] Alt-Down
--
-- [home-folder] Alt-Home
--
-- To change these defaults to something else, you could include the
-- following fragment in your .gtkrc-2.0 file:
--
-- > binding "my-own-gtkfilechooser-bindings" {
-- > 	bind "AltShiftl" {
-- > 		"location-popup" ()
-- > 	}
-- > 	bind "AltShiftUp" {
-- > 		"up-folder" ()
-- > 	}
-- > 	bind "AltShiftDown" {
-- > 		"down-folder" ()
-- > 	}
-- > 	bind "AltShiftHome" {
-- > 		"home-folder-folder" ()
-- > 	}
-- > }
-- > 
-- > class "GtkFileChooserDefault" binding "my-own-gtkfilechooser-bindings"
-- > 	
-- 

-- * Class Hierarchy
-- |
-- @
-- |  GInterface
-- |   +----FileChooser
-- @

#if GTK_CHECK_VERSION(2,4,0)
-- * Types
  FileChooser,
  FileChooserClass,
  castToFileChooser,
  FileChooserAction(..),

-- * Methods
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
  fileChooserListShortcutFolders,
  fileChooserAddShortcutFolderURI,
  fileChooserRemoveShortcutFolderURI,
  fileChooserListShortcutFolderURIs,
  fileChooserErrorDomain,

-- * Properties
  fileChooserUsePreviewLabel,
  fileChooserSelectMultiple,
  fileChooserPreviewWidgetActive,
  fileChooserLocalOnly,
  fileChooserAction,

-- * Signals
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

import Monad (liftM, when)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes		(Attr(..))
{#import Graphics.UI.Gtk.Types#}
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
import Graphics.UI.Gtk.Signals
{#import System.Glib.GList#}
import System.Glib.GError		(propagateGError, GErrorDomain, GErrorClass(..))

{# context lib="gtk" prefix="gtk" #}

#if GTK_CHECK_VERSION(2,4,0)

{# enum FileChooserAction {underscoreToCase} #}
{# enum FileChooserError {underscoreToCase} #}

--------------------
-- Methods

fileChooserErrorDomain :: GErrorDomain
fileChooserErrorDomain = unsafePerformIO {#call unsafe file_chooser_error_quark#}

instance GErrorClass FileChooserError where
  gerrorDomain _ = fileChooserErrorDomain

-- | Sets the type of operation that the chooser is performing; the user
-- interface is adapted to suit the selected action. For example, an option to
-- create a new folder might be shown if the action is 'FileChooserActionSave'
-- but not if the action is 'FileChooserActionOpen'.
--
fileChooserSetAction :: FileChooserClass self => self
 -> FileChooserAction -- ^ @action@ - the action that the file selector is
                      -- performing
 -> IO ()
fileChooserSetAction self action =
  {# call gtk_file_chooser_set_action #}
    (toFileChooser self)
    ((fromIntegral . fromEnum) action)

-- | Gets the type of operation that the file chooser is performing; see
-- 'fileChooserSetAction'.
--
fileChooserGetAction :: FileChooserClass self => self -> IO FileChooserAction
fileChooserGetAction self =
  liftM (toEnum . fromIntegral) $
  {# call gtk_file_chooser_get_action #}
    (toFileChooser self)

-- | Sets whether only local files can be selected in the file selector. If
-- @localOnly@ is @True@ (the default), then the selected file are files are
-- guaranteed to be accessible through the operating systems native file file
-- system and therefore the application only needs to worry about the filename
-- functions in 'FileChooser', like 'fileChooserGetFilename', rather than the
-- URI functions like 'fileChooserGetURI',
--
fileChooserSetLocalOnly :: FileChooserClass self => self -> Bool -> IO ()
fileChooserSetLocalOnly self localOnly =
  {# call gtk_file_chooser_set_local_only #}
    (toFileChooser self)
    (fromBool localOnly)

-- | Gets whether only local files can be selected in the file selector. See
-- 'fileChooserSetLocalOnly'
--
fileChooserGetLocalOnly :: FileChooserClass self => self -> IO Bool
fileChooserGetLocalOnly self =
  liftM toBool $
  {# call gtk_file_chooser_get_local_only #}
    (toFileChooser self)

-- | Sets whether multiple files can be selected in the file selector. This is
-- only relevant if the action is set to be 'FileChooserActionOpen' or
-- 'FileChooserActionSave'. It cannot be set with either of the folder actions.
--
fileChooserSetSelectMultiple :: FileChooserClass self => self -> Bool -> IO ()
fileChooserSetSelectMultiple self selectMultiple =
  {# call gtk_file_chooser_set_select_multiple #}
    (toFileChooser self)
    (fromBool selectMultiple)

-- | Gets whether multiple files can be selected in the file selector. See
-- 'fileChooserSetSelectMultiple'.
--
fileChooserGetSelectMultiple :: FileChooserClass self => self -> IO Bool
fileChooserGetSelectMultiple self =
  liftM toBool $
  {# call gtk_file_chooser_get_select_multiple #}
    (toFileChooser self)

-- | Sets the current name in the file selector, as if entered by the user.
-- Note that the name passed in here is a Unicode string rather than a filename.
-- This function is meant for such uses as a suggested name in a \"Save As...\"
-- dialog.
--
-- If you want to preselect a particular existing file, you should use
-- 'fileChooserSetFilename' instead.
--
fileChooserSetCurrentName :: FileChooserClass self => self
 -> FilePath -- ^ @name@ - the filename to use, as a Unicode string
 -> IO ()
fileChooserSetCurrentName self name =
  withUTFString name $ \namePtr ->
  {# call gtk_file_chooser_set_current_name #}
    (toFileChooser self)
    namePtr

-- | Gets the filename for the currently selected file in the file selector.
-- If multiple files are selected, one of the filenames will be returned at
-- random.
--
-- If the file chooser is in folder mode, this function returns the selected
-- folder.
--
fileChooserGetFilename :: FileChooserClass self => self
 -> IO (Maybe FilePath) -- ^ returns The currently selected filename, or
                        -- @Nothing@ if no file is selected, or the selected
                        -- file can't be represented with a local filename.
fileChooserGetFilename self =
#if defined (WIN32) && GTK_CHECK_VERSION(2,6,0)
  {# call gtk_file_chooser_get_filename_utf8 #}
#else
  {# call gtk_file_chooser_get_filename #}
#endif
    (toFileChooser self)
  >>= maybePeek readCString

-- | Sets @filename@ as the current filename for the file chooser; If the file
-- name isn't in the current folder of the chooser, then the current folder of
-- the chooser will be changed to the folder containing @filename@. This is
-- equivalent to a sequence of 'fileChooserUnselectAll' followed by
-- 'fileChooserSelectFilename'.
--
-- Note that the file must exist, or nothing will be done except for the
-- directory change. To pre-enter a filename for the user, as in a save-as
-- dialog, use 'fileChooserSetCurrentName'
--
fileChooserSetFilename :: FileChooserClass self => self
 -> FilePath -- ^ @filename@ - the filename to set as current
 -> IO Bool  -- ^ returns @True@ if both the folder could be changed and the
             -- file was selected successfully, @False@ otherwise.
fileChooserSetFilename self filename =
  liftM toBool $
  withCString filename $ \filenamePtr ->
#if defined (WIN32) && GTK_CHECK_VERSION(2,6,0)
  {# call gtk_file_chooser_set_filename_utf8 #}
#else
  {# call gtk_file_chooser_set_filename #}
#endif
    (toFileChooser self)
    filenamePtr

-- | Selects a filename. If the file name isn't in the current folder of
-- the chooser, then the current folder of the chooser will be changed to the
-- folder containing @filename@.
--
fileChooserSelectFilename :: FileChooserClass self => self
 -> FilePath -- ^ @filename@ - the filename to select
 -> IO Bool  -- ^ returns @True@ if both the folder could be changed and the
             -- file was selected successfully, @False@ otherwise.
fileChooserSelectFilename self filename =
  liftM toBool $
  withCString filename $ \filenamePtr ->
#if defined (WIN32) && GTK_CHECK_VERSION(2,6,0)
  {# call gtk_file_chooser_select_filename_utf8 #}
#else
  {# call gtk_file_chooser_select_filename #}
#endif
    (toFileChooser self)
    filenamePtr

-- | Unselects a currently selected filename. If the filename is not in the
-- current directory, does not exist, or is otherwise not currently selected,
-- does nothing.
--
fileChooserUnselectFilename :: FileChooserClass self => self
 -> FilePath -- ^ @filename@ - the filename to unselect
 -> IO ()
fileChooserUnselectFilename self filename =
  withCString filename $ \filenamePtr ->
#if defined (WIN32) && GTK_CHECK_VERSION(2,6,0)
  {# call gtk_file_chooser_unselect_filename_utf8 #}
#else
  {# call gtk_file_chooser_unselect_filename #}
#endif
    (toFileChooser self)
    filenamePtr

-- | Selects all the files in the current folder of a file chooser.
--
fileChooserSelectAll :: FileChooserClass self => self -> IO ()
fileChooserSelectAll self =
  {# call gtk_file_chooser_select_all #}
    (toFileChooser self)

-- | Unselects all the files in the current folder of a file chooser.
--
fileChooserUnselectAll :: FileChooserClass self => self -> IO ()
fileChooserUnselectAll self =
  {# call gtk_file_chooser_unselect_all #}
    (toFileChooser self)

-- | Lists all the selected files and subfolders in the current folder of
-- the chooser. The returned names are full absolute paths. If files in the
-- current folder cannot be represented as local filenames they will be
-- ignored. (See 'fileChooserGetURIs')
--
fileChooserGetFilenames :: FileChooserClass self => self -> IO [FilePath]
fileChooserGetFilenames self =
#if defined (WIN32) && GTK_CHECK_VERSION(2,6,0)
  {# call gtk_file_chooser_get_filenames_utf8 #}
#else
  {# call gtk_file_chooser_get_filenames #}
#endif
    (toFileChooser self)
  >>= fromStringGSList

-- | Sets the current folder for the chooser from a local filename. The user
-- will be shown the full contents of the current folder, plus user interface
-- elements for navigating to other folders.
--
fileChooserSetCurrentFolder :: FileChooserClass self => self
 -> FilePath -- ^ @filename@ - the full path of the new current folder
 -> IO Bool  -- ^ returns @True@ if the folder could be changed successfully,
             -- @False@ otherwise.
fileChooserSetCurrentFolder self filename =
  liftM toBool $
  withCString filename $ \filenamePtr ->
#if defined (WIN32) && GTK_CHECK_VERSION(2,6,0)
  {# call gtk_file_chooser_set_current_folder_utf8 #}
#else
  {# call gtk_file_chooser_set_current_folder #}
#endif
    (toFileChooser self)
    filenamePtr

-- | Gets the current folder of the chooser as a local filename. See
-- 'fileChooserSetCurrentFolder'.
--
fileChooserGetCurrentFolder :: FileChooserClass self => self
 -> IO (Maybe FilePath) -- ^ returns the full path of the current folder, or
                        -- @Nothing@ if the current path cannot be represented
                        -- as a local filename.
fileChooserGetCurrentFolder self =
#if defined (WIN32) && GTK_CHECK_VERSION(2,6,0)
  {# call gtk_file_chooser_get_current_folder_utf8 #}
#else
  {# call gtk_file_chooser_get_current_folder #}
#endif
    (toFileChooser self)
  >>= maybePeek readCString

-- | Gets the URI for the currently selected file in the file selector. If
-- multiple files are selected, one of the filenames will be returned at
-- random.
--
-- If the file chooser is in folder mode, this function returns the selected
-- folder.
--
fileChooserGetURI :: FileChooserClass self => self
 -> IO (Maybe String) -- ^ returns The currently selected URI, or @Nothing@ if
                      -- no file is selected.
fileChooserGetURI self =
  {# call gtk_file_chooser_get_uri #}
    (toFileChooser self)
  >>= maybePeek readCString

-- | Sets the file referred to by @uri@ as the current file for the file
-- chooser; If the file name isn't in the current folder of the chooser, then
-- the current folder of the chooser will be changed to the folder containing
-- @uri@. This is equivalent to a sequence of 'fileChooserUnselectAll' followed
-- by 'fileChooserSelectURI'.
--
-- Note that the file must exist, or nothing will be done except for the
-- directory change. To pre-enter a filename for the user, as in a save-as
-- dialog, use 'fileChooserSetCurrentName'
--
fileChooserSetURI :: FileChooserClass self => self
 -> String  -- ^ @uri@ - the URI to set as current
 -> IO Bool -- ^ returns @True@ if both the folder could be changed and the
            -- URI was selected successfully, @False@ otherwise.
fileChooserSetURI self uri =
  liftM toBool $
  withCString uri $ \uriPtr ->
  {# call gtk_file_chooser_set_uri #}
    (toFileChooser self)
    uriPtr

-- | Selects the file to by @uri@. If the URI doesn't refer to a file in the
-- current folder of the chooser, then the current folder of the chooser will
-- be changed to the folder containing @filename@.
--
fileChooserSelectURI :: FileChooserClass self => self
 -> String  -- ^ @uri@ - the URI to select
 -> IO Bool -- ^ returns @True@ if both the folder could be changed and the
            -- URI was selected successfully, @False@ otherwise.
fileChooserSelectURI self uri =
  liftM toBool $
  withCString uri $ \uriPtr ->
  {# call gtk_file_chooser_select_uri #}
    (toFileChooser self)
    uriPtr

-- | Unselects the file referred to by @uri@. If the file is not in the
-- current directory, does not exist, or is otherwise not currently selected,
-- does nothing.
--
fileChooserUnselectURI :: FileChooserClass self => self
 -> String -- ^ @uri@ - the URI to unselect
 -> IO ()
fileChooserUnselectURI self uri =
  withCString uri $ \uriPtr ->
  {# call gtk_file_chooser_unselect_uri #}
    (toFileChooser self)
    uriPtr

-- | Lists all the selected files and subfolders in the current folder of
-- the chooser. The returned names are full absolute URIs.
--
fileChooserGetURIs :: FileChooserClass self => self -> IO [String]
fileChooserGetURIs self =
  {# call gtk_file_chooser_get_uris #}
    (toFileChooser self)
  >>= fromStringGSList

-- | Sets the current folder for the chooser from an URI. The user will be
-- shown the full contents of the current folder, plus user interface elements
-- for navigating to other folders.
--
fileChooserSetCurrentFolderURI :: FileChooserClass self => self
 -> String  -- ^ @uri@ - the URI for the new current folder
 -> IO Bool -- ^ returns @True@ if the folder could be changed successfully,
            -- @False@ otherwise.
fileChooserSetCurrentFolderURI self uri =
  liftM toBool $
  withCString uri $ \uriPtr ->
  {# call gtk_file_chooser_set_current_folder_uri #}
    (toFileChooser self)
    uriPtr

-- | Gets the current folder of the chooser as an URI. See
-- 'fileChooserSetCurrentFolderURI'.
--
fileChooserGetCurrentFolderURI :: FileChooserClass self => self
 -> IO String -- ^ returns the URI for the current folder.
fileChooserGetCurrentFolderURI self =
  {# call gtk_file_chooser_get_current_folder_uri #}
    (toFileChooser self)
  >>= readCString

-- | Sets an application-supplied widget to use to display a custom preview of
-- the currently selected file. To implement a preview, after setting the
-- preview widget, you connect to the UpdatePreview signal, and call
-- 'fileChooserGetPreviewFilename' or 'fileChooserGetPreviewURI' on each
-- change. If you can display a preview of the new file, update your widget and
-- set the preview active using 'fileChooserSetPreviewWidgetActive'. Otherwise,
-- set the preview inactive.
--
-- When there is no application-supplied preview widget, or the
-- application-supplied preview widget is not active, the file chooser may
-- display an internally generated preview of the current file or it may
-- display no preview at all.
--
fileChooserSetPreviewWidget :: (FileChooserClass self, WidgetClass previewWidget) => self
 -> previewWidget -- ^ @previewWidget@ - widget for displaying preview.
 -> IO ()
fileChooserSetPreviewWidget self previewWidget =
  {# call gtk_file_chooser_set_preview_widget #}
    (toFileChooser self)
    (toWidget previewWidget)

-- | Gets the current preview widget; see 'fileChooserSetPreviewWidget'.
--
fileChooserGetPreviewWidget :: FileChooserClass self => self
 -> IO (Maybe Widget) -- ^ returns the current preview widget, or @Nothing@
fileChooserGetPreviewWidget self =
  maybeNull (makeNewObject mkWidget) $
  {# call gtk_file_chooser_get_preview_widget #}
    (toFileChooser self)

-- | Sets whether the preview widget set by 'fileChooserSetPreviewWidget'
-- should be shown for the current filename. When @active@ is set to false, the
-- file chooser may display an internally generated preview of the current file
-- or it may display no preview at all. See 'fileChooserSetPreviewWidget' for
-- more details.
--
fileChooserSetPreviewWidgetActive :: FileChooserClass self => self
 -> Bool  -- ^ @active@ - whether to display the user-specified preview widget
 -> IO ()
fileChooserSetPreviewWidgetActive self active =
  {# call gtk_file_chooser_set_preview_widget_active #}
    (toFileChooser self)
    (fromBool active)

-- | Gets whether the preview widget set by 'fileChooserSetPreviewWidget'
-- should be shown for the current filename. See
-- 'fileChooserSetPreviewWidgetActive'.
--
fileChooserGetPreviewWidgetActive :: FileChooserClass self => self
 -> IO Bool -- ^ returns @True@ if the preview widget is active for the
            -- current filename.
fileChooserGetPreviewWidgetActive self =
  liftM toBool $
  {# call gtk_file_chooser_get_preview_widget_active #}
    (toFileChooser self)

-- | Sets whether the file chooser should display a stock label with the name
-- of the file that is being previewed; the default is @True@. Applications
-- that want to draw the whole preview area themselves should set this to
-- @False@ and display the name themselves in their preview widget.
--
-- See also: 'fileChooserSetPreviewWidget'
--
fileChooserSetUsePreviewLabel :: FileChooserClass self => self
 -> Bool  -- ^ @useLabel@ - whether to display a stock label with the name of
          -- the previewed file
 -> IO ()
fileChooserSetUsePreviewLabel self useLabel =
  {# call gtk_file_chooser_set_use_preview_label #}
    (toFileChooser self)
    (fromBool useLabel)

-- | Gets whether a stock label should be drawn with the name of the previewed
-- file. See 'fileChooserSetUsePreviewLabel'.
--
fileChooserGetUsePreviewLabel :: FileChooserClass self => self
 -> IO Bool -- ^ returns @True@ if the file chooser is set to display a label
            -- with the name of the previewed file, @False@ otherwise.
fileChooserGetUsePreviewLabel self =
  liftM toBool $
  {# call gtk_file_chooser_get_use_preview_label #}
    (toFileChooser self)

-- | Gets the filename that should be previewed in a custom preview widget.
-- See 'fileChooserSetPreviewWidget'.
--
fileChooserGetPreviewFilename :: FileChooserClass self => self
 -> IO (Maybe FilePath) -- ^ returns the filename to preview, or @Nothing@ if
                        -- no file is selected, or if the selected file cannot
                        -- be represented as a local filename.
fileChooserGetPreviewFilename self =
#if defined (WIN32) && GTK_CHECK_VERSION(2,6,0)
  {# call gtk_file_chooser_get_preview_filename_utf8 #}
#else
  {# call gtk_file_chooser_get_preview_filename #}
#endif
    (toFileChooser self)
  >>= maybePeek readCString

-- | Gets the URI that should be previewed in a custom preview widget. See
-- 'fileChooserSetPreviewWidget'.
--
fileChooserGetPreviewURI :: FileChooserClass self => self
 -> IO (Maybe String) -- ^ returns the URI for the file to preview, or
                      -- @Nothing@ if no file is selected.
fileChooserGetPreviewURI self =
  {# call gtk_file_chooser_get_preview_uri #}
    (toFileChooser self)
  >>= maybePeek readCString

-- | Sets an application-supplied widget to provide extra options to the user.
--
fileChooserSetExtraWidget :: (FileChooserClass self, WidgetClass extraWidget) => self
 -> extraWidget -- ^ @extraWidget@ - widget for extra options
 -> IO ()
fileChooserSetExtraWidget self extraWidget =
  {# call gtk_file_chooser_set_extra_widget #}
    (toFileChooser self)
    (toWidget extraWidget)

-- | Gets the current preview widget; see 'fileChooserSetExtraWidget'.
--
fileChooserGetExtraWidget :: FileChooserClass self => self
 -> IO (Maybe Widget) -- ^ returns the current extra widget, or @Nothing@
fileChooserGetExtraWidget self =
  maybeNull (makeNewObject mkWidget) $
  {# call gtk_file_chooser_get_extra_widget #}
    (toFileChooser self)

-- | Adds the filter to the list of filters that the user can select between.
-- When a filter is selected, only files that are passed by that filter are
-- displayed.
--
fileChooserAddFilter :: FileChooserClass self => self -> FileFilter -> IO ()
fileChooserAddFilter self filter =
  {# call gtk_file_chooser_add_filter #}
    (toFileChooser self)
    filter

-- | Removes the filter from the list of filters that the user can select
-- between.
--
fileChooserRemoveFilter :: FileChooserClass self => self -> FileFilter -> IO ()
fileChooserRemoveFilter self filter =
  {# call gtk_file_chooser_remove_filter #}
    (toFileChooser self)
    filter

-- | Lists the current set of user-selectable filters; see
-- 'fileChooserAddFilter', 'fileChooserRemoveFilter'.
--
fileChooserListFilters :: FileChooserClass self => self -> IO [FileFilter]
fileChooserListFilters self = do
  filterList <- {# call gtk_file_chooser_list_filters #}
    (toFileChooser self)
  filterPtrs <- fromGSList filterList
  mapM (makeNewObject mkFileFilter . return) filterPtrs

-- | Sets the current filter; only the files that pass the filter will be
-- displayed. If the user-selectable list of filters is non-empty, then the
-- filter should be one of the filters in that list. Setting the current filter
-- when the list of filters is empty is useful if you want to restrict the
-- displayed set of files without letting the user change it.
--
fileChooserSetFilter :: FileChooserClass self => self -> FileFilter -> IO ()
fileChooserSetFilter self filter =
  {# call gtk_file_chooser_set_filter #}
    (toFileChooser self)
    filter

-- | Gets the current filter; see 'fileChooserSetFilter'.
--
fileChooserGetFilter :: FileChooserClass self => self
 -> IO (Maybe FileFilter) -- ^ returns the current filter, or @Nothing@
fileChooserGetFilter self =
  maybeNull (makeNewObject mkFileFilter) $
  {# call gtk_file_chooser_get_filter #}
    (toFileChooser self)

-- | Adds a folder to be displayed with the shortcut folders in a file
-- chooser. Note that shortcut folders do not get saved, as they are provided
-- by the application. For example, you can use this to add a
-- \"\/usr\/share\/mydrawprogram\/Clipart\" folder to the volume list.
--
-- If the folder can not be added successfully an exception will be thrown.
--
fileChooserAddShortcutFolder :: FileChooserClass self => self
 -> FilePath -- ^ @folder@ - filename of the folder to add
 -> IO ()
fileChooserAddShortcutFolder self folder =
  propagateGError $ \errorPtr ->
  withCString folder $ \folderPtr -> do
#if defined (WIN32) && GTK_CHECK_VERSION(2,6,0)
  {# call gtk_file_chooser_add_shortcut_folder_utf8 #}
#else
  {# call gtk_file_chooser_add_shortcut_folder #}
#endif
    (toFileChooser self)
    folderPtr
    errorPtr
  return ()

-- | Removes a folder from a file chooser's list of shortcut folders.
--
-- If the folder can not be removed successfully an exception will be thrown.
--
fileChooserRemoveShortcutFolder :: FileChooserClass self => self
 -> FilePath -- ^ @folder@ - filename of the folder to remove
 -> IO ()
fileChooserRemoveShortcutFolder self folder =
  propagateGError $ \errorPtr ->
  withCString folder $ \folderPtr -> do
#if defined (WIN32) && GTK_CHECK_VERSION(2,6,0)
  {# call gtk_file_chooser_remove_shortcut_folder_utf8 #}
#else
  {# call gtk_file_chooser_remove_shortcut_folder #}
#endif
    (toFileChooser self)
    folderPtr
    errorPtr
  return ()

-- | Queries the list of shortcut folders in the file chooser, as set by
-- 'fileChooserAddShortcutFolder'.
--
fileChooserListShortcutFolders :: FileChooserClass self => self -> IO [String]
fileChooserListShortcutFolders self =
#if defined (WIN32) && GTK_CHECK_VERSION(2,6,0)
  {# call gtk_file_chooser_list_shortcut_folders_utf8 #}
#else
  {# call gtk_file_chooser_list_shortcut_folders #}
#endif
    (toFileChooser self)
  >>= fromStringGSList

-- | Adds a folder URI to be displayed with the shortcut folders in a file
-- chooser. Note that shortcut folders do not get saved, as they are provided
-- by the application. For example, you can use this to add a
-- \"file:\/\/\/usr\/share\/mydrawprogram\/Clipart\" folder to the volume list.
--
-- If the folder can not be added successfully an exception will be thrown.
--
fileChooserAddShortcutFolderURI :: FileChooserClass self => self
 -> String -- ^ @uri@ - URI of the folder to add
 -> IO ()
fileChooserAddShortcutFolderURI self uri =
  propagateGError $ \errorPtr ->
  withCString uri $ \uriPtr -> do
  {# call gtk_file_chooser_add_shortcut_folder_uri #}
    (toFileChooser self)
    uriPtr
    errorPtr
  return ()

-- | Removes a folder URI from a file chooser's list of shortcut folders.
--
fileChooserRemoveShortcutFolderURI :: FileChooserClass self => self
 -> String  -- ^ @uri@ - URI of the folder to remove
 -> IO ()
fileChooserRemoveShortcutFolderURI self uri =
  propagateGError $ \errorPtr ->
  withCString uri $ \uriPtr -> do
  {# call gtk_file_chooser_remove_shortcut_folder_uri #}
    (toFileChooser self)
    uriPtr
    errorPtr
  return ()

-- | Queries the list of shortcut folders in the file chooser, as set by
-- 'fileChooserAddShortcutFolderURI'.
--
fileChooserListShortcutFolderURIs :: FileChooserClass self => self -> IO [String]
fileChooserListShortcutFolderURIs self =
  {# call gtk_file_chooser_list_shortcut_folder_uris #}
    (toFileChooser self)
  >>= fromStringGSList

--------------------
-- Properties

-- | \'usePreviewLabel\' property. See 'fileChooserGetUsePreviewLabel' and
-- 'fileChooserSetUsePreviewLabel'
--
fileChooserUsePreviewLabel :: FileChooserClass self => Attr self Bool
fileChooserUsePreviewLabel = Attr 
  fileChooserGetUsePreviewLabel
  fileChooserSetUsePreviewLabel

-- | \'selectMultiple\' property. See 'fileChooserGetSelectMultiple' and
-- 'fileChooserSetSelectMultiple'
--
fileChooserSelectMultiple :: FileChooserClass self => Attr self Bool
fileChooserSelectMultiple = Attr 
  fileChooserGetSelectMultiple
  fileChooserSetSelectMultiple

-- | \'previewWidgetActive\' property. See 'fileChooserGetPreviewWidgetActive'
-- and 'fileChooserSetPreviewWidgetActive'
--
fileChooserPreviewWidgetActive :: FileChooserClass self => Attr self Bool
fileChooserPreviewWidgetActive = Attr 
  fileChooserGetPreviewWidgetActive
  fileChooserSetPreviewWidgetActive

-- | \'localOnly\' property. See 'fileChooserGetLocalOnly' and
-- 'fileChooserSetLocalOnly'
--
fileChooserLocalOnly :: FileChooserClass self => Attr self Bool
fileChooserLocalOnly = Attr 
  fileChooserGetLocalOnly
  fileChooserSetLocalOnly

-- | \'action\' property. See 'fileChooserGetAction' and
-- 'fileChooserSetAction'
--
fileChooserAction :: FileChooserClass self => Attr self FileChooserAction
fileChooserAction = Attr 
  fileChooserGetAction
  fileChooserSetAction

--------------------
-- Signals

-- | This signal is emitted when the current folder in a 'FileChooser'
-- changes. This can happen due to the user performing some action that changes
-- folders, such as selecting a bookmark or visiting a folder on the file list.
-- It can also happen as a result of calling a function to explicitly change
-- the current folder in a file chooser.
--
-- Normally you do not need to connect to this signal, unless you need to
-- keep track of which folder a file chooser is showing.
--
-- See also: 'fileChooserSetCurrentFolder', 'fileChooserGetCurrentFolder',
-- 'fileChooserSetCurrentFolderURI', 'fileChooserGetCurrentFolderURI'.
--
onCurrentFolderChanged, afterCurrentFolderChanged :: FileChooserClass self => self
 -> IO ()
 -> IO (ConnectId self)
onCurrentFolderChanged = connect_NONE__NONE "current-folder-changed" False
afterCurrentFolderChanged = connect_NONE__NONE "current-folder-changed" True

-- | This signal is emitted when there is a change in the set of selected
-- files in a 'FileChooser'. This can happen when the user modifies the
-- selection with the mouse or the keyboard, or when explicitly calling
-- functions to change the selection.
--
-- Normally you do not need to connect to this signal, as it is easier to
-- wait for the file chooser to finish running, and then to get the list of
-- selected files using the functions mentioned below.
--
-- See also: 'fileChooserSelectFilename', 'fileChooserUnselectFilename',
-- 'fileChooserGetFilename', 'fileChooserGetFilenames', 'fileChooserSelectURI',
-- 'fileChooserUnselectURI', 'fileChooserGetURI', 'fileChooserGetURIs'.
--
--onSelectionChanged, afterSelectionChanged :: FileChooserClass self => self
-- -> IO ()
-- -> IO (ConnectId self)
--onSelectionChanged = connect_NONE__NONE "selection-changed" False
--afterSelectionChanged = connect_NONE__NONE "selection-changed" True

-- | This signal is emitted when the preview in a file chooser should be
-- regenerated. For example, this can happen when the currently selected file
-- changes. You should use this signal if you want your file chooser to have a
-- preview widget.
--
-- Once you have installed a preview widget with
-- 'fileChooserSetPreviewWidget', you should update it when this signal is
-- emitted. You can use the functions 'fileChooserGetPreviewFilename' or
-- 'fileChooserGetPreviewURI' to get the name of the file to preview. Your
-- widget may not be able to preview all kinds of files; your callback must
-- call 'fileChooserSetPreviewWigetActive' to inform the file chooser about
-- whether the preview was generated successfully or not.
--
-- See also: 'fileChooserSetPreviewWidget',
-- 'fileChooserSetPreviewWidgetActive', 'fileChooserSetUsePreviewLabel',
-- 'fileChooserGetPreviewFilename', 'fileChooserGetPreviewURI'.
--
onUpdatePreview, afterUpdatePreview :: FileChooserClass self => self
 -> IO ()
 -> IO (ConnectId self)
onUpdatePreview = connect_NONE__NONE "update-preview" False
afterUpdatePreview = connect_NONE__NONE "update-preview" True

-- | This signal is emitted when the user \"activates\" a file in the file
-- chooser. This can happen by double-clicking on a file in the file list, or
-- by pressing Enter.
--
-- Normally you do not need to connect to this signal. It is used internally
-- by 'FileChooserDialog' to know when to activate the default button in the
-- dialog.
--
-- See also: 'fileChooserGetFilename', 'fileChooserGetFilenames',
-- 'fileChooserGetURI', 'fileChooserGetURIs'.
--
onFileActivated, afterFileActivated :: FileChooserClass self => self
 -> IO ()
 -> IO (ConnectId self)
onFileActivated = connect_NONE__NONE "file-activated" False
afterFileActivated = connect_NONE__NONE "file-activated" True
#endif

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
