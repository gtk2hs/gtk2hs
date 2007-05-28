-- -*-haskell-*-
--  GIMP Toolkit (GTK) StockItems
--
--  Author : Axel Simon
--
--  Created: 24 May 2001
--
--  Version $Revision: 1.8 $ from $Date: 2005/12/08 17:30:55 $
--
--  Copyright (C) 1999-2005 Axel Simon
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
-- A StockItem is a resource that is know throughout Gtk.
--
-- * Defining you own
--   'IconSet's as 'StockItem's will make it possible for 
--   Gtk to choose
--   the most appropriate sizes and enables themes to override your built in
--   icons. A couple of constants are defined here as well. They are useful 
--   in accessing Gtk's predefined items.
--

module Graphics.UI.Gtk.General.StockItems (
  StockItem(StockItem),
  StockId,
  siStockId,
  siLabel,
  siModifier,
  siKeyval,
  siTransDom,
  stockAddItem,
  stockLookupItem,
  stockListIds,
  stockAbout,
  stockAdd,
  stockApply,
  stockBold,
  stockCancel,
  stockCDROM,
  stockClear,
  stockClose,
  stockColorPicker,
  stockConvert,
  stockConnect,
  stockCopy,
  stockCut,
  stockDelete,
  stockDialogAuthentication,
  stockDialogError,
  stockDialogInfo,
  stockDialogQuestion,
  stockDialogWarning,
  stockDirectory,
  stockDisconnect,
  stockDnd,
  stockDndMultiple,
  stockEdit,
  stockExecute,
  stockFile,
  stockFind,
  stockFindAndRelpace,
  stockFloppy,
  stockFullscreen,
  stockGotoBottom,
  stockGotoFirst,
  stockGotoLast,
  stockGotoTop,
  stockGoBack,
  stockGoDown,
  stockGoForward,
  stockGoUp,
  stockHarddisk,
  stockHelp,
  stockHome,
  stockIndent,
  stockIndex,
  stockInfo,
  stockItalic,
  stockJumpTo,
  stockJustifyCenter,
  stockJustifyFill,
  stockJustifyLeft,
  stockJustifyRight,
  stockMediaForward,
  stockMediaNext,
  stockMediaPause,
  stockMediaPlay,
  stockMediaPrevious,
  stockMediaRecord,
  stockMediaRewind,
  stockMediaStop,
  stockMissingImage,
  stockNetwork,
  stockNew,
  stockNo,
  stockOk,
  stockOpen,
  stockOrientationLandscape,
  stockOrientationReverseLandscape,
  stockOrientationPortrait,
  stockOrientationReversePortrait,
  stockPaste,
  stockPreferences,
  stockPrint,
  stockPrintPreview,
  stockProperties,
  stockQuit,
  stockRedo,
  stockRefresh,
  stockRemove,
  stockRevertToSaved,
  stockSave,
  stockSaveAs,
  stockSelectAll,
  stockSelectColor,
  stockSelectFont,
  stockSortAscending,
  stockSortDescending,
  stockSpellCheck,
  stockStop,
  stockStrikethrough,
  stockUndelete,
  stockUnderline,
  stockUndo,
  stockUnindent,
  stockYes,
  stockZoom100,
  stockZoomFit,
  stockZoomIn,
  stockZoomOut
  ) where

-- The StockItem structure is completely marshaled to Haskell. It is 
-- possible to marshal all strings lazily because the string pointers are
-- valid throughout the lifetime of the application. The only drawback it
-- that a stock item that is replaced by the another item with the same
-- name will never be freed. This deficiency is built into Gtk however.
--

import Control.Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Flags
import System.Glib.GList	(GSList, fromGSListRev)
import Graphics.UI.Gtk.Gdk.Events	(Modifier)
import Graphics.UI.Gtk.Gdk.Keys		(KeyVal)

#include <gtk/gtk.h>

-- |  A synonym for a standard button or icon.
--
type StockId = String


-- Although the structure itself is allocated dynamically, its contents
-- are not. All string pointers are constant throughout the lifetime of
-- the application. We do not need to marshal these Strings to Haskell if
-- they are not needed.
--

-- | The description of a stock item.
--
data StockItem = StockItem {
  siStockId :: StockId,
  siLabel   :: String,
  siModifier:: [Modifier],
  siKeyval  :: KeyVal,
  siTransDom:: String }

instance Storable StockItem where
  sizeOf _	= #const sizeof(GtkStockItem)
  alignment _	= alignment (undefined::CString)
  peek siPtr	= do
    (stockId    :: CString) <- #{peek GtkStockItem, stock_id} siPtr
    (label	:: CString) <- #{peek GtkStockItem, label} siPtr
    (modifier	:: #type GdkModifierType)
		            <- #{peek GtkStockItem, modifier} siPtr
    (keyval	:: #type guint)
			    <- #{peek GtkStockItem, keyval} siPtr
    (transDom	:: CString) <- #{peek GtkStockItem, translation_domain} siPtr
    return $ StockItem {
      siStockId  = unsafePerformIO $ peekUTFString' stockId,
      siLabel	 = unsafePerformIO $ peekUTFString' label,
      -- &%!?$ c2hs and hsc should agree on types
      siModifier = toFlags (fromIntegral modifier), 
      siKeyval	 = keyval,
      siTransDom = unsafePerformIO $ peekUTFString' transDom }
    where
      peekUTFString' :: CString -> IO String
      peekUTFString' strPtr | strPtr==nullPtr = return ""
			  | otherwise	    = peekUTFString strPtr

  poke siPtr (StockItem {
    siStockId = stockId,
    siLabel   = label,
    siModifier= modifier,
    siKeyval  = keyval,
    siTransDom= transDom }) = do
    stockIdPtr <- newUTFString stockId
    #{poke GtkStockItem, stock_id} siPtr stockIdPtr
    labelPtr   <- newUTFString label
    #{poke GtkStockItem, label}	   siPtr labelPtr
    #{poke GtkStockItem, modifier} siPtr 
      ((fromIntegral (fromFlags modifier))::#{type GdkModifierType})
    #{poke GtkStockItem, keyval}   siPtr ((fromIntegral keyval)::#{type guint})
    transDomPtr<- newUTFString transDom
    #{poke GtkStockItem, translation_domain} siPtr transDomPtr


-- | Add new stock items to Gtk.
--

-- Using stock_add_static would be possible if we used g_malloc to reserve
-- space since the allocated space might actually be freed when another
-- stock item with the same name is added.
stockAddItem :: [StockItem] -> IO ()
stockAddItem [] = return ()
stockAddItem sis = let items = length sis in do
  allocaArray items $ \aPtr -> do
  pokeArray aPtr sis
  stock_add aPtr (fromIntegral items)

-- | Lookup an item in stock.
--
stockLookupItem :: StockId -> IO (Maybe StockItem)
stockLookupItem stockId = 
  alloca $ \siPtr ->
  withUTFString stockId $ \strPtr -> do
  res <- stock_lookup strPtr siPtr
  if (toBool res) then liftM Just $ peek siPtr else return Nothing

-- | Produce a list of all known stock identifiers.
--
-- * Retrieve a list of all known stock identifiers. These can either be
--   added by 'stockAddItem' or by adding items to a
--   'Graphics.UI.Gtk.General.IconFactory.IconFactory'.
--
-- * The list is sorted alphabetically (sorting is not Unicode aware).
--
stockListIds :: IO [StockId]
stockListIds = do
  lPtr <- stock_list_ids
  sPtrs <- fromGSListRev lPtr
  res <- mapM readUTFString sPtrs
  return res

foreign import ccall unsafe "gtk_stock_add"
  stock_add :: Ptr StockItem -> #{type guint} -> IO ()

foreign import ccall unsafe "gtk_stock_lookup"
  stock_lookup :: CString -> Ptr StockItem -> IO #type gboolean

foreign import ccall unsafe "gtk_stock_list_ids"
  stock_list_ids :: IO GSList

#if GTK_CHECK_VERSION(2,6,0)

-- | <<images/stock-icons/stock_about_24.png>>
stockAbout		:: StockId
stockAbout		= #{const_str GTK_STOCK_ABOUT}
#else
stockAbout		= stockMissingImage
#endif

-- | <<images/stock-icons/stock_add_24.png>>
stockAdd		:: StockId
stockAdd		= #{const_str GTK_STOCK_ADD}

-- | <<images/stock-icons/stock_apply_20.png>>
stockApply		:: StockId
stockApply		= #{const_str GTK_STOCK_APPLY}

-- | <<images/stock-icons/stock_text_bold_24.png>>
stockBold		:: StockId
stockBold		= #{const_str GTK_STOCK_BOLD}

-- | <<images/stock-icons/stock_cancel_20.png>>
stockCancel		:: StockId
stockCancel		= #{const_str GTK_STOCK_CANCEL}

-- | <<images/stock-icons/stock_cdrom_24.png>>
stockCDROM		:: StockId
stockCDROM		= #{const_str GTK_STOCK_CDROM}

-- | <<images/stock-icons/stock_clear_24.png>>
stockClear		:: StockId
stockClear		= #{const_str GTK_STOCK_CLEAR}

-- | <<images/stock-icons/stock_close_24.png>>
stockClose		:: StockId
stockClose		= #{const_str GTK_STOCK_CLOSE}
#if GTK_CHECK_VERSION(2,2,0)

-- | <<images/stock-icons/stock_color_picker_25.png>>
stockColorPicker	:: StockId
stockColorPicker	= #{const_str GTK_STOCK_COLOR_PICKER}
#else
stockColorPicker        = stockMissingImage
#endif

-- | <<images/stock-icons/stock_convert_24.png>>
stockConvert		:: StockId
stockConvert		= #{const_str GTK_STOCK_CONVERT}
#if GTK_CHECK_VERSION(2,6,0)

-- | <<images/stock-icons/stock_connect_24.png>>
stockConnect		:: StockId
stockConnect		= #{const_str GTK_STOCK_CONNECT}
#else
stockConnect		= stockMissingImage
#endif

-- | <<images/stock-icons/stock_copy_24.png>>
stockCopy		:: StockId
stockCopy		= #{const_str GTK_STOCK_COPY}

-- | <<images/stock-icons/stock_cut_24.png>>
stockCut		:: StockId
stockCut		= #{const_str GTK_STOCK_CUT}

-- | <<images/stock-icons/stock_trash_24.png>>
stockDelete		:: StockId
stockDelete		= #{const_str GTK_STOCK_DELETE}

#if GTK_CHECK_VERSION(2,6,0)
-- | <<images/stock-icons/stock_dialog_authentication_48.png>>
stockDialogAuthentication :: StockId
stockDialogAuthentication = #{const_str GTK_STOCK_DIALOG_AUTHENTICATION}
#else
stockDialogAuthentication = stockDialogQuestion
#endif

-- | <<images/stock-icons/stock_dialog_error_48.png>>
stockDialogError	:: StockId
stockDialogError	= #{const_str GTK_STOCK_DIALOG_ERROR}

-- | <<images/stock-icons/stock_dialog_info_48.png>>
stockDialogInfo		:: StockId
stockDialogInfo		= #{const_str GTK_STOCK_DIALOG_INFO}

-- | <<images/stock-icons/stock_dialog_question_48.png>>
stockDialogQuestion	:: StockId
stockDialogQuestion	= #{const_str GTK_STOCK_DIALOG_QUESTION}

-- | <<images/stock-icons/stock_dialog_warning_48.png>>
stockDialogWarning	:: StockId
stockDialogWarning	= #{const_str GTK_STOCK_DIALOG_WARNING}
#if GTK_CHECK_VERSION(2,6,0)

-- | <<images/stock-icons/stock_directory_24.png>>
stockDirectory		:: StockId
stockDirectory		= #{const_str GTK_STOCK_DIRECTORY}
#else
stockDirectory		= stockMissingImage
#endif
#if GTK_CHECK_VERSION(2,6,0)

-- | <<images/stock-icons/stock_disconnect_24.png>>
stockDisconnect		:: StockId
stockDisconnect		= #{const_str GTK_STOCK_DISCONNECT}
#else
stockDisconnect		= stockMissingImage
#endif

-- | <<images/stock-icons/stock_dnd_32.png>>
stockDnd		:: StockId
stockDnd		= #{const_str GTK_STOCK_DND}

-- | <<images/stock-icons/stock_dnd_multiple_32.png>>
stockDndMultiple	:: StockId
stockDndMultiple	= #{const_str GTK_STOCK_DND_MULTIPLE}
#if GTK_CHECK_VERSION(2,6,0)

-- | <<images/stock-icons/stock_edit_24.png>>
stockEdit		:: StockId
stockEdit		= #{const_str GTK_STOCK_EDIT}
#else
stockEdit		= stockMissingImage
#endif

-- | <<images/stock-icons/stock_exec_24.png>>
stockExecute		:: StockId
stockExecute		= #{const_str GTK_STOCK_EXECUTE}
#if GTK_CHECK_VERSION(2,6,0)

-- | <<images/stock-icons/stock_file_24.png>>
stockFile		:: StockId
stockFile		= #{const_str GTK_STOCK_FILE}
#else
stockFile		= stockMissingImage
#endif

-- | <<images/stock-icons/stock_search_24.png>>
stockFind		:: StockId
stockFind		= #{const_str GTK_STOCK_FIND}

-- | <<images/stock-icons/stock_search_replace_24.png>>
stockFindAndRelpace	:: StockId
stockFindAndRelpace	= #{const_str GTK_STOCK_FIND_AND_REPLACE}

-- | <<images/stock-icons/stock_save_24.png>>
stockFloppy		:: StockId
stockFloppy		= #{const_str GTK_STOCK_FLOPPY}
#if GTK_CHECK_VERSION(2,8,0)

-- | <<images/stock-icons/stock_fullscreen_24.png>>
stockFullscreen		:: StockId
stockFullscreen		= #{const_str GTK_STOCK_FULLSCREEN}
#else
stockFullscreen		= stockMissingImage
#endif

-- | <<images/stock-icons/stock_bottom_24.png>>
stockGotoBottom		:: StockId
stockGotoBottom		= #{const_str GTK_STOCK_GOTO_BOTTOM}

-- | <<images/stock-icons/stock_first_24.png>>
stockGotoFirst		:: StockId
stockGotoFirst		= #{const_str GTK_STOCK_GOTO_FIRST}

-- | <<images/stock-icons/stock_last_24.png>>
stockGotoLast		:: StockId
stockGotoLast		= #{const_str GTK_STOCK_GOTO_LAST}

-- | <<images/stock-icons/stock_top_24.png>>
stockGotoTop		:: StockId
stockGotoTop		= #{const_str GTK_STOCK_GOTO_TOP}

-- | <<images/stock-icons/stock_left_arrow_24.png>>
stockGoBack		:: StockId
stockGoBack		= #{const_str GTK_STOCK_GO_BACK}

-- | <<images/stock-icons/stock_down_arrow_24.png>>
stockGoDown		:: StockId
stockGoDown		= #{const_str GTK_STOCK_GO_DOWN}

-- | <<images/stock-icons/stock_right_arrow_24.png>>
stockGoForward		:: StockId
stockGoForward		= #{const_str GTK_STOCK_GO_FORWARD}

-- | <<images/stock-icons/stock_up_arrow_24.png>>
stockGoUp		:: StockId
stockGoUp		= #{const_str GTK_STOCK_GO_UP}
#if GTK_CHECK_VERSION(2,4,0)

-- | <<images/stock-icons/stock_harddisk_24.png>>
stockHarddisk		:: StockId
stockHarddisk		= #{const_str GTK_STOCK_HARDDISK}
#else
stockHarddisk		= stockMissingImage
#endif

-- | <<images/stock-icons/stock_help_24.png>>
stockHelp		:: StockId
stockHelp		= #{const_str GTK_STOCK_HELP}

-- | <<images/stock-icons/stock_home_24.png>>
stockHome		:: StockId
stockHome		= #{const_str GTK_STOCK_HOME}
#if GTK_CHECK_VERSION(2,4,0)

-- | <<images/stock-icons/stock_text_indent_24.png>>
stockIndent		:: StockId
stockIndent		= #{const_str GTK_STOCK_INDENT}
#else
stockIndent		= stockMissingImage
#endif

-- | <<images/stock-icons/stock_index_24.png>>
stockIndex		:: StockId
stockIndex		= #{const_str GTK_STOCK_INDEX}
#if GTK_CHECK_VERSION(2,8,0)

-- | <<images/stock-icons/stock_info_24.png>>
stockInfo		:: StockId
stockInfo		= #{const_str GTK_STOCK_INFO}
#else
stockInfo		= stockMissingImage
#endif

-- | <<images/stock-icons/stock_text_italic_24.png>>
stockItalic		:: StockId
stockItalic		= #{const_str GTK_STOCK_ITALIC}

-- | <<images/stock-icons/stock_jump_to_24.png>>
stockJumpTo		:: StockId
stockJumpTo		= #{const_str GTK_STOCK_JUMP_TO}

-- | <<images/stock-icons/stock_align_center_24.png>>
stockJustifyCenter	:: StockId
stockJustifyCenter	= #{const_str GTK_STOCK_JUSTIFY_CENTER}

-- | <<images/stock-icons/stock_align_justify_24.png>>
stockJustifyFill	:: StockId
stockJustifyFill	= #{const_str GTK_STOCK_JUSTIFY_FILL}

-- | <<images/stock-icons/stock_align_left_24.png>>
stockJustifyLeft	:: StockId
stockJustifyLeft	= #{const_str GTK_STOCK_JUSTIFY_LEFT}

-- | <<images/stock-icons/stock_align_right_24.png>>
stockJustifyRight	:: StockId
stockJustifyRight	= #{const_str GTK_STOCK_JUSTIFY_RIGHT}

-- | <<images/stock-icons/stock_broken_image_24.png>>
stockMissingImage	:: StockId
stockMissingImage	= #{const_str GTK_STOCK_MISSING_IMAGE}
#if GTK_CHECK_VERSION(2,6,0)

-- | <<images/stock-icons/stock_media_forward_24.png>>
stockMediaForward	:: StockId
stockMediaForward	= #{const_str GTK_STOCK_MEDIA_FORWARD}

-- | <<images/stock-icons/stock_media_next_24.png>>
stockMediaNext  	:: StockId
stockMediaNext  	= #{const_str GTK_STOCK_MEDIA_NEXT}

-- | <<images/stock-icons/stock_media_pause_24.png>>
stockMediaPause		:: StockId
stockMediaPause		= #{const_str GTK_STOCK_MEDIA_PAUSE}

-- | <<images/stock-icons/stock_media_play_24.png>>
stockMediaPlay		:: StockId
stockMediaPlay		= #{const_str GTK_STOCK_MEDIA_PLAY}

-- | <<images/stock-icons/stock_media_previous_24.png>>
stockMediaPrevious	:: StockId
stockMediaPrevious	= #{const_str GTK_STOCK_MEDIA_PREVIOUS}

-- | <<images/stock-icons/stock_media_record_24.png>>
stockMediaRecord	:: StockId
stockMediaRecord	= #{const_str GTK_STOCK_MEDIA_RECORD}

-- | <<images/stock-icons/stock_media_rewind_24.png>>
stockMediaRewind	:: StockId
stockMediaRewind	= #{const_str GTK_STOCK_MEDIA_REWIND}

-- | <<images/stock-icons/stock_media_stop_24.png>>
stockMediaStop		:: StockId
stockMediaStop		= #{const_str GTK_STOCK_MEDIA_STOP}
#else
stockMediaForward	= stockMissingImage
stockMediaNext  	= stockMissingImage
stockMediaPause		= stockMissingImage
stockMediaPlay		= stockMissingImage
stockMediaPrevious	= stockMissingImage
stockMediaRecord	= stockMissingImage
stockMediaRewind	= stockMissingImage
stockMediaStop		= stockMissingImage
#endif
#if GTK_CHECK_VERSION(2,4,0)

-- | <<images/stock-icons/stock_network_24.png>>
stockNetwork		:: StockId
stockNetwork		= #{const_str GTK_STOCK_NETWORK}
#else
stockNetwork		= stockMissingImage
#endif

-- | <<images/stock-icons/stock_new_24.png>>
stockNew		:: StockId
stockNew		= #{const_str GTK_STOCK_NEW}

-- | <<images/stock-icons/stock_no_20.png>>
stockNo			:: StockId
stockNo			= #{const_str GTK_STOCK_NO}

-- | <<images/stock-icons/stock_ok_20.png>>
stockOk			:: StockId
stockOk			= #{const_str GTK_STOCK_OK}

-- | <<images/stock-icons/stock_open_24.png>>
stockOpen		:: StockId
stockOpen		= #{const_str GTK_STOCK_OPEN}
#if GTK_CHECK_VERSION(2,10,0)

-- | <<images/stock-icons/stock_orientation_landscape_24.png>>
stockOrientationLandscape :: StockId
stockOrientationLandscape = #{const_str GTK_STOCK_ORIENTATION_LANDSCAPE}

-- | <<images/stock-icons/stock_orientation_reverse_landscape_24.png>>
stockOrientationReverseLandscape :: StockId
stockOrientationReverseLandscape = #{const_str GTK_STOCK_ORIENTATION_REVERSE_LANDSCAPE}

-- | <<images/stock-icons/stock_orientation_portrait_24.png>>
stockOrientationPortrait  :: StockId
stockOrientationPortrait  = #{const_str GTK_STOCK_ORIENTATION_PORTRAIT}

-- | <<images/stock-icons/stock_orientation_reverse_portrait_24.png>>
stockOrientationReversePortrait  :: StockId
stockOrientationReversePortrait  = #{const_str GTK_STOCK_ORIENTATION_REVERSE_PORTRAIT}
#else
stockOrientationLandscape = stockMissingImage
stockOrientationReverseLandscape = stockMissingImage
stockOrientationPortrait  = stockMissingImage
stockOrientationReversePortrait  = stockMissingImage
#endif

-- | <<images/stock-icons/stock_paste_24.png>>
stockPaste		:: StockId
stockPaste		= #{const_str GTK_STOCK_PASTE}

-- | <<images/stock-icons/stock_preferences_24.png>>
stockPreferences	:: StockId
stockPreferences	= #{const_str GTK_STOCK_PREFERENCES}

-- | <<images/stock-icons/stock_print_24.png>>
stockPrint		:: StockId
stockPrint		= #{const_str GTK_STOCK_PRINT}

-- | <<images/stock-icons/stock_print_preview_24.png>>
stockPrintPreview	:: StockId
stockPrintPreview	= #{const_str GTK_STOCK_PRINT_PREVIEW}

-- | <<images/stock-icons/stock_properties_24.png>>
stockProperties		:: StockId
stockProperties		= #{const_str GTK_STOCK_PROPERTIES}

-- | <<images/stock-icons/stock_exit_24.png>>
stockQuit		:: StockId
stockQuit		= #{const_str GTK_STOCK_QUIT}

-- | <<images/stock-icons/stock_redo_24.png>>
stockRedo		:: StockId
stockRedo		= #{const_str GTK_STOCK_REDO}

-- | <<images/stock-icons/stock_refresh_24.png>>
stockRefresh		:: StockId
stockRefresh		= #{const_str GTK_STOCK_REFRESH}

-- | <<images/stock-icons/stock_remove_24.png>>
stockRemove		:: StockId
stockRemove		= #{const_str GTK_STOCK_REMOVE}

-- | <<images/stock-icons/stock_revert_24.png>>
stockRevertToSaved	:: StockId
stockRevertToSaved	= #{const_str GTK_STOCK_REVERT_TO_SAVED}

-- | <<images/stock-icons/stock_save_24.png>>
stockSave		:: StockId
stockSave		= #{const_str GTK_STOCK_SAVE}

-- | <<images/stock-icons/stock_save_as_24.png>>
stockSaveAs		:: StockId
stockSaveAs		= #{const_str GTK_STOCK_SAVE_AS}
#if GTK_CHECK_VERSION(2,10,0)

-- | <<images/stock-icons/stock_select_all_24.png>>
stockSelectAll		:: StockId
stockSelectAll		= #{const_str GTK_STOCK_SELECT_ALL}
#else
stockSelectAll		= stockMissingImage
#endif

-- | <<images/stock-icons/stock_colorselector_24.png>>
stockSelectColor	:: StockId
stockSelectColor	= #{const_str GTK_STOCK_SELECT_COLOR}

-- | <<images/stock-icons/stock_font_24.png>>
stockSelectFont		:: StockId
stockSelectFont		= #{const_str GTK_STOCK_SELECT_FONT}

-- | <<images/stock-icons/stock_sort_ascending_24.png>>
stockSortAscending	:: StockId
stockSortAscending	= #{const_str GTK_STOCK_SORT_ASCENDING}

-- | <<images/stock-icons/stock_sort_descending_24.png>>
stockSortDescending	:: StockId
stockSortDescending	= #{const_str GTK_STOCK_SORT_DESCENDING}

-- | <<images/stock-icons/stock_spellcheck_24.png>>
stockSpellCheck		:: StockId
stockSpellCheck		= #{const_str GTK_STOCK_SPELL_CHECK}

-- | <<images/stock-icons/stock_stop_24.png>>
stockStop		:: StockId
stockStop		= #{const_str GTK_STOCK_STOP}

-- | <<images/stock-icons/stock_text_strikethrough_24.png>>
stockStrikethrough	:: StockId
stockStrikethrough	= #{const_str GTK_STOCK_STRIKETHROUGH}

-- | <<images/stock-icons/stock_undelete_24.png>>
stockUndelete		:: StockId
stockUndelete		= #{const_str GTK_STOCK_UNDELETE}

-- | <<images/stock-icons/stock_text_underline_24.png>>
stockUnderline		:: StockId
stockUnderline		= #{const_str GTK_STOCK_UNDERLINE}

-- | <<images/stock-icons/stock_undo_24.png>>
stockUndo		:: StockId
stockUndo		= #{const_str GTK_STOCK_UNDO}
#if GTK_CHECK_VERSION(2,4,0)

-- | <<images/stock-icons/stock_text_unindent_24.png>>
stockUnindent		:: StockId
stockUnindent		= #{const_str GTK_STOCK_UNINDENT}
#else
stockUnindent		= stockMissingImage
#endif

-- | <<images/stock-icons/stock_yes_20.png>>
stockYes		:: StockId
stockYes		= #{const_str GTK_STOCK_YES}

-- | <<images/stock-icons/stock_zoom_1_24.png>>
stockZoom100		:: StockId
stockZoom100		= #{const_str GTK_STOCK_ZOOM_100}

-- | <<images/stock-icons/stock_zoom_fit_24.png>>
stockZoomFit		:: StockId
stockZoomFit		= #{const_str GTK_STOCK_ZOOM_FIT}

-- | <<images/stock-icons/stock_zoom_in_24.png>>
stockZoomIn		:: StockId
stockZoomIn		= #{const_str GTK_STOCK_ZOOM_IN}

-- | <<images/stock-icons/stock_zoom_out_24.png>>
stockZoomOut		:: StockId
stockZoomOut		= #{const_str GTK_STOCK_ZOOM_OUT}




