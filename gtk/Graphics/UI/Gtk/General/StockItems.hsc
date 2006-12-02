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

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Flags
import System.Glib.FFI	(unsafePerformIO)	-- to read CStrings lazyly
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
--   'IconFactory'.
--
-- * The list is sorted alphabetically (sorting is not Unicode aware).
--
stockListIds :: IO [StockId]
stockListIds = do
  lPtr <- stock_list_ids
  sPtrs <- fromGSListRev lPtr
  res <- mapM readUTFString sPtrs
  return res

#if __GLASGOW_HASKELL__>=600

foreign import ccall unsafe "gtk_stock_add"
  stock_add :: Ptr StockItem -> #{type guint} -> IO ()

foreign import ccall unsafe "gtk_stock_lookup"
  stock_lookup :: CString -> Ptr StockItem -> IO #type gboolean

foreign import ccall unsafe "gtk_stock_list_ids"
  stock_list_ids :: IO GSList

#else

foreign import ccall "gtk_stock_add" unsafe 
  stock_add :: Ptr StockItem -> #{type guint} -> IO ()

foreign import ccall "gtk_stock_lookup" unsafe
  stock_lookup :: CString -> Ptr StockItem -> IO #type gboolean

foreign import ccall "gtk_stock_list_ids" unsafe
  stock_list_ids :: IO GSList

#endif

-- | Standard icon and menu entry.
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
  stockCopy,
  stockCut,
  stockDelete,
  stockDialogError,
  stockDialogInfo,
  stockDialogQuestion,
  stockDialogWarning,
  stockDnd,
  stockDndMultiple,
  stockExecute,
  stockFind,
  stockFindAndRelpace,
  stockFloppy,
  stockGotoBottom,
  stockGotoFirst,
  stockGotoLast,
  stockGotoTop,
  stockGoBack,
  stockGoDown,
  stockGoForward,
  stockGoUp,
  stockHelp,
  stockHome,
  stockIndex,
  stockItalic,
  stockJumpTo,
  stockJustifyCenter,
  stockJustifyFill,
  stockJustifyLeft,
  stockJustifyRight,
  stockMissingImage,
  stockMediaForward,
  stockMediaNext,
  stockMediaPause,
  stockMediaPlay,
  stockMediaPrevious,
  stockMediaRecord,
  stockMediaRewind,
  stockMediaStop,
  stockNew,
  stockNetwork,
  stockNo,
  stockOk,
  stockOpen,
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
  stockYes,
  stockZoom100,
  stockZoomFit,
  stockZoomIn,
  stockZoomOut :: StockId

#if GTK_CHECK_VERSION(2,6,0)
stockAbout		= #{const_str GTK_STOCK_ABOUT}
#else
stockAbout		= stockMissingImage
#endif
stockAdd		= #{const_str GTK_STOCK_ADD}
stockApply		= #{const_str GTK_STOCK_APPLY}
stockBold		= #{const_str GTK_STOCK_BOLD}
stockCancel		= #{const_str GTK_STOCK_CANCEL}
stockCDROM		= #{const_str GTK_STOCK_CDROM}
stockClear		= #{const_str GTK_STOCK_CLEAR}
stockClose		= #{const_str GTK_STOCK_CLOSE}
#if GTK_CHECK_VERSION(2,2,0)
stockColorPicker	= #{const_str GTK_STOCK_COLOR_PICKER}
#else
stockColorPicker        = stockMissingImage
#endif
stockConvert		= #{const_str GTK_STOCK_CONVERT}
#if GTK_CHECK_VERSION(2,6,0)
stockConnect		= #{const_str GTK_STOCK_CONNECT}
#else
stockConnect		= stockMissingImage
#endif
stockCopy		= #{const_str GTK_STOCK_COPY}
stockCut		= #{const_str GTK_STOCK_CUT}
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
stockDialogInfo		= #{const_str GTK_STOCK_DIALOG_INFO}
stockDialogQuestion	= #{const_str GTK_STOCK_DIALOG_QUESTION}
stockDialogWarning	= #{const_str GTK_STOCK_DIALOG_WARNING}
#if GTK_CHECK_VERSION(2,6,0)
stockDirectory		= #{const_str GTK_STOCK_DIRECTORY}
#else
stockDirectory		= stockMissingImage
#endif
#if GTK_CHECK_VERSION(2,6,0)
stockDisconnect		= #{const_str GTK_STOCK_DISCONNECT}
#else
stockDisconnect		= stockMissingImage
#endif
stockDnd		= #{const_str GTK_STOCK_DND}
stockDndMultiple	= #{const_str GTK_STOCK_DND_MULTIPLE}
#if GTK_CHECK_VERSION(2,6,0)
stockEdit		= #{const_str GTK_STOCK_EDIT}
#else
stockEdit		= stockMissingImage
#endif
stockExecute		= #{const_str GTK_STOCK_EXECUTE}
#if GTK_CHECK_VERSION(2,6,0)
stockFile		= #{const_str GTK_STOCK_FILE}
#else
stockFile		= stockMissingImage
#endif
stockFind		= #{const_str GTK_STOCK_FIND}
stockFindAndRelpace	= #{const_str GTK_STOCK_FIND_AND_REPLACE}
stockFloppy		= #{const_str GTK_STOCK_FLOPPY}
#if GTK_CHECK_VERSION(2,8,0)
stockFullscreen		= #{const_str GTK_STOCK_FULLSCREEN}
#else
stockFullscreen		= stockMissingImage
#endif
stockGotoBottom		= #{const_str GTK_STOCK_GOTO_BOTTOM}
stockGotoFirst		= #{const_str GTK_STOCK_GOTO_FIRST}
stockGotoLast		= #{const_str GTK_STOCK_GOTO_LAST}
stockGotoTop		= #{const_str GTK_STOCK_GOTO_TOP}
stockGoBack		= #{const_str GTK_STOCK_GO_BACK}
stockGoDown		= #{const_str GTK_STOCK_GO_DOWN}
stockGoForward		= #{const_str GTK_STOCK_GO_FORWARD}
stockGoUp		= #{const_str GTK_STOCK_GO_UP}
#if GTK_CHECK_VERSION(2,4,0)
stockHarddisk		= #{const_str GTK_STOCK_HARDDISK}
#else
stockHarddisk		= stockMissingImage
#endif
stockHelp		= #{const_str GTK_STOCK_HELP}
stockHome		= #{const_str GTK_STOCK_HOME}
#if GTK_CHECK_VERSION(2,4,0)
stockIndent		= #{const_str GTK_STOCK_INDENT}
#else
stockIndent		= stockMissingImage
#endif
stockIndex		= #{const_str GTK_STOCK_INDEX}
#if GTK_CHECK_VERSION(2,8,0)
stockInfo		= #{const_str GTK_STOCK_INFO}
#else
stockInfo		= stockMissingImage
#endif
stockItalic		= #{const_str GTK_STOCK_ITALIC}
stockJumpTo		= #{const_str GTK_STOCK_JUMP_TO}
stockJustifyCenter	= #{const_str GTK_STOCK_JUSTIFY_CENTER}
stockJustifyFill	= #{const_str GTK_STOCK_JUSTIFY_FILL}
stockJustifyLeft	= #{const_str GTK_STOCK_JUSTIFY_LEFT}
stockJustifyRight	= #{const_str GTK_STOCK_JUSTIFY_RIGHT}
stockMissingImage	= #{const_str GTK_STOCK_MISSING_IMAGE}
#if GTK_CHECK_VERSION(2,6,0)
stockMediaForward	= #{const_str GTK_STOCK_MEDIA_FORWARD}
stockMediaNext  	= #{const_str GTK_STOCK_MEDIA_NEXT}
stockMediaPause		= #{const_str GTK_STOCK_MEDIA_PAUSE}
stockMediaPlay		= #{const_str GTK_STOCK_MEDIA_PLAY}
stockMediaPrevious	= #{const_str GTK_STOCK_MEDIA_PREVIOUS}
stockMediaRecord	= #{const_str GTK_STOCK_MEDIA_RECORD}
stockMediaRewind	= #{const_str GTK_STOCK_MEDIA_REWIND}
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
stockNetwork		= #{const_str GTK_STOCK_NETWORK}
#else
stockNetwork		= stockMissingImage
#endif
stockNew		= #{const_str GTK_STOCK_NEW}
stockNo			= #{const_str GTK_STOCK_NO}
stockOk			= #{const_str GTK_STOCK_OK}
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
stockPreferences	= #{const_str GTK_STOCK_PREFERENCES}
stockPrint		= #{const_str GTK_STOCK_PRINT}
stockPrintPreview	= #{const_str GTK_STOCK_PRINT_PREVIEW}
stockProperties		= #{const_str GTK_STOCK_PROPERTIES}
stockQuit		= #{const_str GTK_STOCK_QUIT}
stockRedo		= #{const_str GTK_STOCK_REDO}
stockRefresh		= #{const_str GTK_STOCK_REFRESH}
stockRemove		= #{const_str GTK_STOCK_REMOVE}
stockRevertToSaved	= #{const_str GTK_STOCK_REVERT_TO_SAVED}
stockSave		= #{const_str GTK_STOCK_SAVE}
stockSaveAs		= #{const_str GTK_STOCK_SAVE_AS}
#if GTK_CHECK_VERSION(2,10,0)
stockSelectAll		= #{const_str GTK_STOCK_SELECT_ALL}
#else
stockSelectAll		= stockMissingImage
#endif
stockSelectColor	= #{const_str GTK_STOCK_SELECT_COLOR}
stockSelectFont		= #{const_str GTK_STOCK_SELECT_FONT}
stockSortAscending	= #{const_str GTK_STOCK_SORT_ASCENDING}
stockSortDescending	= #{const_str GTK_STOCK_SORT_DESCENDING}
stockSpellCheck		= #{const_str GTK_STOCK_SPELL_CHECK}
stockStop		= #{const_str GTK_STOCK_STOP}
stockStrikethrough	= #{const_str GTK_STOCK_STRIKETHROUGH}
stockUndelete		= #{const_str GTK_STOCK_UNDELETE}
stockUnderline		= #{const_str GTK_STOCK_UNDERLINE}
stockUndo		= #{const_str GTK_STOCK_UNDO}
#if GTK_CHECK_VERSION(2,4,0)
stockUnindent		= #{const_str GTK_STOCK_UNINDENT}
#else
stockUnindent		= stockMissingImage
#endif
stockYes		= #{const_str GTK_STOCK_YES}
stockZoom100		= #{const_str GTK_STOCK_ZOOM_100}
stockZoomFit		= #{const_str GTK_STOCK_ZOOM_FIT}
stockZoomIn		= #{const_str GTK_STOCK_ZOOM_IN}
stockZoomOut		= #{const_str GTK_STOCK_ZOOM_OUT}




