-- -*-haskell-*-
--  GIMP Toolkit (GTK) StockItems
--
--  Author : Axel Simon
--          
--  Created: 24 May 2001
--
--  Version $Revision: 1.1 $ from $Date: 2005/01/08 15:20:28 $
--
--  Copyright (c) 1999..2003 Axel Simon
--
--  This file is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This file is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
-- |
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
-- * The StockItem structure is completely marshaled to haskell. It is 
--   possible to marshal all strings lazily because the string pointers are
--   valid throughout the lifetime of the application. The only drawback it
--   that a stock item that is replaced by the another item with the same
--   name will never be freed. This deficiency is built into Gtk however.
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
  stockNew,
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
  stockZoomOut
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.FFI	(unsafePerformIO)	-- to read CStrings lazyly
import System.Glib.GList	(GSList, fromGSListRev)
import Graphics.UI.Gtk.Gdk.Events	(Modifier)

#include <gtk/gtk.h>

-- |  A synonym for a standard button or icon.
--
type StockId = String

-- The StockItem structure.
--
-- * Although the structure itself is allocated dynamically, its contents
--   are not. All string pointers are constant throughout the lifetime of
--   the application. We do not need to marshal these Strings to Haskell if
--   they are not needed.
--
data StockItem = StockItem {
  siStockId :: StockId,
  siLabel   :: String,
  siModifier:: Modifier,
  siKeyval  :: Integer,
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
      siModifier = fromIntegral modifier, 
      siKeyval	 = fromIntegral keyval,
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
      ((fromIntegral modifier)::#{type GdkModifierType})
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
  res <- mapM peekUTFString sPtrs
  mapM_ g_free sPtrs
  return res

#if __GLASGOW_HASKELL__>=600

foreign import ccall unsafe "gtk_stock_add"
  stock_add :: Ptr StockItem -> #{type guint} -> IO ()

foreign import ccall unsafe "gtk_stock_lookup"
  stock_lookup :: CString -> Ptr StockItem -> IO #type gboolean

foreign import ccall unsafe "gtk_stock_list_ids"
  stock_list_ids :: IO GSList

foreign import ccall unsafe "g_free"
  g_free :: Ptr a -> IO ()

#else

foreign import ccall "gtk_stock_add" unsafe 
  stock_add :: Ptr StockItem -> #{type guint} -> IO ()

foreign import ccall "gtk_stock_lookup" unsafe
  stock_lookup :: CString -> Ptr StockItem -> IO #type gboolean

foreign import ccall "gtk_stock_list_ids" unsafe
  stock_list_ids :: IO GSList

foreign import ccall "g_free" unsafe
  g_free :: Ptr a -> IO ()

#endif
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
#if GTK_CHECK_VERSION(2,2,0)
-- | Standard icon and menu entry.
--
-- * This icon is only available in Gtk 2.2 or higher.
--
#endif
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.
-- | Standard icon and menu entry.


stockAdd,
  stockApply,
  stockBold,
  stockCancel,
  stockCDROM,
  stockClear,
  stockClose,
#if GTK_CHECK_VERSION(2,2,0)
  stockColorPicker,
#endif
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
  stockNew,
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
stockCopy		= #{const_str GTK_STOCK_COPY}
stockCut		= #{const_str GTK_STOCK_CUT}
stockDelete		= #{const_str GTK_STOCK_DELETE}
stockDialogError	= #{const_str GTK_STOCK_DIALOG_ERROR}
stockDialogInfo		= #{const_str GTK_STOCK_DIALOG_INFO}
stockDialogQuestion	= #{const_str GTK_STOCK_DIALOG_QUESTION}
stockDialogWarning	= #{const_str GTK_STOCK_DIALOG_WARNING}
stockDnd		= #{const_str GTK_STOCK_DND}
stockDndMultiple	= #{const_str GTK_STOCK_DND_MULTIPLE}
stockExecute		= #{const_str GTK_STOCK_EXECUTE}
stockFind		= #{const_str GTK_STOCK_FIND}
stockFindAndRelpace	= #{const_str GTK_STOCK_FIND_AND_REPLACE}
stockFloppy		= #{const_str GTK_STOCK_FLOPPY}
stockGotoBottom		= #{const_str GTK_STOCK_GOTO_BOTTOM}
stockGotoFirst		= #{const_str GTK_STOCK_GOTO_FIRST}
stockGotoLast		= #{const_str GTK_STOCK_GOTO_LAST}
stockGotoTop		= #{const_str GTK_STOCK_GOTO_TOP}
stockGoBack		= #{const_str GTK_STOCK_GO_BACK}
stockGoDown		= #{const_str GTK_STOCK_GO_DOWN}
stockGoForward		= #{const_str GTK_STOCK_GO_FORWARD}
stockGoUp		= #{const_str GTK_STOCK_GO_UP}
stockHelp		= #{const_str GTK_STOCK_HELP}
stockHome		= #{const_str GTK_STOCK_HOME}
stockIndex		= #{const_str GTK_STOCK_INDEX}
stockItalic		= #{const_str GTK_STOCK_ITALIC}
stockJumpTo		= #{const_str GTK_STOCK_JUMP_TO}
stockJustifyCenter	= #{const_str GTK_STOCK_JUSTIFY_CENTER}
stockJustifyFill	= #{const_str GTK_STOCK_JUSTIFY_FILL}
stockJustifyLeft	= #{const_str GTK_STOCK_JUSTIFY_LEFT}
stockJustifyRight	= #{const_str GTK_STOCK_JUSTIFY_RIGHT}
stockMissingImage	= #{const_str GTK_STOCK_MISSING_IMAGE}
stockNew		= #{const_str GTK_STOCK_NEW}
stockNo			= #{const_str GTK_STOCK_NO}
stockOk			= #{const_str GTK_STOCK_OK}
stockOpen		= #{const_str GTK_STOCK_OPEN}
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
stockYes		= #{const_str GTK_STOCK_YES}
stockZoom100		= #{const_str GTK_STOCK_ZOOM_100}
stockZoomFit		= #{const_str GTK_STOCK_ZOOM_FIT}
stockZoomIn		= #{const_str GTK_STOCK_ZOOM_IN}
stockZoomOut		= #{const_str GTK_STOCK_ZOOM_OUT}




