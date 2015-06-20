{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
-- -*-haskell-*-

#include <gtk/gtk.h>
#include "template-hsc-gtk2hs.h"

--  GIMP Toolkit (GTK) StockItems
--
--  Author : Axel Simon
--
--  Created: 24 May 2001
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
-- * Defining you own 'Graphics.UI.Gtk.General.IconFactory.IconSet's
--   as 'StockItem's will make it possible for Gtk to choose the most
--   appropriate sizes and enables themes to override your built in
--   icons. A couple of constants are defined here as well. They are
--   useful in accessing Gtk's predefined items.
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
#if GTK_CHECK_VERSION(2,16,0)
  stockCapsLockWarning,
#endif
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
#if GTK_CHECK_VERSION(2,12,0)
  stockDiscard,
#endif
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
  stockLeaveFullscreen,
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
#if GTK_CHECK_VERSION(2,10,0)
  stockOrientationLandscape,
  stockOrientationReverseLandscape,
  stockOrientationPortrait,
  stockOrientationReversePortrait,
#endif
#if GTK_CHECK_VERSION(2,14,0)
  stockPageSetup,
#endif
  stockPaste,
  stockPreferences,
  stockPrint,
#if GTK_CHECK_VERSION(2,14,0)
  stockPrintError,
  stockPrintPaused,
  stockPrintReport,
  stockPrintWarning,
#endif
  stockPrintPreview,
  stockProperties,
  stockQuit,
  stockRedo,
  stockRefresh,
  stockRemove,
  stockRevertToSaved,
  stockSave,
  stockSaveAs,
#if GTK_CHECK_VERSION(2,10,0)
  stockSelectAll,
#endif
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

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Flags
import System.Glib.GList        (GSList, fromGSListRev)
import Graphics.UI.Gtk.Gdk.Events       (Modifier)
import Graphics.UI.Gtk.Gdk.Keys         (KeyVal)

-- |  A synonym for a standard button or icon.
--
type StockId = DefaultGlibString


-- Although the structure itself is allocated dynamically, its contents
-- are not. All string pointers are constant throughout the lifetime of
-- the application. We do not need to marshal these Strings to Haskell if
-- they are not needed.
--

-- | The description of a stock item.
--
data StockItem = StockItem {
  siStockId :: StockId,
  siLabel   :: DefaultGlibString,
  siModifier:: [Modifier],
  siKeyval  :: KeyVal,
  siTransDom:: DefaultGlibString }

instance Storable StockItem where
  sizeOf _      = #const sizeof(GtkStockItem)
  alignment _   = alignment (undefined::CString)
  peek siPtr    = do
    (stockId    :: CString) <- #{peek GtkStockItem, stock_id} siPtr
    (label      :: CString) <- #{peek GtkStockItem, label} siPtr
    (modifier   :: #gtk2hs_type GdkModifierType)
                            <- #{peek GtkStockItem, modifier} siPtr
    (keyval     :: #gtk2hs_type guint)
                            <- #{peek GtkStockItem, keyval} siPtr
    (transDom   :: CString) <- #{peek GtkStockItem, translation_domain} siPtr
    return $ StockItem {
      siStockId  = unsafePerformIO $ peekUTFString' stockId,
      siLabel    = unsafePerformIO $ peekUTFString' label,
      -- &%!?$ c2hs and hsc should agree on types
      siModifier = toFlags (fromIntegral modifier),
      siKeyval   = keyval,
      siTransDom = unsafePerformIO $ peekUTFString' transDom }
    where
      peekUTFString' :: CString -> IO DefaultGlibString
      peekUTFString' strPtr | strPtr==nullPtr = return ""
                            | otherwise       = peekUTFString strPtr

  poke siPtr (StockItem {
    siStockId = stockId,
    siLabel   = label,
    siModifier= modifier,
    siKeyval  = keyval,
    siTransDom= transDom }) = do
    stockIdPtr <- newUTFString stockId
    #{poke GtkStockItem, stock_id} siPtr stockIdPtr
    labelPtr   <- newUTFString label
    #{poke GtkStockItem, label}    siPtr labelPtr
    #{poke GtkStockItem, modifier} siPtr
      ((fromIntegral (fromFlags modifier))::#{gtk2hs_type GdkModifierType})
    #{poke GtkStockItem, keyval}   siPtr ((fromIntegral keyval)::#{gtk2hs_type guint})
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
  stock_add :: Ptr StockItem -> #{gtk2hs_type guint} -> IO ()

foreign import ccall unsafe "gtk_stock_lookup"
  stock_lookup :: CString -> Ptr StockItem -> IO #gtk2hs_type gboolean

foreign import ccall unsafe "gtk_stock_list_ids"
  stock_list_ids :: IO GSList

#if GTK_CHECK_VERSION(2,6,0)

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-about.png>>
stockAbout              :: StockId
stockAbout              = #{const_str GTK_STOCK_ABOUT}
#else
stockAbout              = stockMissingImage
#endif

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-add.png>>
stockAdd                :: StockId
stockAdd                = #{const_str GTK_STOCK_ADD}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-apply.png>>
stockApply              :: StockId
stockApply              = #{const_str GTK_STOCK_APPLY}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-bold.png>>
stockBold               :: StockId
stockBold               = #{const_str GTK_STOCK_BOLD}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-cancel.png>>
stockCancel             :: StockId
stockCancel             = #{const_str GTK_STOCK_CANCEL}

#if GTK_CHECK_VERSION(2,16,0)
-- | <<http://library.gnome.org/devel/gtk/stable/gtk-caps-lock-warning.png>>
stockCapsLockWarning    :: StockId
stockCapsLockWarning    = #{const_str GTK_STOCK_CAPS_LOCK_WARNING}
#endif

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-cdrom.png>>
stockCDROM              :: StockId
stockCDROM              = #{const_str GTK_STOCK_CDROM}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-clear.png>>
stockClear              :: StockId
stockClear              = #{const_str GTK_STOCK_CLEAR}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-close.png>>
stockClose              :: StockId
stockClose              = #{const_str GTK_STOCK_CLOSE}
#if GTK_CHECK_VERSION(2,2,0)

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-color-picker.png>>
stockColorPicker        :: StockId
stockColorPicker        = #{const_str GTK_STOCK_COLOR_PICKER}
#else
stockColorPicker        = stockMissingImage
#endif

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-convert.png>>
stockConvert            :: StockId
stockConvert            = #{const_str GTK_STOCK_CONVERT}
#if GTK_CHECK_VERSION(2,6,0)

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-connect.png>>
stockConnect            :: StockId
stockConnect            = #{const_str GTK_STOCK_CONNECT}
#else
stockConnect            = stockMissingImage
#endif

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-copy.png>>
stockCopy               :: StockId
stockCopy               = #{const_str GTK_STOCK_COPY}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-cut.png>>
stockCut                :: StockId
stockCut                = #{const_str GTK_STOCK_CUT}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-delete.png>>
stockDelete             :: StockId
stockDelete             = #{const_str GTK_STOCK_DELETE}

#if GTK_CHECK_VERSION(2,6,0)
-- | <<http://library.gnome.org/devel/gtk/stable/gtk-dialog-authentication.png>>
stockDialogAuthentication :: StockId
stockDialogAuthentication = #{const_str GTK_STOCK_DIALOG_AUTHENTICATION}
#else
stockDialogAuthentication = stockDialogQuestion
#endif

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-dialog-error.png>>
stockDialogError        :: StockId
stockDialogError        = #{const_str GTK_STOCK_DIALOG_ERROR}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-dialog-info.png>>
stockDialogInfo         :: StockId
stockDialogInfo         = #{const_str GTK_STOCK_DIALOG_INFO}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-dialog-question.png>>
stockDialogQuestion     :: StockId
stockDialogQuestion     = #{const_str GTK_STOCK_DIALOG_QUESTION}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-dialog-warning.png>>
stockDialogWarning      :: StockId
stockDialogWarning      = #{const_str GTK_STOCK_DIALOG_WARNING}
#if GTK_CHECK_VERSION(2,6,0)

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-directory.png>>
stockDirectory          :: StockId
stockDirectory          = #{const_str GTK_STOCK_DIRECTORY}
#else
stockDirectory          = stockMissingImage
#endif

#if GTK_CHECK_VERSION(2,12,0)
-- |
stockDiscard            :: StockId
stockDiscard            = #{const_str GTK_STOCK_DISCARD}
#endif

#if GTK_CHECK_VERSION(2,6,0)

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-disconnect.png>>
stockDisconnect         :: StockId
stockDisconnect         = #{const_str GTK_STOCK_DISCONNECT}
#else
stockDisconnect         = stockMissingImage
#endif

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-dnd.png>>
stockDnd                :: StockId
stockDnd                = #{const_str GTK_STOCK_DND}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-dnd-multiple.png>>
stockDndMultiple        :: StockId
stockDndMultiple        = #{const_str GTK_STOCK_DND_MULTIPLE}
#if GTK_CHECK_VERSION(2,6,0)

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-edit.png>>
stockEdit               :: StockId
stockEdit               = #{const_str GTK_STOCK_EDIT}
#else
stockEdit               = stockMissingImage
#endif

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-execute.png>>
stockExecute            :: StockId
stockExecute            = #{const_str GTK_STOCK_EXECUTE}
#if GTK_CHECK_VERSION(2,6,0)

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-file.png>>
stockFile               :: StockId
stockFile               = #{const_str GTK_STOCK_FILE}
#else
stockFile               = stockMissingImage
#endif

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-find.png>>
stockFind               :: StockId
stockFind               = #{const_str GTK_STOCK_FIND}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-find-and-replace.png>>
stockFindAndRelpace     :: StockId
stockFindAndRelpace     = #{const_str GTK_STOCK_FIND_AND_REPLACE}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-floppy.png>>
stockFloppy             :: StockId
stockFloppy             = #{const_str GTK_STOCK_FLOPPY}
#if GTK_CHECK_VERSION(2,8,0)

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-fullscreen.png>>
stockFullscreen         :: StockId
stockFullscreen         = #{const_str GTK_STOCK_FULLSCREEN}
#else
stockFullscreen         = stockMissingImage
#endif

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-goto-bottom.png>>
stockGotoBottom         :: StockId
stockGotoBottom         = #{const_str GTK_STOCK_GOTO_BOTTOM}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-goto-first-ltr.png>>
-- <<http://library.gnome.org/devel/gtk/stable/gtk-goto-first-rtl.png>>
stockGotoFirst          :: StockId
stockGotoFirst          = #{const_str GTK_STOCK_GOTO_FIRST}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-goto-last-ltr.png>>
-- <<http://library.gnome.org/devel/gtk/stable/gtk-goto-last-rtl.png>>
stockGotoLast           :: StockId
stockGotoLast           = #{const_str GTK_STOCK_GOTO_LAST}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-goto-top.png>>
stockGotoTop            :: StockId
stockGotoTop            = #{const_str GTK_STOCK_GOTO_TOP}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-go-back-ltr.png>>
-- <<http://library.gnome.org/devel/gtk/stable/gtk-go-back-rtl.png>>
stockGoBack             :: StockId
stockGoBack             = #{const_str GTK_STOCK_GO_BACK}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-go-down.png>>
stockGoDown             :: StockId
stockGoDown             = #{const_str GTK_STOCK_GO_DOWN}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-go-forward-ltr.png>>
-- <<http://library.gnome.org/devel/gtk/stable/gtk-go-forward-rtl.png>>
stockGoForward          :: StockId
stockGoForward          = #{const_str GTK_STOCK_GO_FORWARD}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-go-up.png>>
stockGoUp               :: StockId
stockGoUp               = #{const_str GTK_STOCK_GO_UP}
#if GTK_CHECK_VERSION(2,4,0)

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-harddisk.png>>
stockHarddisk           :: StockId
stockHarddisk           = #{const_str GTK_STOCK_HARDDISK}
#else
stockHarddisk           = stockMissingImage
#endif

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-help.png>>
stockHelp               :: StockId
stockHelp               = #{const_str GTK_STOCK_HELP}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-home.png>>
stockHome               :: StockId
stockHome               = #{const_str GTK_STOCK_HOME}
#if GTK_CHECK_VERSION(2,4,0)

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-indent-ltr.png>>
-- <<http://library.gnome.org/devel/gtk/stable/gtk-indent-rtl.png>>
stockIndent             :: StockId
stockIndent             = #{const_str GTK_STOCK_INDENT}
#else
stockIndent             = stockMissingImage
#endif

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-index.png>>
stockIndex              :: StockId
stockIndex              = #{const_str GTK_STOCK_INDEX}
#if GTK_CHECK_VERSION(2,8,0)

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-info.png>>
stockInfo               :: StockId
stockInfo               = #{const_str GTK_STOCK_INFO}
#else
stockInfo               = stockMissingImage
#endif

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-italic.png>>
stockItalic             :: StockId
stockItalic             = #{const_str GTK_STOCK_ITALIC}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-jump-to-ltr.png>>
-- <<http://library.gnome.org/devel/gtk/stable/gtk-jump-to-rtl.png>>
stockJumpTo             :: StockId
stockJumpTo             = #{const_str GTK_STOCK_JUMP_TO}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-justify-center.png>>
stockJustifyCenter      :: StockId
stockJustifyCenter      = #{const_str GTK_STOCK_JUSTIFY_CENTER}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-justify-fill.png>>
stockJustifyFill        :: StockId
stockJustifyFill        = #{const_str GTK_STOCK_JUSTIFY_FILL}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-justify-left.png>>
stockJustifyLeft        :: StockId
stockJustifyLeft        = #{const_str GTK_STOCK_JUSTIFY_LEFT}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-justify-right.png>>
stockJustifyRight       :: StockId
stockJustifyRight       = #{const_str GTK_STOCK_JUSTIFY_RIGHT}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-leave-fullscreen.png>>
stockLeaveFullscreen    :: StockId
stockLeaveFullscreen    = #{const_str GTK_STOCK_LEAVE_FULLSCREEN}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-missing-image.png>>
stockMissingImage       :: StockId
stockMissingImage       = #{const_str GTK_STOCK_MISSING_IMAGE}
#if GTK_CHECK_VERSION(2,6,0)

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-media-forward-ltr.png>>
-- <<http://library.gnome.org/devel/gtk/stable/gtk-media-forward-rtl.png>>
stockMediaForward       :: StockId
stockMediaForward       = #{const_str GTK_STOCK_MEDIA_FORWARD}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-media-next-ltr.png>>
-- <<http://library.gnome.org/devel/gtk/stable/gtk-media-next-rtl.png>>
stockMediaNext          :: StockId
stockMediaNext          = #{const_str GTK_STOCK_MEDIA_NEXT}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-media-pause.png>>
stockMediaPause         :: StockId
stockMediaPause         = #{const_str GTK_STOCK_MEDIA_PAUSE}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-media-play-ltr.png>>
-- <<http://library.gnome.org/devel/gtk/stable/gtk-media-play-rtl.png>>
stockMediaPlay          :: StockId
stockMediaPlay          = #{const_str GTK_STOCK_MEDIA_PLAY}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-media-previous-ltr.png>>
-- <<http://library.gnome.org/devel/gtk/stable/gtk-media-previous-rtl.png>>
stockMediaPrevious      :: StockId
stockMediaPrevious      = #{const_str GTK_STOCK_MEDIA_PREVIOUS}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-media-record.png>>
stockMediaRecord        :: StockId
stockMediaRecord        = #{const_str GTK_STOCK_MEDIA_RECORD}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-media-rewind-ltr.png>>
-- <<http://library.gnome.org/devel/gtk/stable/gtk-media-rewind-rtl.png>>
stockMediaRewind        :: StockId
stockMediaRewind        = #{const_str GTK_STOCK_MEDIA_REWIND}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-media-stop.png>>
stockMediaStop          :: StockId
stockMediaStop          = #{const_str GTK_STOCK_MEDIA_STOP}
#else
stockMediaForward       = stockMissingImage
stockMediaNext          = stockMissingImage
stockMediaPause         = stockMissingImage
stockMediaPlay          = stockMissingImage
stockMediaPrevious      = stockMissingImage
stockMediaRecord        = stockMissingImage
stockMediaRewind        = stockMissingImage
stockMediaStop          = stockMissingImage
#endif
#if GTK_CHECK_VERSION(2,4,0)

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-network.png>>
stockNetwork            :: StockId
stockNetwork            = #{const_str GTK_STOCK_NETWORK}
#else
stockNetwork            = stockMissingImage
#endif

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-new.png>>
stockNew                :: StockId
stockNew                = #{const_str GTK_STOCK_NEW}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-no.png>>
stockNo                 :: StockId
stockNo                 = #{const_str GTK_STOCK_NO}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-ok.png>>
stockOk                 :: StockId
stockOk                 = #{const_str GTK_STOCK_OK}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-open.png>>
stockOpen               :: StockId
stockOpen               = #{const_str GTK_STOCK_OPEN}
#if GTK_CHECK_VERSION(2,10,0)

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-orientation-landscape.png>>
stockOrientationLandscape :: StockId
stockOrientationLandscape = #{const_str GTK_STOCK_ORIENTATION_LANDSCAPE}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-orientation-reverse-landscape.png>>
stockOrientationReverseLandscape :: StockId
stockOrientationReverseLandscape = #{const_str GTK_STOCK_ORIENTATION_REVERSE_LANDSCAPE}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-orientation-portrait.png>>
stockOrientationPortrait  :: StockId
stockOrientationPortrait  = #{const_str GTK_STOCK_ORIENTATION_PORTRAIT}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-orientation-reverse-portrait.png>>
stockOrientationReversePortrait  :: StockId
stockOrientationReversePortrait  = #{const_str GTK_STOCK_ORIENTATION_REVERSE_PORTRAIT}
#else
stockOrientationLandscape = stockMissingImage
stockOrientationReverseLandscape = stockMissingImage
stockOrientationPortrait  = stockMissingImage
stockOrientationReversePortrait  = stockMissingImage
#endif

#if GTK_CHECK_VERSION(2,14,0)
-- | <<http://library.gnome.org/devel/gtkmm/stable/gtk-page-setup.png>>
stockPageSetup          :: StockId
stockPageSetup          = #{const_str GTK_STOCK_PAGE_SETUP}
#endif

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-paste.png>>
stockPaste              :: StockId
stockPaste              = #{const_str GTK_STOCK_PASTE}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-preferences.png>>
stockPreferences        :: StockId
stockPreferences        = #{const_str GTK_STOCK_PREFERENCES}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-print.png>>
stockPrint              :: StockId
stockPrint              = #{const_str GTK_STOCK_PRINT}

#if GTK_CHECK_VERSION(2,14,0)
-- | <<http://library.gnome.org/devel/gtk/stable/gtk-print-error.png>>
stockPrintError         :: StockId
stockPrintError         = #{const_str GTK_STOCK_PRINT_ERROR}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-print-paused.png>>
stockPrintPaused        :: StockId
stockPrintPaused        = #{const_str GTK_STOCK_PRINT_PAUSED}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-print-report.png>>
stockPrintReport        :: StockId
stockPrintReport        = #{const_str GTK_STOCK_PRINT_REPORT}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-print-warning.png>>
stockPrintWarning       :: StockId
stockPrintWarning       = #{const_str GTK_STOCK_PRINT_WARNING}
#endif

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-print-preview.png>>
stockPrintPreview       :: StockId
stockPrintPreview       = #{const_str GTK_STOCK_PRINT_PREVIEW}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-properties.png>>
stockProperties         :: StockId
stockProperties         = #{const_str GTK_STOCK_PROPERTIES}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-quit.png>>
stockQuit               :: StockId
stockQuit               = #{const_str GTK_STOCK_QUIT}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-redo-ltr.png>>
-- <<http://library.gnome.org/devel/gtk/stable/gtk-redo-rtl.png>>
stockRedo               :: StockId
stockRedo               = #{const_str GTK_STOCK_REDO}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-refresh.png>>
stockRefresh            :: StockId
stockRefresh            = #{const_str GTK_STOCK_REFRESH}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-remove.png>>
stockRemove             :: StockId
stockRemove             = #{const_str GTK_STOCK_REMOVE}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-revert-to-saved-ltr.png>>
-- <<http://library.gnome.org/devel/gtk/stable/gtk-revert-to-saved-rtl.png>>
stockRevertToSaved      :: StockId
stockRevertToSaved      = #{const_str GTK_STOCK_REVERT_TO_SAVED}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-save.png>>
stockSave               :: StockId
stockSave               = #{const_str GTK_STOCK_SAVE}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-save-as.png>>
stockSaveAs             :: StockId
stockSaveAs             = #{const_str GTK_STOCK_SAVE_AS}
#if GTK_CHECK_VERSION(2,10,0)

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-select-all.png>>
stockSelectAll          :: StockId
stockSelectAll          = #{const_str GTK_STOCK_SELECT_ALL}
#else
stockSelectAll          = stockMissingImage
#endif

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-select-color.png>>
stockSelectColor        :: StockId
stockSelectColor        = #{const_str GTK_STOCK_SELECT_COLOR}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-font.png>>
stockSelectFont         :: StockId
stockSelectFont         = #{const_str GTK_STOCK_SELECT_FONT}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-sort-ascending.png>>
stockSortAscending      :: StockId
stockSortAscending      = #{const_str GTK_STOCK_SORT_ASCENDING}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-sort-descending.png>>
stockSortDescending     :: StockId
stockSortDescending     = #{const_str GTK_STOCK_SORT_DESCENDING}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-spell-check.png>>
stockSpellCheck         :: StockId
stockSpellCheck         = #{const_str GTK_STOCK_SPELL_CHECK}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-stop.png>>
stockStop               :: StockId
stockStop               = #{const_str GTK_STOCK_STOP}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-strikethrough.png>>
stockStrikethrough      :: StockId
stockStrikethrough      = #{const_str GTK_STOCK_STRIKETHROUGH}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-undelete-ltr.png>>
-- <<http://library.gnome.org/devel/gtk/stable/gtk-undelete-rtl.png>>
stockUndelete           :: StockId
stockUndelete           = #{const_str GTK_STOCK_UNDELETE}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-underline.png>>
stockUnderline          :: StockId
stockUnderline          = #{const_str GTK_STOCK_UNDERLINE}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-undo-ltr.png>>
-- <<http://library.gnome.org/devel/gtk/stable/gtk-undo-rtl.png>>
stockUndo               :: StockId
stockUndo               = #{const_str GTK_STOCK_UNDO}
#if GTK_CHECK_VERSION(2,4,0)

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-unindent-ltr.png>>
-- <<http://library.gnome.org/devel/gtk/stable/gtk-unindent-rtl.png>>
stockUnindent           :: StockId
stockUnindent           = #{const_str GTK_STOCK_UNINDENT}
#else
stockUnindent           = stockMissingImage
#endif

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-yes.png>>
stockYes                :: StockId
stockYes                = #{const_str GTK_STOCK_YES}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-zoom-100.png>>
stockZoom100            :: StockId
stockZoom100            = #{const_str GTK_STOCK_ZOOM_100}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-zoom-fit.png>>
stockZoomFit            :: StockId
stockZoomFit            = #{const_str GTK_STOCK_ZOOM_FIT}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-zoom-in.png>>
stockZoomIn             :: StockId
stockZoomIn             = #{const_str GTK_STOCK_ZOOM_IN}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-zoom-out.png>>
stockZoomOut            :: StockId
stockZoomOut            = #{const_str GTK_STOCK_ZOOM_OUT}




