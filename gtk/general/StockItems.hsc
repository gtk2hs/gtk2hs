-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Stock Items@
--
--  Author : Axel Simon
--          
--  Created: 24 May 2001
--
--  Version $Revision: 1.2 $ from $Date: 2002/05/24 09:43:25 $
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
-- @description@ --------------------------------------------------------------
--
-- * A StockItem is a resource that is know throughout Gtk. Defineing you own
--   @IconSet@s as @StockItem@s will make it possible for Gtk to choose
--   the most appropriate sizes and enables themes to override you built in
--   icons. A couple of constants are defined here as well. They are useful 
--   in accessing Gtk's predefined items.
--
-- @documentation@ ------------------------------------------------------------
--
-- * The StockItem structure is completely marshaled to haskell. It is 
--   possible to marshal all strings lazily because the string pointers are
--   valid throughout the lifetime of the application. The only drawback it
--   that a stock item that is replaced by the another item with the same
--   name will never be freed. This deficiency is built into Gtk however.
--
-- @todo@ ---------------------------------------------------------------------

module StockItems(
  StockItem(StockItem),
  siStockId,
  siLabel,
  siModifier,
  siKeyval,
  siTransDom,
  stockAdd,
  stockLookup,
  stockButtonApply,
  stockButtonCancel,
  stockButtonClose,
  stockButtonNo,
  stockButtonOk,
  stockButtonYes,
  stockDialogError,
  stockDialogInfo,
  stockDialogQuestion,
  stockDialogWarning,
  stockHelp,
  stockNew,
  stockOpen,
  stockQuit,
  stockSave,
  stockClose,
  stockMissingImage
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import IOExts	(unsafePerformIO)	-- to read CStrings lazyly

import Events	(Modifier)

#include <gtk/gtk.h>

-- The StockItem structure.
--
-- * Although the structure itself is allocated dynamically, its contents
--   are not. All string pointers are constant throughout the lifetime of
--   the application. We do not need to marshal these Strings to Haskell if
--   they are not needed.
--
data StockItem = StockItem {
  siStockId :: String,
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
      siStockId  = unsafePerformIO $ peekCString' stockId,
      siLabel	 = unsafePerformIO $ peekCString' label,
      -- &%!?$ c2hs and hsc should agree on types
      siModifier = fromIntegral modifier, 
      siKeyval	 = fromIntegral keyval,
      siTransDom = unsafePerformIO $ peekCString' transDom }
    where
      peekCString' :: CString -> IO String
      peekCString' strPtr | strPtr==nullPtr = return ""
			  | otherwise	    = peekCString strPtr

  poke siPtr (StockItem {
    siStockId = stockId,
    siLabel   = label,
    siModifier= modifier,
    siKeyval  = keyval,
    siTransDom= transDom }) = do
    stockIdPtr <- newCString stockId
    #{poke GtkStockItem, stock_id} siPtr stockIdPtr
    labelPtr   <- newCString label
    #{poke GtkStockItem, label}	   siPtr labelPtr
    #{poke GtkStockItem, modifier} siPtr 
      ((fromIntegral modifier)::#{type GdkModifierType})
    #{poke GtkStockItem, keyval}   siPtr ((fromIntegral keyval)::#{type guint})
    transDomPtr<- newCString transDom
    #{poke GtkStockItem, translation_domain} siPtr transDomPtr


-- @method stockAdd@ Add new stock items to Gtk.
--
stockAdd :: [StockItem] -> IO ()
stockAdd [] = return ()
stockAdd sis = let items = length sis in do
  aPtr <- mallocArray items
  pokeArray aPtr sis
  stock_add_static aPtr (fromIntegral items)

foreign import ccall "gtk_stock_add_static" unsafe 
  stock_add_static :: Ptr StockItem -> #{type guint} -> IO ()

-- @method stockLookup@ Lookup an item in stock.
--
stockLookup :: String -> IO (Maybe StockItem)
stockLookup stockId = 
  alloca $ \siPtr ->
  withCString stockId $ \strPtr -> do
  res <- stock_lookup strPtr siPtr
  if (toBool res) then liftM Just $ peek siPtr else return Nothing

foreign import ccall "gtk_stock_lookup" unsafe
  stock_lookup :: CString -> Ptr StockItem -> IO #type gboolean


stockButtonApply	:: String
stockButtonApply	= #{const_str GTK_STOCK_APPLY}

stockButtonCancel	:: String
stockButtonCancel	= #{const_str GTK_STOCK_CANCEL}

stockButtonClose	:: String
stockButtonClose	= #{const_str GTK_STOCK_CLOSE}

stockButtonNo		:: String
stockButtonNo		= #{const_str GTK_STOCK_NO}

stockButtonOk		:: String
stockButtonOk		= #{const_str GTK_STOCK_OK}

stockButtonYes		:: String
stockButtonYes		= #{const_str GTK_STOCK_YES}

stockDialogError	:: String
stockDialogError	= #{const_str GTK_STOCK_DIALOG_ERROR}

stockDialogInfo		:: String
stockDialogInfo		= #{const_str GTK_STOCK_DIALOG_INFO}

stockDialogQuestion	:: String
stockDialogQuestion	= #{const_str GTK_STOCK_DIALOG_QUESTION}

stockDialogWarning	:: String
stockDialogWarning	= #{const_str GTK_STOCK_DIALOG_WARNING}

stockHelp		:: String
stockHelp		= #{const_str GTK_STOCK_HELP}

stockNew		:: String
stockNew		= #{const_str GTK_STOCK_NEW}

stockOpen		:: String
stockOpen		= #{const_str GTK_STOCK_OPEN}

stockQuit		:: String
stockQuit		= #{const_str GTK_STOCK_QUIT}

stockSave		:: String
stockSave		= #{const_str GTK_STOCK_SAVE}

stockClose		:: String
stockClose		= #{const_str GTK_STOCK_CLOSE}

stockMissingImage	:: String
stockMissingImage	= #{const_str GTK_STOCK_MISSING_IMAGE}






