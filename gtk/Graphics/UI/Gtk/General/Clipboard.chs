-- -*-haskell-*-
--  GIMP Toolkit (GTK) Clipboard support
--
--  Author : Juergen Nicklisch
--
--  Created: 26 March 2007
--
--  Copyright (C) 2007 Juergen Nicklisch
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
-- Functions for the Clipboard.
--
module Graphics.UI.Gtk.General.Clipboard (

-- * Types
ClipboardType(..),
  
-- * Methods
clipboardGet
  ) where

import System.Glib.FFI
import System.Glib.Flags	(fromFlags)
import System.Glib.Signals
import System.Glib.GObject
{#import Graphics.UI.Gtk.Types#} 
{#import Graphics.UI.Gtk.General.DNDTypes#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Types

-- | Use ClipClipboard for the Windows-style cut/copy/paste menu items;
--  use ClipPrimary for the currently-selected text, even if it isn't
--  explicitly copied, and for middle-mouse-click (Netscape, Mozilla,
--  XEmacs, some GTK+ apps)

data ClipboardType  = ClipClipboard
                    | ClipPrimary
                      deriving (Eq,Show)

--------------------
-- Methods

-- | Returns the clipboard object for the given selection.
-- Cut/copy/paste menu items and keyboard shortcuts should use the default clipboard,
-- returned by passing GDK_SELECTION_CLIPBOARD for selection.
-- The currently-selected object or text should be provided on the clipboard
-- identified by --GDK_SELECTION_PRIMARY. Cut/copy/paste menu items conceptually
-- copy the contents of the GDK_SELECTION_PRIMARY clipboard to the default clipboard,
-- i.e. they copy the selection to what the user sees as the clipboard.
--
clipboardGet :: ClipboardType
  -> IO Clipboard
clipboardGet clipboardType = do
  makeNewGObject Clipboard $
    {#call gtk_clipboard_get#}
       (clipboardTypeToPointer clipboardType)

clipboardTypeToPointer :: ClipboardType -> Ptr ()
clipboardTypeToPointer ClipClipboard =   nullPtr `plusPtr` 1
clipboardTypeToPointer ClipPrimary   =   nullPtr `plusPtr` 69
