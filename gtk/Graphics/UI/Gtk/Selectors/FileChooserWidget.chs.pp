-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget FileChooserWidget
--
--  Author : Duncan Coutts
--
--  Created: 24 April 2004
--
--  Version $Revision: 1.4 $ from $Date: 2005/02/25 22:53:43 $
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
--  for the old 'FileSel'ection dialog. It provides a better user
--  interface and an improved API.
--
-- * This is the widget variant of the 'FileChooser'
--
-- * Added in GTK+ 2.4
--
module Graphics.UI.Gtk.Selectors.FileChooserWidget (
-- * Description
-- 
-- | 'FileChooserWidget' is a widget suitable for selecting files. It is the
-- main building block of a 'FileChooserDialog'. Most applications will only
-- need to use the latter; you can use 'FileChooserWidget' as part of a larger
-- window if you have special needs.
--
-- Note that 'FileChooserWidget' does not have any methods of its own.
-- Instead, you should use the functions that work on a 'FileChooser'.
--
-- * Module available since Gtk version 2.4

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Box'
-- |                           +----'VBox'
-- |                                 +----FileChooserWidget
-- @


#if GTK_CHECK_VERSION(2,4,0)
-- * Types
  FileChooserWidget,
  FileChooserWidgetClass,
  castToFileChooserWidget,

-- * Constructors
  FileChooserAction,
  fileChooserWidgetNew,
  fileChooserWidgetNewWithBackend,
#endif
  ) where

#if GTK_CHECK_VERSION(2,4,0)

import Monad (liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Selectors.FileChooser#} (FileChooserAction)

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Interfaces

instance FileChooserClass FileChooserWidget

--------------------
-- Constructors

fileChooserWidgetNew ::  FileChooserAction -> IO FileChooserWidget
fileChooserWidgetNew action =
  makeNewObject mkFileChooserWidget $ liftM castPtr $
  {# call unsafe gtk_file_chooser_widget_new #}
    (fromIntegral $ fromEnum action)

fileChooserWidgetNewWithBackend ::  FileChooserAction -> String -> IO FileChooserWidget
fileChooserWidgetNewWithBackend action backend =
  makeNewObject mkFileChooserWidget $ liftM castPtr $
  withCString backend $ \strPtr ->
  {# call unsafe gtk_file_chooser_widget_new_with_backend #}
    (fromIntegral $ fromEnum action) strPtr

#endif
