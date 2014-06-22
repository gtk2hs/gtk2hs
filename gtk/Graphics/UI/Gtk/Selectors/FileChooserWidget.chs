{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget FileChooserWidget
--
--  Author : Duncan Coutts
--
--  Created: 24 April 2004
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
-- File chooser widget that can be embedded in other widgets
--
-- * Module available since Gtk+ version 2.4
--
module Graphics.UI.Gtk.Selectors.FileChooserWidget (
-- * Detail
--
-- | 'FileChooserWidget' is a widget suitable for selecting files. It is the
-- main building block of a 'FileChooserDialog'. Most applications will only
-- need to use the latter; you can use 'FileChooserWidget' as part of a larger
-- window if you have special needs.
--
-- Note that 'FileChooserWidget' does not have any methods of its own.
-- Instead, you should use the functions that work on a 'FileChooser'.

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
  castToFileChooserWidget, gTypeFileChooserWidget,
  toFileChooserWidget,

-- * Constructors
  FileChooserAction,
  fileChooserWidgetNew,
#if GTK_MAJOR_VERSION < 3
  fileChooserWidgetNewWithBackend,
#endif
#endif
  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object
{#import Graphics.UI.Gtk.Types#}

#if GTK_CHECK_VERSION(2,4,0)
{#import Graphics.UI.Gtk.Selectors.FileChooser#} (FileChooserAction)

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Interfaces

instance FileChooserClass FileChooserWidget

--------------------
-- Constructors

-- | Creates a new 'FileChooserWidget'. This is a file chooser widget that can
-- be embedded in custom windows, and it is the same widget that is used by
-- 'FileChooserDialog'.
--
fileChooserWidgetNew ::
    FileChooserAction    -- ^ @action@ - Open or save mode for the widget
 -> IO FileChooserWidget
fileChooserWidgetNew action =
  makeNewObject mkFileChooserWidget $
  liftM (castPtr :: Ptr Widget -> Ptr FileChooserWidget) $
  {# call unsafe gtk_file_chooser_widget_new #}
    ((fromIntegral . fromEnum) action)

#if GTK_MAJOR_VERSION < 3
-- | Creates a new 'FileChooserWidget' with a specified backend. This is
-- especially useful if you use 'fileChooserSetLocalOnly' to allow non-local
-- files. This is a file chooser widget that can be embedded in custom windows
-- and it is the same widget that is used by 'FileChooserDialog'.
--
-- Removed in Gtk3.
fileChooserWidgetNewWithBackend ::
    FileChooserAction    -- ^ @action@ - Open or save mode for the widget
 -> String               -- ^ @backend@ - The name of the specific filesystem
                         -- backend to use.
 -> IO FileChooserWidget
fileChooserWidgetNewWithBackend action backend =
  makeNewObject mkFileChooserWidget $
  liftM (castPtr :: Ptr Widget -> Ptr FileChooserWidget) $
  withCString backend $ \backendPtr ->
  {# call unsafe gtk_file_chooser_widget_new_with_backend #}
    ((fromIntegral . fromEnum) action)
    backendPtr
#endif
#endif
