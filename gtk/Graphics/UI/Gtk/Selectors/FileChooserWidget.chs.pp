-- -*-haskell-*-
--  GIMP Toolkit (GTK) entry Widget FileChooserWidget
--
--  Author : Duncan Coutts
--  Created: 24 April 2004
--
--  Copyright (c) 2004 Duncan Coutts
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Library General Public
--  License as published by the Free Software Foundation; either
--  version 2 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Library General Public License for more details.
--
-- |
--
--  The file chooser dialog and widget is a replacement
--  for the old "FileSel"ection dialog. It provides a better user
--  interface and an improved API.
--
-- * This is the widget variant of the "FileChooser"
--
-- * Added in GTK+ 2.4
--
module Graphics.UI.Gtk.Selectors.FileChooserWidget  (
#if GTK_CHECK_VERSION(2,4,0)
  FileChooserWidgetClass,
  FileChooserWidget,
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

{# context lib="gtk" prefix ="gtk" #}

-- The FileChooserWidget implements the FileChooser interface
-- which we model in Haskell as another instance decleration
instance FileChooserClass FileChooserWidget

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
