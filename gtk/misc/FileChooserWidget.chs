-- -*-haskell-*-
-- |GIMP Toolkit (GTK) @entry Widget FileChooserWidget@
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
--- Description ---------------------------------------------------------------
--
--  Language: Haskell 98 Binding Module
--
--  The file chooser dialog & widget is a replacement (introduced with gtk+ 2.4)
--  for the old and ugly GtkFileSelection dialog. It provides a better user
--  interface and an improved API
--
--  This is the widget variant of the FileChooser
--

module FileChooserWidget (
  FileChooserWidgetClass,
  FileChooserWidget,
  FileChooserAction,
  fileChooserWidgetNew,
  fileChooserWidgetNewWithBackend,
) where

import Monad (liftM)
import FFI
import Object
{#import Hierarchy#}
{#import FileChooser#} (FileChooserAction)

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
