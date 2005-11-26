-- -*-haskell-*-
--  GIMP Toolkit (GTK) SourceStyleScheme
--
--  Author : Duncan Coutts
--  derived from the GtkTextView bindings by Axel Simon
--
--  Created: 22 October 2003
--
--  Version $Revision: 1.4 $ from $Date: 2005/11/26 16:00:22 $
--
--  Copyright (C) 2003-2005 Duncan Coutts, Axel Simon
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
module Graphics.UI.Gtk.SourceView.SourceStyleScheme (
  SourceStyleScheme,
  castToSourceStyleScheme,
  sourceStyleSchemeGetTagStyle,
  sourceStyleSchemeGetName,
  sourceStyleSchemeGetDefault
) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GObject	(constructNewGObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.SourceView.Types#}
import Graphics.UI.Gtk.SourceView.SourceTagStyle

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | 
-- 
sourceStyleSchemeGetTagStyle :: SourceStyleScheme -> String -> IO SourceTagStyle
sourceStyleSchemeGetTagStyle ss styleName =
  withCString styleName $ \strPtr -> do
    tsPtr <- {#call source_style_scheme_get_tag_style#} ss strPtr
    ts <- peek (castPtr tsPtr)
    {#call unsafe g_free#} tsPtr
    return ts

-- | 
-- 
sourceStyleSchemeGetName :: SourceStyleScheme -> IO String
sourceStyleSchemeGetName ss =
  {#call source_style_scheme_get_name#} ss >>= peekUTFString

-- | 
-- 
sourceStyleSchemeGetDefault :: IO SourceStyleScheme
sourceStyleSchemeGetDefault =
  constructNewGObject mkSourceStyleScheme $ liftM castPtr $
  {#call source_style_scheme_get_default#}
