-- -*-haskell-*-
--  GIMP Toolkit (GTK) SourceTagStyle
--
--  Author : Duncan Coutts
--  derived from GtkTextView bindings by Axel Simon
--          
--  Created: 20 October 2003
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
module Graphics.UI.Gtk.SourceView.SourceTagStyle (
  SourceTagStyle(..),
) where

import Maybe (isJust, fromMaybe)
import Data.Bits (testBit, bit, (.|.))

import System.Glib.FFI
import Graphics.UI.Gtk.General.Structs (Color(..))

#include <gtksourceview/gtksourcetagstyle.h>

data SourceTagStyleMask = SourceTagStyleUseBackground
                        | SourceTagStyleUseForeground
  deriving (Eq)

instance Enum SourceTagStyleMask where
  fromEnum SourceTagStyleUseBackground = 1
  fromEnum SourceTagStyleUseForeground = 2
                                                                                                              
  toEnum 1 = SourceTagStyleUseBackground
  toEnum 2 = SourceTagStyleUseForeground
  toEnum unmatched = error ("SourceTagStyleMask.toEnum: Cannot match " ++ show unmatched)
                                                                                                              
data SourceTagStyle = SourceTagStyle {
    isDefault     :: Bool,           -- readonly
    foreground    :: Maybe Color,
    background    :: Maybe Color,
    italic        :: Bool,
    bold          :: Bool,
    underline     :: Bool,
    strikethrough :: Bool
  }

instance Storable SourceTagStyle where
  sizeOf    _ = #{const sizeof(GtkSourceTagStyle)}
  alignment _ = alignment (undefined::#type gboolean)
  peek ptr = do
    (isDefault'::#type gboolean)     <- #{peek GtkSourceTagStyle, is_default} ptr
    (mask::#type guint)              <- #{peek GtkSourceTagStyle, mask} ptr
    foreground'                      <- peek (#{ptr GtkSourceTagStyle, foreground} ptr)
    background'                      <- peek (#{ptr GtkSourceTagStyle, background} ptr)
    (italic'::#type gboolean)        <- #{peek GtkSourceTagStyle, italic} ptr
    (bold'::#type gboolean)          <- #{peek GtkSourceTagStyle, bold} ptr
    (underline'::#type gboolean)     <- #{peek GtkSourceTagStyle, underline} ptr
    (strikethrough'::#type gboolean) <- #{peek GtkSourceTagStyle, strikethrough} ptr
    return SourceTagStyle {
        isDefault = toBool isDefault',
        foreground = if mask `testBit` (fromEnum SourceTagStyleUseForeground) then Just foreground' else Nothing,
        background = if mask `testBit` (fromEnum SourceTagStyleUseBackground) then Just background' else Nothing,
        italic = toBool italic',
        bold = toBool bold',
        underline = toBool underline',
        strikethrough = toBool strikethrough'
      }
  poke ptr tag = do
    #{poke GtkSourceTagStyle, is_default}    ptr  (fromBool $ isDefault tag ::#type gboolean)
    #{poke GtkSourceTagStyle, mask}          ptr  ((if isJust (foreground tag) then bit (fromEnum SourceTagStyleUseForeground) else 0)
                                                .|.(if isJust (background tag) then bit (fromEnum SourceTagStyleUseBackground) else 0) ::#type guint)
    poke (#{ptr GtkSourceTagStyle, foreground}  ptr) (fromMaybe (Color 0 0 0) (foreground tag))
    poke (#{ptr GtkSourceTagStyle, background}  ptr) (fromMaybe (Color 0 0 0) (background tag))
    #{poke GtkSourceTagStyle, italic}        ptr  (fromBool $ italic tag ::#type gboolean)
    #{poke GtkSourceTagStyle, bold}          ptr  (fromBool $ bold tag ::#type gboolean)
    #{poke GtkSourceTagStyle, underline}     ptr  (fromBool $ underline tag ::#type gboolean)
    #{poke GtkSourceTagStyle, strikethrough} ptr  (fromBool $ strikethrough tag ::#type gboolean)
