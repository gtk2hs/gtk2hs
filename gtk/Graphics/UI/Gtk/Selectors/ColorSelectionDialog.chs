-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget ColorSelectionDialog
--
--  Author : Duncan Coutts
--
--  Created: 2 August 2004
--
--  Version $Revision: 1.4 $ from $Date: 2005/04/03 12:56:07 $
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
-- A standard dialog box for selecting a color
--
module Graphics.UI.Gtk.Selectors.ColorSelectionDialog (
-- * Detail
-- 
-- | The 'ColorSelectionDialog' provides a standard dialog which allows the
-- user to select a color much like the 'FileSelection' provides a standard
-- dialog for file selection.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Bin'
-- |                           +----'Window'
-- |                                 +----'Dialog'
-- |                                       +----ColorSelectionDialog
-- @

-- * Types
  ColorSelectionDialog,
  ColorSelectionDialogClass,
  castToColorSelectionDialog,

-- * Constructors
  colorSelectionDialogNew,
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Creates a new 'ColorSelectionDialog'.
--
colorSelectionDialogNew :: 
    String                  -- ^ @title@ - a string containing the title text
                            -- for the dialog.
 -> IO ColorSelectionDialog
colorSelectionDialogNew title =
  makeNewObject mkColorSelectionDialog $
  liftM (castPtr :: Ptr Widget -> Ptr ColorSelectionDialog) $
  withUTFString title $ \titlePtr ->
  {# call unsafe color_selection_dialog_new #}
    titlePtr
