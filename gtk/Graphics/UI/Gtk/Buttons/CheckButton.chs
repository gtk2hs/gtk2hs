-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget CheckButton
--
--  Author : Axel Simon
--
--  Created: 15 May 2001
--
--  Version $Revision: 1.2 $ from $Date: 2005/02/12 17:19:21 $
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
-- create widgets with a discrete toggle button.
--
module Graphics.UI.Gtk.Buttons.CheckButton (
  CheckButton,
  CheckButtonClass,
  castToCheckButton,
  checkButtonNew,
  checkButtonNewWithLabel,
  checkButtonNewWithMnemonic
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Create a new button with a check field.
--
checkButtonNew :: IO CheckButton
checkButtonNew  = makeNewObject mkCheckButton $ 
  liftM castPtr {#call unsafe check_button_new#}

-- | Create a new CheckButton with a text to
-- the right of it.
--
checkButtonNewWithLabel :: String -> IO CheckButton
checkButtonNewWithLabel lbl = withUTFString lbl (\strPtr ->
  makeNewObject mkCheckButton $ liftM castPtr $
  {#call unsafe check_button_new_with_label#} strPtr)

-- | Create a checkButton with an
-- accelerator key.
--
-- * Like 'checkButtonNewWithLabel' but turns every underscore in
--   the label to a underlined character.
--
checkButtonNewWithMnemonic :: String -> IO CheckButton
checkButtonNewWithMnemonic lbl = withUTFString lbl (\strPtr ->
  makeNewObject mkCheckButton $ liftM castPtr $ 
  {#call unsafe check_button_new_with_mnemonic#} strPtr)


