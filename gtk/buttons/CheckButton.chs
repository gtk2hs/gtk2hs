-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget CheckButton@
--
--  Author : Axel Simon
--          
--  Created: 15 May 2001
--
--  Version $Revision: 1.2 $ from $Date: 2002/05/24 09:43:24 $
--
--  Copyright (c) 1999..2002 Axel Simon
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
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------

module CheckButton(
  CheckButton,
  CheckButtonClass,
  castToCheckButton,
  checkButtonNew,
  checkButtonNewWithLabel,
  checkButtonNewWithMnemonic
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- @constructor checkButtonNew@ Create a new button with a check field.
--
checkButtonNew :: IO CheckButton
checkButtonNew  = makeNewObject mkCheckButton $ 
  liftM castPtr {#call unsafe check_button_new#}

-- @method checkButtonNewWithLabel@ Create a new CheckButton with a text to
-- the right of it.
--
checkButtonNewWithLabel :: String -> IO CheckButton
checkButtonNewWithLabel lbl = withCString lbl (\strPtr ->
  makeNewObject mkCheckButton $ liftM castPtr $
  {#call unsafe check_button_new_with_label#} strPtr)

-- @method checkButtonNewWithMnemonic@ Create a checkButton with an
-- accelerator key.
--
-- * Like @ref method checkButtonNewWithLabel@ but turns every underscore in
--   the label to a underlined character.
--
checkButtonNewWithMnemonic :: String -> IO CheckButton
checkButtonNewWithMnemonic lbl = withCString lbl (\strPtr ->
  makeNewObject mkCheckButton $ liftM castPtr $ 
  {#call unsafe check_button_new_with_mnemonic#} strPtr)


