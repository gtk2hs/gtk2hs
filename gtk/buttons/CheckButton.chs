-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: Widget CheckButton
--
--  Author : Axel Simon
--          
--  Created: 15 May 2001
--
--  Version $Revision: 1.1.1.1 $ from $Date: 2002/03/24 21:56:19 $
--
--  Copyright (c) [1999.2001] Manuel Chakravarty, Axel Simon
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
--- DESCRIPTION ---------------------------------------------------------------
--
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------

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

-- Create a new button with a check field. (EXPORTED)
--
checkButtonNew :: IO CheckButton
checkButtonNew = makeNewObject mkCheckButton $ 
  liftM castPtr {#call unsafe check_button_new#}

-- Create a new CheckButton with a text to the right of it. (EXPORTED)
--
checkButtonNewWithLabel :: String -> IO CheckButton
checkButtonNewWithLabel lbl = withCString lbl (\strPtr ->
  makeNewObject mkCheckButton $ liftM castPtr $
  {#call unsafe check_button_new_with_label#} strPtr)

-- Create a checkButton with an accelerator key. (EXPORTED)
--
-- * Like @checkButtonNewWithLabel but turns every underscore in the label
--   to a underlined character.
checkButtonNewWithMnemonic :: String -> IO CheckButton
checkButtonNewWithMnemonic lbl = withCString lbl (\strPtr ->
  makeNewObject mkCheckButton $ liftM castPtr $ 
  {#call unsafe check_button_new_with_mnemonic#} strPtr)


