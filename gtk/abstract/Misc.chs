-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Misc
--
--  Author : Manuel M. T. Chakravarty,
--	     Axel Simon
--          
--  Created: 2 May 2001
--
--  Version $Revision: 1.4 $ from $Date: 2004/05/23 15:46:02 $
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
-- |
--

module Misc(
  Misc,
  MiscClass,
  castToMisc,
  miscSetAlignment,
  miscSetPadding
  ) where

import Monad	(liftM)
import FFI

{#import Hierarchy#}
{#import Signal#}

{# context lib="gtk" prefix="gtk" #}

-- Misc type declaration

-- methods

-- | Set the alignment of the widget.
--
miscSetAlignment :: MiscClass m => m -> Double -> Double -> IO ()
miscSetAlignment misc xalign yalign =  {#call misc_set_alignment#} 
  (toMisc misc) (realToFrac xalign) (realToFrac yalign) 
    

-- | Set the amount of space to add around the widget.
--
miscSetPadding :: MiscClass m => m -> Int -> Int -> IO ()
miscSetPadding misc xpad ypad = {#call misc_set_padding#} 
  (toMisc misc) (fromIntegral xpad) (fromIntegral ypad) 
    
