-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: Widget Alignment
--
--  Author : Axel Simon
--          
--  Created: 15 May 2001
--
--  Version $Revision: 1.1.1.1 $ from $Date: 2002/03/24 21:56:20 $
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

module Alignment(
  Alignment,
  AlignmentClass,
  castToAlignment,
  alignmentNew,
  alignmentSet
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- Create an alignment widget. This widget tells its child widget
-- how to use the given space. (EXPORTED)
--
alignmentNew :: Float -> Float -> Float -> Float -> IO Alignment
alignmentNew xalign yalign xscale yscale = makeNewObject mkAlignment $
  liftM castPtr $ {#call unsafe alignment_new#} (realToFrac xalign) 
  (realToFrac yalign) (realToFrac xscale) (realToFrac yscale)


-- Change the space use behaviour of an @Alignment. (EXPORTED)
--
alignmentSet :: AlignmentClass al => 
  Float -> Float -> Float -> Float -> al -> IO ()
alignmentSet xalign yalign xscale yscale al = {#call alignment_set#}
  (toAlignment al) (realToFrac xalign) (realToFrac yalign)
  (realToFrac xscale) (realToFrac yscale)

