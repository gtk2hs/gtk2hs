-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget Alignment@
--
--  Author : Axel Simon
--          
--  Created: 15 May 2001
--
--  Version $Revision: 1.3 $ from $Date: 2003/07/09 22:42:44 $
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

module Alignment(
  Alignment,
  AlignmentClass,
  castToAlignment,
  alignmentNew,
  alignmentSet
  ) where

import Monad	(liftM)
import FFI

import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- @constructor alignmentNew@ Create an alignment widget. This widget tells
-- its child widget how to use the given space.
--
alignmentNew :: Float -> Float -> Float -> Float -> IO Alignment
alignmentNew yscale xalign yalign xscale = makeNewObject mkAlignment $
  liftM castPtr $ {#call unsafe alignment_new#} (realToFrac xalign) 
  (realToFrac yalign) (realToFrac xscale) (realToFrac yscale)


-- @method alignmentSet@ Change the space use behaviour of an
-- @ref type Alignment@.
--
alignmentSet :: AlignmentClass al => al -> Float -> Float -> Float -> Float ->
                IO ()
alignmentSet al xalign yalign xscale yscale = {#call alignment_set#}
  (toAlignment al) (realToFrac xalign) (realToFrac yalign)
  (realToFrac xscale) (realToFrac yscale)

