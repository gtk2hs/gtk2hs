--  GIMP Toolkit (GTK) - pango non-GObject types @entry@
--
--  Author : Axel Simon
--          
--  Created: 9 Feburary 2003
--
--  Version $Revision: 1.1 $ from $Date: 2003/02/10 09:04:23 $
--
--  Copyright (c) 1999..2003 Axel Simon
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
-- * Define types used in Pango which are not derived from GObject.
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------
--
--
module PangoTypes(
  LayoutIter(LayoutIter),
  layout_iter_free,
  LayoutLine,
  mkLayoutLine
  ) where

import Monad    (liftM)
import Foreign
import UTFCForeign

{# context lib="pango" prefix="pango" #}

-- @entry PangoLayout@

-- @data LayoutIter@ An iterator to examine a layout.
--
{#pointer *PangoLayoutIter as LayoutIter foreign newtype #}

foreign import ccall "pango_layout_iter_free" unsafe
  layout_iter_free :: Ptr LayoutIter -> IO ()

-- @data LayoutLine@ A single line in a @ref data PangoLayout@.
--
{#pointer *PangoLayoutLine as LayoutLine foreign newtype #}

foreign import ccall "pango_layout_line_ref" unsafe
  layout_line_ref :: Ptr LayoutLine -> IO ()

foreign import ccall "pango_layout_line_unref" unsafe
  layout_line_unref :: Ptr LayoutLine -> IO ()

mkLayoutLine :: Ptr LayoutLine -> IO LayoutLine
mkLayoutLine llPtr = do
  layout_line_ref llPtr
  liftM LayoutLine $ newForeignPtr llPtr (layout_line_unref llPtr)

