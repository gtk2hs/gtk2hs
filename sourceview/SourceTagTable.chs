-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry SourceTagTable@
--
--  Author : Duncan Coutts
--  derived from GtkTextView bindings by Axel Simon
--          
--  Created: 22 October 2003
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
--
--
module SourceTagTable (
  SourceTagTable,
  SourceTagTableClass,
  sourceTagTableNew,
--  sourceTagTableAddTags,          --requires toGSList
  sourceTagTableRemoveSourceTags
) where

import Monad	(liftM)
import FFI
import GObject	(makeNewGObject)
{#import Hierarchy#}
{#import SourceViewType#}
{#import Signal#}
{#import SourceTag#}
import GList	(readGSList)

{# context lib="gtk" prefix="gtk" #}

-- methods

-- @constructor sourceTagTableNew@ Create a new @ref type SourceTagTable@
--
sourceTagTableNew :: IO SourceTagTable
sourceTagTableNew = makeNewGObject mkSourceTagTable
  {#call unsafe source_tag_table_new#} 

{-
-- @method sourceTagTableAddTags@
-- 
sourceTagTableAddTags :: SourceTagTable -> [SourceTag] -> IO ()
sourceTagTableAddTags tt tags =
  {#call source_tag_table_add_tags#} tt {- make GSList of SourceTag from tags -}
-}

-- @method sourceTagTableRemoveSourceTags@
-- 
sourceTagTableRemoveSourceTags :: SourceTagTable -> IO ()
sourceTagTableRemoveSourceTags tt =
  {#call source_tag_table_remove_source_tags#} tt 
