-- -*-haskell-*-
--  GIMP Toolkit (GTK) SourceTagTable
--
--  Author : Duncan Coutts
--  derived from GtkTextView bindings by Axel Simon
--
--  Created: 22 October 2003
--
--  Version $Revision: 1.3 $ from $Date: 2005/05/20 23:54:02 $
--
--  Copyright (C) 2003-2005 Duncan Coutts, Axel Simon
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
module Graphics.UI.Gtk.SourceView.SourceTagTable (
  SourceTagTable,
  SourceTagTableClass,
  sourceTagTableNew,
  sourceTagTableAddTags,
  sourceTagTableRemoveSourceTags
) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.GList        (fromGSList, toGSList)
import System.Glib.GObject	(makeNewGObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.SourceView.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.SourceView.SourceTag

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Create a new 'SourceTagTable'
--
sourceTagTableNew :: IO SourceTagTable
sourceTagTableNew = makeNewGObject mkSourceTagTable
  {#call unsafe source_tag_table_new#} 


-- | Add a list of tag to the table.
-- 
-- * The added tags are assigned the highest priority in the table. If a tag is
--   already present in table or has the same name as an already-added tag, then
--   it is not added to the table.
-- 
sourceTagTableAddTags :: SourceTagTable -> [SourceTag] -> IO ()
sourceTagTableAddTags tt tags = do
  let tagForeignPtrs = map unSourceTag tags
  tagList <- toGSList (map unsafeForeignPtrToPtr tagForeignPtrs)
  {#call source_tag_table_add_tags#} tt tagList
  -- destroy the list
  fromGSList tagList
  -- make sure the ForeignPtrs are not gc'd while we are still using the Ptrs
  mapM_ touchForeignPtr tagForeignPtrs

-- | 
-- 
sourceTagTableRemoveSourceTags :: SourceTagTable -> IO ()
sourceTagTableRemoveSourceTags tt =
  {#call source_tag_table_remove_source_tags#} tt 

-- | The source tag table has changed.
--
onTagChanged, afterTagChanged :: 
  SourceTagTableClass stt => stt -> IO () -> IO (ConnectId stt)
onTagChanged = connect_NONE__NONE "changed" False
afterTagChanged = connect_NONE__NONE "changed" True
