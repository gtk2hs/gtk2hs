-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget TextTag
--
--  Author : Duncan Coutts
--
--  Created: 4 August 2004
--
--  Version $Revision: 1.6 $ from $Date: 2005/04/04 01:24:10 $
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
-- TODO
--
-- accessor functions for TextAttributes
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- A tag that can be applied to text in a 'TextBuffer'
--
module Graphics.UI.Gtk.Multiline.TextTag (
-- * Detail
-- 
-- | You may wish to begin by reading the text widget conceptual overview
-- which gives an overview of all the objects and data types related to the
-- text widget and how they work together.
--
-- Tags should be in the 'TextTagTable' for a given 'TextBuffer' before
-- using them with that buffer.
--
-- 'textBufferCreateTag' is the best way to create tags.
--
-- The \"invisible\" property was not implemented for Gtk+ 2.0; it's planned
-- to be implemented in future releases.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----TextTag
-- @

-- * Types
  TextTag,
  TextTagClass,
  castToTextTag,
  TagName,

-- * Constructors
  textTagNew,

-- * Methods
  textTagSetPriority,
  textTagGetPriority,
  TextAttributes(..),
  textAttributesNew,
  makeNewTextAttributes,  --internal

-- * Properties
  textTagPriority
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.Attributes		(Attr(..))
import System.Glib.GObject		(makeNewGObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

type TagName = String

--------------------
-- Constructors

-- | Creates a 'TextTag'.
--
textTagNew :: TagName -> IO TextTag
textTagNew name =
  makeNewGObject mkTextTag $
  withCString name $ \namePtr ->
  {# call unsafe text_tag_new #}
    namePtr

--------------------
-- Methods

-- | Get the tag priority.
--
textTagGetPriority :: TextTagClass self => self -> IO Int
textTagGetPriority self =
  liftM fromIntegral $
  {# call unsafe text_tag_get_priority #}
    (toTextTag self)

-- | Sets the priority of a 'TextTag'. Valid priorities are start at 0 and go
-- to one less than 'textTagTableGetSize'. Each tag in a table has a unique
-- priority; setting the priority of one tag shifts the priorities of all the
-- other tags in the table to maintain a unique priority for each tag. Higher
-- priority tags \"win\" if two tags both set the same text attribute. When
-- adding a tag to a tag table, it will be assigned the highest priority in the
-- table by default; so normally the precedence of a set of tags is the order
-- in which they were added to the table, or created with
-- 'textBufferCreateTag', which adds the tag to the buffer's table
-- automatically.
--
textTagSetPriority :: TextTagClass self => self -> Int -> IO ()
textTagSetPriority self priority =
  {# call text_tag_set_priority #}
    (toTextTag self)
    (fromIntegral priority)

-- TextAttributes methods

{#pointer * TextAttributes foreign newtype#}

-- | Creates a 'TextAttributes', which describes a set of properties on some
-- text.
--
textAttributesNew :: IO TextAttributes
textAttributesNew =
  {#call unsafe text_attributes_new#} >>= makeNewTextAttributes

makeNewTextAttributes :: Ptr TextAttributes -> IO TextAttributes
makeNewTextAttributes ptr =
  liftM TextAttributes $ newForeignPtr ptr (text_attributes_unref ptr)

#if __GLASGOW_HASKELL__>=600
                                                                                
foreign import ccall unsafe "&gtk_text_attributes_unref"
  text_attributes_unref' :: FinalizerPtr TextAttributes
                                                                                
text_attributes_unref :: Ptr TextAttributes -> FinalizerPtr TextAttributes
text_attributes_unref _ = text_attributes_unref'
                                                                                
#else
                                                                                
foreign import ccall unsafe "gtk_text_attributes_unref"
  text_attributes_unref :: Ptr TextAttributes -> IO ()
                                                                                
#endif

--------------------
-- Properties

-- | \'priority\' property. See 'textTagGetPriority' and 'textTagSetPriority'
--
textTagPriority :: TextTagClass self => Attr self Int
textTagPriority = Attr 
  textTagGetPriority
  textTagSetPriority
