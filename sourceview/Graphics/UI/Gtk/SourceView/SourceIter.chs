-- -*-haskell-*-
--  GIMP Toolkit (GTK) SourceIter
--
--  Author : Duncan Coutts
--
--  Created: 15 April 2004
--
--  Version $Revision: 1.4 $ from $Date: 2005/11/26 16:00:22 $
--
--  Copyright (C) 2004-2005 Duncan Coutts, Axel Simon
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
-- Adds extra useful methods for "TextIter" for searching forwards and
-- backwards within a region in the buffer and matching brackets.
--
-- * There is no SourceIter object, just extra methods for "TextIter"
--
module Graphics.UI.Gtk.SourceView.SourceIter (

  sourceIterForwardSearch,
  sourceIterBackwardSearch,
  sourceIterFindMatchingBracket
) where

import Monad	(liftM)
import Maybe    (fromMaybe)

import System.Glib.FFI
import System.Glib.Flags		(Flags, fromFlags)
import System.Glib.UTFString
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.SourceView.Types#}
{#import Graphics.UI.Gtk.Multiline.TextIter#}

{# context lib="gtk" prefix="gtk" #}

{# enum SourceSearchFlags {underscoreToCase} deriving(Bounded) #}

instance Flags SourceSearchFlags

-- methods

-- | same as 'textIterForwardSearch' but allows
-- case insensitive search and possibly in the future regular expressions.
--
sourceIterForwardSearch :: TextIter -> String -> [SourceSearchFlags] -> 
                           Maybe TextIter -> IO (Maybe (TextIter, TextIter))
sourceIterForwardSearch ti str flags limit = do
   start  <- makeEmptyTextIter
   end <- makeEmptyTextIter
   found <- liftM toBool $ withUTFString str $ \cStr ->
     {#call unsafe source_iter_forward_search#} ti cStr
       ((fromIntegral.fromFlags) flags) start end
       (fromMaybe (TextIter nullForeignPtr) limit)
   return $ if found then Just (start,end) else Nothing

-- | same as 'textIterForwardSearch' but allows
-- case insensitive search and possibly in the future regular expressions.
--
sourceIterBackwardSearch :: TextIter -> String -> [SourceSearchFlags] -> 
                           Maybe TextIter -> IO (Maybe (TextIter, TextIter))
sourceIterBackwardSearch ti str flags limit = do
   start  <- makeEmptyTextIter
   end <- makeEmptyTextIter
   found <- liftM toBool $ withUTFString str $ \cStr ->
     {#call unsafe source_iter_backward_search#} ti cStr
       ((fromIntegral.fromFlags) flags) start end
       (fromMaybe (TextIter nullForeignPtr) limit)
   return $ if found then Just (start,end) else Nothing

-- | Tries to match the bracket character
-- currently at the given iter with its opening\/closing counterpart, and if
-- found moves iter to the position where it was found.
--
-- * the 'TextIter' must belong to a 'SourceBuffer'
--
sourceIterFindMatchingBracket :: TextIter -> IO Bool
sourceIterFindMatchingBracket ti =
  liftM toBool $ {# call unsafe source_iter_find_matching_bracket #} ti
