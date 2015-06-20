{-# LANGUAGE CPP, ScopedTypeVariables #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget IMContextSimple
--
--  Author : Andy Stewart
--
--  Created: 24 Mar 2010
--
--  Copyright (C) 2010 Andy Stewart
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
-- An input method context supporting table-based input methods
--
module Graphics.UI.Gtk.Misc.IMContextSimple (

-- * Class Hierarchy
--
-- |
-- @
-- |  'GObject'
-- |   +----'IMContext'
-- |         +----IMContextSimple
-- @

-- * Types
  IMContextSimple,
  IMContextSimpleClass,
  castToIMContextSimple,
  toIMContextSimple,

-- * Constructors
  imContextSimpleNew,

-- * Methods
  imContextSimpleAddTable,
  ) where

import Control.Monad    (liftM)
import Data.Map (Map)

import System.Glib.FFI
import System.Glib.UTFString
{#import Graphics.UI.Gtk.Types#}

{# context lib="gtk" prefix="gtk" #}

import qualified Data.Map as M

--------------------
-- Constructors

-- | Creates a new 'IMContextSimple'.
--
imContextSimpleNew :: IO IMContextSimple
imContextSimpleNew =
  wrapNewGObject mkIMContextSimple $
  liftM castPtr {# call gtk_im_context_simple_new #}

--------------------
-- Methods

-- | Adds an additional table to search to the input context. Each row of the table consists of
-- @maxSeqLen@ key symbols followed by two 'Int' interpreted as the high and low words of a gunicode
-- value. Tables are searched starting from the last added.
--
-- The table must be sorted in dictionary order on the numeric value of the key symbol fields. (Values
-- beyond the length of the sequence should be zero.)
--
imContextSimpleAddTable :: (IMContextSimpleClass self, GlibString string) => self
 -> Map string string -- ^ @data@ - the table
 -> Int          -- ^ @maxSeqLen@ - Maximum length of a sequence in the table
                 -- (cannot be greater than 'MaxComposeLen')
 -> Int          -- ^ @nSeqs@ - number of sequences in the table
 -> IO ()
imContextSimpleAddTable self table maxSeqLen nSeqs = do
  tableList <- mapM (\(x,y) -> do
                       nx <- newUTFString x
                       ny <- newUTFString y
                       return (nx, ny)) (M.toList table)
  withArray (concatMap (\(x,y) -> [x, y]) tableList) $ \(tablePtr :: Ptr CString) ->
      {# call gtk_im_context_simple_add_table #}
        (toIMContextSimple self)
        (castPtr tablePtr)
        (fromIntegral maxSeqLen)
        (fromIntegral nSeqs)
