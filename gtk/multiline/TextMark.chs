-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry TextMark for the editor widget@
--
--  Author : Axel Simon
--          
--  Created: 23 February 2002
--
--  Version $Revision: 1.2 $ from $Date: 2002/05/24 09:43:25 $
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

module TextMark(
  TextMark,
  TextMarkClass,
  castToTextMark,
  MarkName,
  textMarkSetVisible,
  textMarkGetVisible,
  textMarkGetDeleted,
  textMarkGetName,
  textMarkGetBuffer,
  textMarkGetLeftGravity
  ) where

import Monad	(liftM)
import Foreign
import CForeign
import GObject	(makeNewGObject)
{#import Hierarchy#}
{#import Signal#}

{# context lib="gtk" prefix="gtk" #}

type MarkName = String

-- methods

-- @method textMarkSetVisible@ Set the visibility of a @ref type TextMark@.
--
textMarkSetVisible :: TextMarkClass tm => tm -> Bool -> IO ()
textMarkSetVisible tm vis = 
  {#call unsafe text_mark_set_visible#} (toTextMark tm) (fromBool vis)


-- @method textMarkGetVisible@ Get the visibility of a @ref type TextMark@.
--
textMarkGetVisible :: TextMarkClass tm => tm -> IO Bool
textMarkGetVisible tm = liftM toBool $
  {#call unsafe text_mark_get_visible#} (toTextMark tm)

-- @method textMarkGetDeleted@ Query if a @ref type TextMark@ is still valid.
--
textMarkGetDeleted :: TextMarkClass tm => tm -> IO Bool
textMarkGetDeleted tm = liftM toBool $
  {#call unsafe text_mark_get_deleted#} (toTextMark tm)

-- @method textMarkGetName@ Get the name of a @ref type TextMark@.
--
-- * Returns Nothing, if the mark is anonymous.
--
textMarkGetName :: TextMarkClass tm => tm -> IO (Maybe String)
textMarkGetName tm = do
  strPtr <- {#call unsafe text_mark_get_name#} (toTextMark tm)
  if strPtr==nullPtr then return Nothing else liftM Just $ peekCString strPtr

-- @method textMarkGetBuffer@ Extract the @ref type TextBuffer@ of the mark.
--
-- * Returns Nothing if the mark was deleted.
--
textMarkGetBuffer :: TextMarkClass tm => tm -> IO (Maybe TextBuffer)
textMarkGetBuffer tm = do
  bufPtr <- {#call unsafe text_mark_get_buffer#} (toTextMark tm)
  if bufPtr==nullPtr then return Nothing else liftM Just $
    makeNewGObject mkTextBuffer (return $ castPtr bufPtr)

-- @method textMarkGetLeftGravity@ Determine whether the mark has gravity
-- towards the beginning of a line.
--
-- * The name is misleading as Arabic, Hebrew and some other languages have
--   the beginning of a line towards the right.
--
textMarkGetLeftGravity :: TextMarkClass tm => tm -> IO Bool
textMarkGetLeftGravity tm = liftM toBool $
  {#call unsafe text_mark_get_left_gravity#} (toTextMark tm)


 