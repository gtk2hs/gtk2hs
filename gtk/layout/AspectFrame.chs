-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget AspectFrame@
--
--  Author : Axel Simon
--          
--  Created: 15 May 2001
--
--  Version $Revision: 1.2 $ from $Date: 2002/05/24 09:43:25 $
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
-- * A frame that constrains its child to a particular aspect ratio.
--
-- @documentation@ ------------------------------------------------------------
--
-- * The @AspectFrame is useful when you want pack a widget so that it can 
--   resize but always retains the same aspect ratio. For instance, one might
--   be drawing a small preview of a larger image. @AspectFrame derives from
--   @Frame, so it can draw a label and a frame around the child. The frame
--   will be "shrink-wrapped" to the size of the child.
--
-- @todo@ ---------------------------------------------------------------------

module AspectFrame(
  AspectFrame,
  AspectFrameClass,
  castToAspectFrame,
  aspectFrameNew,
  aspectFrameSet
  ) where

import Monad	(liftM)
import Maybe	(isNothing)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- @constructor aspectFrameNew@ Create an AspectFrame widget.
--
-- * If ratio is not given, the aspect ratio is taken from the child widget.
--
-- * The frame may be augmented with a label which can be set by
--   @ref arg frameSetLabel@.
--
aspectFrameNew :: Float -> Float -> Maybe Float -> IO AspectFrame
aspectFrameNew xalign yalign ratio = makeNewObject mkAspectFrame $
  liftM castPtr $ {#call unsafe aspect_frame_new#} nullPtr (realToFrac xalign) 
  (realToFrac yalign) (maybe 0.0 realToFrac ratio) (fromBool $ isNothing ratio)


-- @method aspectFrameSet@ Change the space use behaviour of an
-- @ref type AspectFrame@.
--
aspectFrameSet :: AspectFrameClass af => af -> Float -> Float -> Maybe Float ->
                  IO ()
aspectFrameSet af xalign yalign ratio = {#call aspect_frame_set#} 
  (toAspectFrame af) (realToFrac xalign) (realToFrac yalign) 
  (maybe 0.0 realToFrac ratio) (fromBool $ isNothing ratio)
