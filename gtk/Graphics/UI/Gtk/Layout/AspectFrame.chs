-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget AspectFrame
--
--  Author : Axel Simon
--
--  Created: 15 May 2001
--
--  Version $Revision: 1.2 $ from $Date: 2005/02/12 17:19:23 $
--
--  Copyright (C) 1999-2005 Axel Simon
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
-- A frame that constrains its child to a particular aspect ratio.
--
-- * The 'AspectFrame' is useful when you want pack a widget so 
--   that it can 
--   resize but always retains the same aspect ratio. For instance, one might
--   be drawing a small preview of a larger image. 'AspectFrame'
--   derives from
--   'Frame', so it can draw a label and a frame around the child. 
--   The frame
--   will be \"shrink-wrapped\" to the size of the child.
--
module Graphics.UI.Gtk.Layout.AspectFrame (
  AspectFrame,
  AspectFrameClass,
  castToAspectFrame,
  aspectFrameNew,
  aspectFrameSet
  ) where

import Monad	(liftM)
import Maybe	(isNothing)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Create an AspectFrame widget.
--
-- * If ratio is not given, the aspect ratio is taken from the child widget.
--
-- * The frame may be augmented with a label which can be set by
--   @frameSetLabel@.
--
aspectFrameNew :: Float -> Float -> Maybe Float -> IO AspectFrame
aspectFrameNew xalign yalign ratio = makeNewObject mkAspectFrame $
  liftM castPtr $ {#call unsafe aspect_frame_new#} nullPtr (realToFrac xalign) 
  (realToFrac yalign) (maybe 0.0 realToFrac ratio) (fromBool $ isNothing ratio)


-- | Change the space use behaviour of an
-- 'AspectFrame'.
--
aspectFrameSet :: AspectFrameClass af => af -> Float -> Float -> Maybe Float ->
                  IO ()
aspectFrameSet af xalign yalign ratio = {#call aspect_frame_set#} 
  (toAspectFrame af) (realToFrac xalign) (realToFrac yalign) 
  (maybe 0.0 realToFrac ratio) (fromBool $ isNothing ratio)
