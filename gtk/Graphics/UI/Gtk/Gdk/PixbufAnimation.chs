{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Pixbuf Animation
--
--  Author : Matthew Arsenault
--
--  Created: 14 November 2009
--
--  Copyright (C) 2009 Matthew Arsenault
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
module Graphics.UI.Gtk.Gdk.PixbufAnimation (
-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'PixbufAnimation'
-- |        +----'PixbufSimpleAnim'
-- @

-- * Types
  PixbufAnimation,
  PixbufAnimationClass,
  castToPixbufAnimation, gTypePixbufAnimation,
  toPixbufAnimation,

  PixbufAnimationIter,
  PixbufAnimationIterClass,
  castToPixbufAnimationIter, gTypePixbufAnimationIter,
  toPixbufAnimationIter,

  PixbufSimpleAnim,
  PixbufSimpleAnimClass,
  castToPixbufSimpleAnim, gTypePixbufSimpleAnim,
  toPixbufSimpleAnim,

-- * Constructors
  pixbufAnimationNewFromFile,
#if GTK_CHECK_VERSION(2,8,0)
  pixbufSimpleAnimNew,
#endif

-- * Methods
  pixbufAnimationGetWidth,
  pixbufAnimationGetHeight,
  pixbufAnimationGetIter,
  pixbufAnimationIsStaticImage,
  pixbufAnimationGetStaticImage,
  pixbufAnimationIterAdvance,
  pixbufAnimationIterGetDelayTime,
  pixbufAnimationIterOnCurrentlyLoadingFrame,
  pixbufAnimationIterGetPixbuf,
#if GTK_CHECK_VERSION(2,8,0)
  pixbufSimpleAnimAddFrame,
#endif

#if GTK_CHECK_VERSION(2,18,0)
  pixbufSimpleAnimSetLoop,
  pixbufSimpleAnimGetLoop
#endif
  ) where

import Control.Monad (liftM)
import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GDateTime
import System.Glib.GObject
{#import Graphics.UI.Gtk.Types#}
import System.Glib.GError       (propagateGError)

{# context prefix="gdk" #}


--CHECKME: Domain error doc, GFileError ???
-- | Creates a new animation by loading it from a file. The file
--   format is detected automatically. If the file's format does not
--   support multi-frame images, then an animation with a single frame
--   will be created. Possible errors are in the 'PixbufError' and
--   'GFileError' domains.
--
-- Any of several error conditions may occur: the file could not be
-- opened, there was no loader for the file's format, there was not
-- enough memory to allocate the image buffer, or the image file
-- contained invalid data.
--
-- * If an error occurs, the function will throw an exception that can
--   be caught using e.g. 'System.Glib.GError.catchGErrorJust' and one of the
--   error codes in 'PixbufError' or 'GFileError'
--
pixbufAnimationNewFromFile :: GlibFilePath fp
                           => fp                 -- ^ Name of file to load, in the GLib file name encoding
                           -> IO PixbufAnimation -- ^ A newly-created animation
pixbufAnimationNewFromFile fname =
  wrapNewGObject mkPixbufAnimation $
  propagateGError $ \errPtrPtr ->
     withUTFFilePath fname $ \strPtr ->
#if defined (WIN32) && GTK_CHECK_VERSION(2,6,5)
     {#call unsafe pixbuf_animation_new_from_file_utf8#} strPtr errPtrPtr
#else
     {#call unsafe pixbuf_animation_new_from_file#} strPtr errPtrPtr
#endif

-- | Queries the width of the bounding box of a pixbuf animation.
pixbufAnimationGetWidth :: PixbufAnimation -- ^ An animation.
                        -> IO Int          -- ^ Width of the bounding box of the animation.
pixbufAnimationGetWidth self = liftM fromIntegral $ {#call unsafe pixbuf_animation_get_width#} self

-- | Queries the height of the bounding box of a pixbuf animation.
pixbufAnimationGetHeight :: PixbufAnimation  -- ^ An animation.
                         -> IO Int           -- ^ Height of the bounding box of the animation.
pixbufAnimationGetHeight self = liftM fromIntegral $ {#call unsafe pixbuf_animation_get_height#} self


-- | Get an iterator for displaying an animation. The iterator
--   provides the frames that should be displayed at a given time. The
--   start time would normally come from 'gGetCurrentTime', and marks
--   the beginning of animation playback. After creating an iterator,
--   you should immediately display the pixbuf returned by
--   'pixbufAnimationIterGetPixbuf'. Then, you should install a
--   timeout (with 'timeoutAdd') or by some other mechanism ensure
--   that you'll update the image after
--   'pixbufAnimationIterGetDelayTime' milliseconds. Each time the
--   image is updated, you should reinstall the timeout with the new,
--   possibly-changed delay time.
--
-- As a shortcut, if start_time is @Nothing@, the result of
-- 'gGetCurrentTime' will be used automatically.
--
-- To update the image (i.e. possibly change the result of
-- 'pixbufAnimationIterGetPixbuf' to a new frame of the animation),
-- call 'pixbufAnimationIterAdvance'.
--
--  If you're using 'PixbufLoader', in addition to updating the image
--  after the delay time, you should also update it whenever you
--  receive the area_updated signal and
--  'pixbufAnimationIterOnCurrentlyLoadingFrame' returns @True@. In
--  this case, the frame currently being fed into the loader has
--  received new data, so needs to be refreshed. The delay time for a
--  frame may also be modified after an area_updated signal, for
--  example if the delay time for a frame is encoded in the data after
--  the frame itself. So your timeout should be reinstalled after any
--  area_updated signal.
--
-- A delay time of -1 is possible, indicating "infinite."
--
pixbufAnimationGetIter :: PixbufAnimation            -- ^ a 'PixbufAnimation'
                       -> Maybe GTimeVal             -- ^ time when the animation starts playing
                       -> IO PixbufAnimationIter     -- ^ an iterator to move over the animation
pixbufAnimationGetIter self tv = maybeWith with tv $ \stPtr ->
                                 wrapNewGObject mkPixbufAnimationIter $
                                   {#call unsafe pixbuf_animation_get_iter#} self (castPtr stPtr)



-- | If you load a file with 'pixbufAnimationNewFromFile' and it turns
--   out to be a plain, unanimated image, then this function will
--   return @True@. Use 'pixbufAnimationGetStaticImage' to retrieve
--   the image.
--
pixbufAnimationIsStaticImage :: PixbufAnimation
                             -> IO Bool          -- ^ TRUE if the "animation" was really just an image
pixbufAnimationIsStaticImage self = liftM toBool $ {#call unsafe pixbuf_animation_is_static_image#} self


-- | If an animation is really just a plain image (has only one
--   frame), this function returns that image. If the animation is an
--   animation, this function returns a reasonable thing to display as
--   a static unanimated image, which might be the first frame, or
--   something more sophisticated. If an animation hasn't loaded any
--   frames yet, this function will return @Nothing@.
--
pixbufAnimationGetStaticImage :: PixbufAnimation
                              -> IO (Maybe Pixbuf) -- ^ unanimated image representing the animation
pixbufAnimationGetStaticImage self =
  maybeNull (makeNewGObject mkPixbuf) $ {#call unsafe pixbuf_animation_get_static_image#} self



-- | Possibly advances an animation to a new frame. Chooses the frame
--   based on the start time passed to 'pixbufAnimationGetIter'.
--
-- current_time would normally come from 'gGetCurrentTime', and must
-- be greater than or equal to the time passed to
-- 'pixbufAnimationGetIter', and must increase or remain unchanged
-- each time 'pixbufAnimationIterGetPixbuf' is called. That is, you
-- can't go backward in time; animations only play forward.
--
--  As a shortcut, pass @Nothing@ for the current time and
-- 'gGetCurrentTime' will be invoked on your behalf. So you only need
-- to explicitly pass current_time if you're doing something odd like
-- playing the animation at double speed.
--
-- If this function returns @False@, there's no need to update the
-- animation display, assuming the display had been rendered prior to
-- advancing; if @True@, you need to call 'animationIterGetPixbuf' and
-- update the display with the new pixbuf.
--
pixbufAnimationIterAdvance :: PixbufAnimationIter  -- ^ A 'PixbufAnimationIter'
                           -> Maybe GTimeVal       -- ^ current time
                           -> IO Bool              -- ^ @True@ if the image may need updating
pixbufAnimationIterAdvance iter currentTime = liftM toBool $ maybeWith with currentTime $ \tvPtr ->
                                                {# call unsafe pixbuf_animation_iter_advance #} iter (castPtr tvPtr)


-- | Gets the number of milliseconds the current pixbuf should be
--   displayed, or -1 if the current pixbuf should be displayed
--   forever. 'timeoutAdd' conveniently takes a timeout in
--   milliseconds, so you can use a timeout to schedule the next
--   update.
--
pixbufAnimationIterGetDelayTime :: PixbufAnimationIter  -- ^ an animation iterator
                                -> IO Int               -- ^ delay time in milliseconds (thousandths of a second)
pixbufAnimationIterGetDelayTime self = liftM fromIntegral $
  {#call unsafe pixbuf_animation_iter_get_delay_time#} self


-- | Used to determine how to respond to the area_updated signal on
--   'PixbufLoader' when loading an animation. area_updated is emitted
--   for an area of the frame currently streaming in to the loader. So
--   if you're on the currently loading frame, you need to redraw the
--   screen for the updated area.
--
pixbufAnimationIterOnCurrentlyLoadingFrame :: PixbufAnimationIter
                                           -> IO Bool              -- ^ @True@ if the frame we're on is partially loaded, or the last frame
pixbufAnimationIterOnCurrentlyLoadingFrame iter = liftM toBool $
  {# call unsafe pixbuf_animation_iter_on_currently_loading_frame #} iter

--CHECKME: referencing, usage of constructNewGObject
-- | Gets the current pixbuf which should be displayed; the pixbuf will
-- be the same size as the animation itself
-- ('pixbufAnimationGetWidth', 'pixbufAnimationGetHeight'). This
-- pixbuf should be displayed for 'pixbufAnimationIterGetDelayTime'
-- milliseconds. The caller of this function does not own a reference
-- to the returned pixbuf; the returned pixbuf will become invalid
-- when the iterator advances to the next frame, which may happen
-- anytime you call 'pixbufAnimationIterAdvance'. Copy the pixbuf to
-- keep it (don't just add a reference), as it may get recycled as you
-- advance the iterator.
--
pixbufAnimationIterGetPixbuf :: PixbufAnimationIter -- ^ an animation iterator
                                -> IO Pixbuf        -- ^ the pixbuf to be displayed
pixbufAnimationIterGetPixbuf iter = makeNewGObject mkPixbuf $
   {# call unsafe pixbuf_animation_iter_get_pixbuf #} iter


#if GTK_CHECK_VERSION(2,8,0)
-- | Creates a new, empty animation.
--
-- * Available since Gtk+ version 2.8
--
pixbufSimpleAnimNew :: Int   -- ^ the width of the animation
                    -> Int   -- ^ the height of the animation
                    -> Float -- ^ the speed of the animation, in frames per second
                    -> IO PixbufSimpleAnim  -- ^ a newly allocated 'PixbufSimpleAnim'
pixbufSimpleAnimNew width height rate = wrapNewGObject mkPixbufSimpleAnim $
  {#call unsafe pixbuf_simple_anim_new#} (fromIntegral width) (fromIntegral height) (realToFrac rate)


-- | Adds a new frame to animation. The pixbuf must have the
--   dimensions specified when the animation was constructed.
--
-- * Available since Gtk+ version 2.8
--
pixbufSimpleAnimAddFrame :: PixbufSimpleAnim   -- ^ a 'PixbufSimpleAnim'
                         -> Pixbuf             -- ^ the pixbuf to add
                         -> IO ()
pixbufSimpleAnimAddFrame psa pb = {#call unsafe pixbuf_simple_anim_add_frame#} psa pb

#endif

#if GTK_CHECK_VERSION(2,18,0)

-- | Sets whether animation should loop indefinitely when it reaches
--   the end.
--
-- * Available since Gtk+ version 2.18
--
pixbufSimpleAnimSetLoop :: PixbufSimpleAnim  -- ^ a 'PixbufSimpleAnim'
                           -> Bool           -- ^ whether to loop the animation
                           -> IO ()
pixbufSimpleAnimSetLoop animation loop = {#call unsafe pixbuf_simple_anim_set_loop#} animation (fromBool loop)


-- | Gets whether animation should loop indefinitely when it reaches
--   the end.
--
-- * Available since Gtk+ version 2.18
--
pixbufSimpleAnimGetLoop :: PixbufSimpleAnim  -- ^ a 'PixbufSimpleAnim'
                           -> IO Bool        -- ^ @True@ if the animation loops forever, @False@ otherwise
pixbufSimpleAnimGetLoop animation = liftM toBool $ {#call unsafe pixbuf_simple_anim_get_loop#} animation

#endif

