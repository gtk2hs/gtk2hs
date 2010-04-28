{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) OpenGL Extension: Interface GLDrawable
--
--  Author : Duncan Coutts
--
--  Created: 9 June 2005
--
--  Copyright (C) 2005 Duncan Coutts
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
-- OpenGL rendering surface interface
--
module Graphics.UI.Gtk.OpenGL.Drawable (

-- * Class Hierarchy
-- |
-- @
-- |  GObject
-- |   +----GLDrawable
-- @

-- * Types
  GLDrawable,
  GLDrawableClass,
  castToGLDrawable,
  toGLDrawable,  

-- * Methods
  glDrawableMakeCurrent,
  glDrawableIsDoubleBuffered,
  glDrawableSwapBuffers,
  glDrawableWaitGL,
  glDrawableWaitGdk,
  glDrawableGLBegin,
  glDrawableGLEnd,
  glDrawableGetGLConfig,
  glDrawableGetSize,
  glDrawableGetCurrent,
  ) where

import Control.Monad	(liftM)

import System.Glib.FFI
import System.Glib.GObject			(makeNewGObject)
{#import Graphics.UI.Gtk.OpenGL.Types#}

{# context lib="gtkglext" prefix="gdk" #}

--------------------
-- Methods

-- | Attach an OpenGL rendering context to a GL drawable.
--
glDrawableMakeCurrent :: GLDrawableClass self => self
 -> GLContext
 -> IO Bool   -- ^ returns @True@ if it is successful, @False@ otherwise.
glDrawableMakeCurrent self glcontext =
  liftM toBool $
  {# call gdk_gl_drawable_make_current #}
    (toGLDrawable self)
    (toGLContext glcontext)

-- | Returns whether the GL drawable supports the double-buffered visual.
--
glDrawableIsDoubleBuffered :: GLDrawableClass self => self
 -> IO Bool -- ^ returns @True@ if the double-buffered visual is supported,
            -- @False@ otherwise.
glDrawableIsDoubleBuffered self =
  liftM toBool $
  {# call gdk_gl_drawable_is_double_buffered #}
    (toGLDrawable self)

-- | Exchange front and back buffers.
--
glDrawableSwapBuffers :: GLDrawableClass self => self -> IO ()
glDrawableSwapBuffers self =
  {# call gdk_gl_drawable_swap_buffers #}
    (toGLDrawable self)

-- | Complete OpenGL execution prior to subsequent Gdk drawing calls.
--
glDrawableWaitGL :: GLDrawableClass self => self -> IO ()
glDrawableWaitGL self =
  {# call gdk_gl_drawable_wait_gl #}
    (toGLDrawable self)

-- | Complete Gdk drawing execution prior to subsequent OpenGL calls.
--
glDrawableWaitGdk :: GLDrawableClass self => self -> IO ()
glDrawableWaitGdk self =
  {# call gdk_gl_drawable_wait_gdk #}
    (toGLDrawable self)

-- | Delimits the begining of the OpenGL execution.
--
glDrawableGLBegin :: GLDrawableClass self => self
 -> GLContext -- ^ @glcontext@ - a 'GLContext'.
 -> IO Bool   -- ^ returns @True@ if it is successful, @False@ otherwise.
glDrawableGLBegin self glcontext =
  liftM toBool $
  {# call gdk_gl_drawable_gl_begin #}
    (toGLDrawable self)
    (toGLContext glcontext)

-- | Delimits the end of the OpenGL execution.
--
glDrawableGLEnd :: GLDrawableClass self => self -> IO ()
glDrawableGLEnd self =
  {# call gdk_gl_drawable_gl_end #}
    (toGLDrawable self)

-- | Gets 'GLConfig' with which the GL drawable is configured.
--
glDrawableGetGLConfig :: GLDrawableClass self => self -> IO GLConfig
glDrawableGetGLConfig self =
  makeNewGObject mkGLConfig $
  {# call gdk_gl_drawable_get_gl_config #}
    (toGLDrawable self)

-- | Returns the width and height of the GL drawable.
--
glDrawableGetSize :: GLDrawableClass self => self
 -> IO (Int, Int) -- ^ @(width, height)@
glDrawableGetSize self =
  alloca $ \widthPtr ->
  alloca $ \heightPtr ->
  {# call gdk_gl_drawable_get_size #}
    (toGLDrawable self)
    widthPtr
    heightPtr
  >>
  peek widthPtr >>= \width ->
  peek heightPtr >>= \height ->
  return (fromIntegral width, fromIntegral height)

-- | Returns the current 'GLDrawable'.
--
glDrawableGetCurrent :: 
    IO (Maybe GLDrawable) -- ^ returns the current 'Drawable' or @Nothing@ if
                          -- there is no current drawable.
glDrawableGetCurrent =
  maybeNull (makeNewGObject mkGLDrawable) $
  {# call gdk_gl_drawable_get_current #}
