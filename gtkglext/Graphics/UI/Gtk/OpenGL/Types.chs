{-# OPTIONS -fglasgow-exts #-}  --due to use of unsafeCoerce#
-- -*-haskell-*-
-- -------------------- automatically generated file - do not edit ----------
--  Object hierarchy for the GIMP Toolkit (GTK) Binding for Haskell
--
--  Author : Axel Simon
--
--  Version $Revision: 1.1 $ from $Date: 2005/11/09 13:40:29 $
--
--  Copyright (C) 2001-2005 Axel Simon
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
-- #hide

-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- This file reflects the Gtk+ object hierarchy in terms of Haskell classes.
--
module Graphics.UI.Gtk.OpenGL.Types (
 
  GLPixmap(GLPixmap), GLPixmapClass,
  toGLPixmap, 
  fromGLPixmap, 
  mkGLPixmap, unGLPixmap,
  castToGLPixmap, 
  GLWindow(GLWindow), GLWindowClass,
  toGLWindow, 
  fromGLWindow, 
  mkGLWindow, unGLWindow,
  castToGLWindow, 
  GLContext(GLContext), GLContextClass,
  toGLContext, 
  fromGLContext, 
  mkGLContext, unGLContext,
  castToGLContext, 
  GLConfig(GLConfig), GLConfigClass,
  toGLConfig, 
  fromGLConfig, 
  mkGLConfig, unGLConfig,
  castToGLConfig, 
  GLDrawable(GLDrawable), GLDrawableClass,
  toGLDrawable, 
  fromGLDrawable, 
  mkGLDrawable, unGLDrawable,
  castToGLDrawable
  ) where

import System.Glib.FFI		(ForeignPtr, castForeignPtr, unsafeForeignPtrToPtr,
                                 CULong, withForeignPtr)
import System.Glib.GType	(GType, typeInstanceIsA)
import GHC.Base			(unsafeCoerce#)
{#import Graphics.UI.Gtk.Types#}

{# context lib="gtk" prefix="gtk" #}

-- The usage of foreignPtrToPtr should be safe as the evaluation will only be
-- forced if the object is used afterwards
--
castTo :: (GObjectClass obj, GObjectClass obj') => GType -> String
                                                -> (obj -> obj')
castTo gtype objTypeName obj =
  case toGObject obj of
    gobj@(GObject objFPtr)
      | typeInstanceIsA ((unsafeForeignPtrToPtr.castForeignPtr) objFPtr) gtype
                  -> fromGObject gobj
      | otherwise -> error $ "Cannot cast object to " ++ objTypeName


castToGLPixmap :: GObjectClass obj => obj -> GLPixmap
castToGLPixmap = castTo
  {# call fun unsafe gdk_gl_pixmap_get_type #} "GLPixmap"

castToGLWindow :: GObjectClass obj => obj -> GLWindow
castToGLWindow = castTo
  {# call fun unsafe gdk_gl_window_get_type #} "GLWindow"

castToGLContext :: GObjectClass obj => obj -> GLContext
castToGLContext = castTo
  {# call fun unsafe gdk_gl_context_get_type #} "GLContext"

castToGLConfig :: GObjectClass obj => obj -> GLConfig
castToGLConfig = castTo
  {# call fun unsafe gdk_gl_config_get_type #} "GLConfig"

castToGLDrawable :: GObjectClass obj => obj -> GLDrawable
castToGLDrawable = castTo
  {# call fun unsafe gdk_gl_drawable_get_type #} "GLDrawable"



-- ******************************************************************* GLPixmap

{#pointer *GdkGLPixmap as GLPixmap foreign newtype #}

mkGLPixmap = GLPixmap
unGLPixmap (GLPixmap o) = o

class DrawableClass o => GLPixmapClass o
toGLPixmap   :: GLPixmapClass o => o -> GLPixmap
toGLPixmap   = unsafeCoerce#
fromGLPixmap :: GLPixmapClass o => GLPixmap -> o
fromGLPixmap = unsafeCoerce#

instance GLPixmapClass GLPixmap
instance DrawableClass GLPixmap
instance GObjectClass GLPixmap


-- ******************************************************************* GLWindow

{#pointer *GdkGLWindow as GLWindow foreign newtype #}

mkGLWindow = GLWindow
unGLWindow (GLWindow o) = o

class DrawableClass o => GLWindowClass o
toGLWindow   :: GLWindowClass o => o -> GLWindow
toGLWindow   = unsafeCoerce#
fromGLWindow :: GLWindowClass o => GLWindow -> o
fromGLWindow = unsafeCoerce#

instance GLWindowClass GLWindow
instance DrawableClass GLWindow
instance GObjectClass GLWindow


-- ****************************************************************** GLContext

{#pointer *GdkGLContext as GLContext foreign newtype #}

mkGLContext = GLContext
unGLContext (GLContext o) = o

class GObjectClass o => GLContextClass o
toGLContext   :: GLContextClass o => o -> GLContext
toGLContext   = unsafeCoerce#
fromGLContext :: GLContextClass o => GLContext -> o
fromGLContext = unsafeCoerce#

instance GLContextClass GLContext
instance GObjectClass GLContext


-- ******************************************************************* GLConfig

{#pointer *GdkGLConfig as GLConfig foreign newtype #}

mkGLConfig = GLConfig
unGLConfig (GLConfig o) = o

class GObjectClass o => GLConfigClass o
toGLConfig   :: GLConfigClass o => o -> GLConfig
toGLConfig   = unsafeCoerce#
fromGLConfig :: GLConfigClass o => GLConfig -> o
fromGLConfig = unsafeCoerce#

instance GLConfigClass GLConfig
instance GObjectClass GLConfig


-- ***************************************************************** GLDrawable

{#pointer *GdkGLDrawable as GLDrawable foreign newtype #}

mkGLDrawable = GLDrawable
unGLDrawable (GLDrawable o) = o

class GObjectClass o => GLDrawableClass o
toGLDrawable   :: GLDrawableClass o => o -> GLDrawable
toGLDrawable   = unsafeCoerce#
fromGLDrawable :: GLDrawableClass o => GLDrawable -> o
fromGLDrawable = unsafeCoerce#

instance GLDrawableClass GLDrawable
instance GObjectClass GLDrawable


