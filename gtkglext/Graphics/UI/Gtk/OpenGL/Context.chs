-- -*-haskell-*-
--  GIMP Toolkit (GTK) OpenGL Extension: GLContext
--
--  Author : Duncan Coutts
--
--  Created: 9 June 2005
--
--  Version $Revision: 1.1 $ from $Date: 2005/11/09 13:40:29 $
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
-- OpenGL rendering context object
--
module Graphics.UI.Gtk.OpenGL.Context (

-- * Types
  GLContext,
  GLContextClass,
  castToGLContext,
  GLRenderType(..),

-- * Constructors
  glContextNew,

-- * Methods
--  glContextCopy,
  glContextGetGLDrawable,
  glContextGetGLConfig,
  glContextGetShareList,
  glContextIsDirect,
  glContextGetRenderType,
  glContextGetCurrent,
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.GObject			(makeNewGObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.OpenGL.Types#}

{# context lib="gtkglext" prefix="gdk" #}

data GLRenderType =
    RGBAType
  | ColorIndexType
  deriving Eq

instance Enum GLRenderType where
  fromEnum RGBAType		= 0x8014
  fromEnum ColorIndexType	= 0x8015
  toEnum 0x8014	= RGBAType
  toEnum 0x8015	= ColorIndexType

--------------------
-- Constructors

-- | Creates a new OpenGL rendering context.
--
glContextNew :: GLDrawableClass gldrawable => 
    gldrawable       -- ^ @gldrawable@ - a 'GLDrawable'.
 -> Maybe GLContext  -- ^ @shareList@ - the 'Context' with which to share
                     -- display lists and texture objects. A value of @Nothing@
                     -- indicates that no sharing is to take place.
 -> Bool             -- ^ @direct@ - whether rendering is to be done with a
                     -- direct connection to the graphics system.
 -> GLRenderType     -- ^ @renderType@ - 'RGBAType' or 'ColorIndexType'
                     -- (currently not used).
 -> IO GLContext
glContextNew gldrawable shareList direct renderType =
  makeNewGObject mkGLContext $
  {# call gdk_gl_context_new #}
    (toGLDrawable gldrawable)
    (maybe (mkGLContext nullForeignPtr) toGLContext shareList)
    (fromBool direct)
    ((fromIntegral.fromEnum) renderType)

--------------------
-- Methods

{-
-- | Copy state from @src@ rendering context to @glcontext@.
--
glContextCopy :: (ContextClass self, ContextClass src) => self
 -> src               -- ^ @src@ - the source context.
 -> {-unsigned-long-}
 -> IO Bool           -- ^ returns @False@ if it fails, @True@ otherwise.
glContextCopy self src mask =
  liftM toBool $
  {# call gdk_gl_context_copy #}
    (toGLContext self)
    (toGLContext src)
    {-mask-}
-}
-- | Gets 'GLDrawable' to which the 'GLContext' is bound.
--
glContextGetGLDrawable :: GLContext
 -> IO GLDrawable -- ^ returns the 'GLDrawable' or @Nothing@ if no 'GLDrawable'
                  -- is bound.
glContextGetGLDrawable self =
  makeNewGObject mkGLDrawable $
  {# call gdk_gl_context_get_gl_drawable #}
    (toGLContext self)

-- | Gets 'Config' with which the 'GLContext' is configured.
--
glContextGetGLConfig :: GLContext -> IO GLConfig
glContextGetGLConfig self =
  makeNewGObject mkGLConfig $
  {# call gdk_gl_context_get_gl_config #}
    (toGLContext self)

-- | Gets 'Context' with which the @glcontext@ shares the display lists and
-- texture objects.
--
glContextGetShareList :: GLContext -> IO GLContext
glContextGetShareList self =
  makeNewGObject mkGLContext $
  {# call gdk_gl_context_get_share_list #}
    (toGLContext self)

-- | Returns whether the 'GLContext' is a direct rendering context.
--
glContextIsDirect :: GLContext -> IO Bool
glContextIsDirect self =
  liftM toBool $
  {# call gdk_gl_context_is_direct #}
    (toGLContext self)

-- | Gets the 'RenderType' of the 'GLContext'.
--
glContextGetRenderType :: GLContext
 -> IO GLRenderType
glContextGetRenderType self =
  liftM (toEnum.fromIntegral) $
  {# call gdk_gl_context_get_render_type #}
    (toGLContext self)

-- | Returns the current 'GLContext'.
--
glContextGetCurrent :: 
    IO (Maybe GLContext) -- ^ returns the current 'GLContext' or @Nothing@ if
                         -- there is no current context.
glContextGetCurrent =
  maybeNull (makeNewGObject mkGLContext) $
  {# call gdk_gl_context_get_current #}
