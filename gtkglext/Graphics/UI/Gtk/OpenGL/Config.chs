{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) OpenGL Extension: GLConfig
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
-- OpenGL frame buffer configuration object
--
module Graphics.UI.Gtk.OpenGL.Config (

-- * Types
  GLConfig,
  GLConfigClass,
  castToGLConfig,
  toGLConfig,
  GLConfigMode(..),

-- * Constructors
  glConfigNew,
#if GTK_CHECK_VERSION(2,2,0)
  glConfigNewForScreen,
#endif

-- * Methods
#if GTK_CHECK_VERSION(2,2,0)
  glConfigGetScreen,
#endif
  glConfigGetColormap,
--  glConfigGetVisual,
  glConfigGetDepth,
  glConfigGetLayerPlane,
  glConfigGetNAuxBuffers,
  glConfigGetNSampleBuffers,
  glConfigIsRgba,
  glConfigIsDoubleBuffered,
  glConfigIsStereo,
  glConfigHasAlpha,
  glConfigHasDepthBuffer,
  glConfigHasStencilBuffer,
  glConfigHasAccumBuffer,
  ) where

import Control.Monad	(liftM)

import System.Glib.FFI
import System.Glib.Flags
import System.Glib.GObject			(makeNewGObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.OpenGL.Types#}

{# context lib="gtkglext" prefix="gdk" #}


data GLConfigMode =
    GLModeRGB
  | GLModeRGBA        -- same as RGB
  | GLModeIndex
  | GLModeSingle
  | GLModeDouble
  | GLModeStereo
  | GLModeAlpha
  | GLModeDepth
  | GLModeStencil
  | GLModeAccum
  | GLModeMultiSample --not supported yet
  deriving (Eq,Bounded)

instance Enum GLConfigMode where
  fromEnum GLModeRGB		= 0
  fromEnum GLModeRGBA		= 0
  fromEnum GLModeIndex		= 1
  fromEnum GLModeSingle		= 0
  fromEnum GLModeDouble		= 2
  fromEnum GLModeStereo		= 4
  fromEnum GLModeAlpha		= 8
  fromEnum GLModeDepth		= 16
  fromEnum GLModeStencil	= 32
  fromEnum GLModeAccum		= 64
  fromEnum GLModeMultiSample	= 128
  toEnum 0	= GLModeRGB           -- note this is not a bijection
  toEnum 1	= GLModeIndex
  toEnum 2	= GLModeDouble
  toEnum 4	= GLModeStereo
  toEnum 8	= GLModeAlpha
  toEnum 16	= GLModeDepth
  toEnum 32	= GLModeStencil
  toEnum 64	= GLModeAccum
  toEnum 128	= GLModeMultiSample

instance Flags GLConfigMode

--------------------
-- Constructors

-- | Returns an OpenGL frame buffer configuration that match the specified
-- display mode.
--
glConfigNew :: 
    [GLConfigMode] -- ^ @mode@ - display mode bit mask.
 -> IO GLConfig
glConfigNew mode =
  makeNewGObject mkGLConfig $
  {# call gdk_gl_config_new_by_mode #}
    ((fromIntegral . fromFlags) mode)

#if GTK_CHECK_VERSION(2,2,0)
-- | Returns an OpenGL frame buffer configuration that matchs the specified
-- display mode.
--
glConfigNewForScreen :: 
    Screen         -- ^ @screen@ - target screen.
 -> [GLConfigMode] -- ^ @mode@ - display mode.
 -> IO GLConfig
glConfigNewForScreen screen mode =
  makeNewGObject mkGLConfig $
  {# call gdk_gl_config_new_by_mode_for_screen #}
    screen
    ((fromIntegral . fromFlags) mode)
#endif

--------------------
-- Methods

#if GTK_CHECK_VERSION(2,2,0)
-- | Gets the 'Screen' associated with the 'GLConfig'.
--
glConfigGetScreen :: GLConfig
 -> IO Screen -- ^ returns the 'Screen'.
glConfigGetScreen self =
  makeNewGObject mkScreen $
  {# call gdk_gl_config_get_screen #}
    (toGLConfig self)
#endif

-- | Gets the 'Colormap' that is appropriate for the OpenGL frame buffer
-- configuration.
--
glConfigGetColormap :: GLConfig
 -> IO Colormap -- ^ returns the appropriate 'Colormap'.
glConfigGetColormap self =
  makeNewGObject mkColormap $
  {# call gdk_gl_config_get_colormap #}
    (toGLConfig self)
{-
-- | Gets the 'Visual' that is appropriate for the OpenGL frame buffer
-- configuration.
--
glConfigGetVisual :: GLConfig
 -> IO Visual -- ^ returns the appropriate 'Visual'.
glConfigGetVisual self =
  makeNewGObject mkVisual $
  {# call gdk_gl_config_get_visual #}
    (toGLConfig self)
-}
-- | Gets the color depth of the OpenGL-capable visual.
--
glConfigGetDepth :: GLConfig
 -> IO Int -- ^ returns number of bits per pixel
glConfigGetDepth self =
  liftM fromIntegral $
  {# call gdk_gl_config_get_depth #}
    (toGLConfig self)

-- | Gets the layer plane (level) of the frame buffer. Zero is the default
-- frame buffer. Positive layer planes correspond to frame buffers that overlay
-- the default buffer, and negative layer planes correspond to frame buffers
-- that underlie the default frame buffer.
--
glConfigGetLayerPlane :: GLConfig
 -> IO Int -- ^ returns layer plane.
glConfigGetLayerPlane self =
  liftM fromIntegral $
  {# call gdk_gl_config_get_layer_plane #}
    (toGLConfig self)

-- | Gets the number of auxiliary color buffers.
--
glConfigGetNAuxBuffers :: GLConfig
 -> IO Int -- ^ returns number of auxiliary color buffers.
glConfigGetNAuxBuffers self =
  liftM fromIntegral $
  {# call gdk_gl_config_get_n_aux_buffers #}
    (toGLConfig self)

-- | Gets the number of multisample buffers.
--
glConfigGetNSampleBuffers :: GLConfig
 -> IO Int -- ^ returns number of multisample buffers.
glConfigGetNSampleBuffers self =
  liftM fromIntegral $
  {# call gdk_gl_config_get_n_sample_buffers #}
    (toGLConfig self)

-- | Returns whether the configured frame buffer is RGBA mode.
--
glConfigIsRgba :: GLConfig
 -> IO Bool -- ^ returns @True@ if the configured frame buffer is RGBA mode,
            -- @False@ otherwise.
glConfigIsRgba self =
  liftM toBool $
  {# call gdk_gl_config_is_rgba #}
    (toGLConfig self)

-- | Returns whether the configuration supports the double-buffered visual.
--
glConfigIsDoubleBuffered :: GLConfig
 -> IO Bool -- ^ returns @True@ if the double-buffered visual is supported,
            -- @False@ otherwise.
glConfigIsDoubleBuffered self =
  liftM toBool $
  {# call gdk_gl_config_is_double_buffered #}
    (toGLConfig self)

-- | Returns whether the configuration supports the stereo visual.
--
glConfigIsStereo :: GLConfig
 -> IO Bool -- ^ returns @True@ if the stereo visual is supported, @False@
            -- otherwise.
glConfigIsStereo self =
  liftM toBool $
  {# call gdk_gl_config_is_stereo #}
    (toGLConfig self)

-- | Returns whether the configured color buffer has alpha bits.
--
glConfigHasAlpha :: GLConfig
 -> IO Bool -- ^ returns @True@ if the color buffer has alpha bits, @False@
            -- otherwise.
glConfigHasAlpha self =
  liftM toBool $
  {# call gdk_gl_config_has_alpha #}
    (toGLConfig self)

-- | Returns whether the configured frame buffer has depth buffer.
--
glConfigHasDepthBuffer :: GLConfig
 -> IO Bool -- ^ returns @True@ if the frame buffer has depth buffer, @False@
            -- otherwise.
glConfigHasDepthBuffer self =
  liftM toBool $
  {# call gdk_gl_config_has_depth_buffer #}
    (toGLConfig self)

-- | Returns whether the configured frame buffer has stencil buffer.
--
glConfigHasStencilBuffer :: GLConfig
 -> IO Bool -- ^ returns @True@ if the frame buffer has stencil buffer,
            -- @False@ otherwise.
glConfigHasStencilBuffer self =
  liftM toBool $
  {# call gdk_gl_config_has_stencil_buffer #}
    (toGLConfig self)

-- | Returns whether the configured frame buffer has accumulation buffer.
--
glConfigHasAccumBuffer :: GLConfig
 -> IO Bool -- ^ returns @True@ if the frame buffer has accumulation buffer,
            -- @False@ otherwise.
glConfigHasAccumBuffer self =
  liftM toBool $
  {# call gdk_gl_config_has_accum_buffer #}
    (toGLConfig self)
