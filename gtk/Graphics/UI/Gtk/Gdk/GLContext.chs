{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) GLContext
--
--  Author : Chris Mennie
--
--  Created: 23 April 2016
--
--  Copyright (C) 2016 Chis Mennie
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
-- OpenGL context
--
module Graphics.UI.Gtk.Gdk.GLContext (
-- * Detail
--
-- | GLContext is an object representing the platform-specific OpenGL drawing context.
--
-- GLContexts are created for a GdkWindow, and the context will match the GdkVisual of the window.
--
-- A 'GLContext' is not tied to any particular normal framebuffer. For instance, it cannot draw to
-- the Window back buffer. The GDK repaint system is in full control of the painting to that.
-- GDK will handle the integration of your rendering with that of other widgets.
--
-- Support for 'GLContext' is platform-specific, context creation can fail, returning NULL context.
--
-- A 'GLContext' has to be made "current" in order to start using it, otherwise any OpenGL call will
-- be ignored.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'GLContext'
-- @
--

-- * Types
  GLContext,
  GLContextClass,
  castToGLContext, gTypeGLContext,

-- * Methods
    glContextGetDisplay,
    glContextGetWindow,
    glContextGetSharedContext,
    glContextGetVersion,
    glContextSetRequiredVersion,
    glContextGetRequiredVersion,
    glContextSetDebugEnabled,
    glContextGetDebugEnabled,
    glContextSetForwardCompatible,
    glContextGetForwardCompatible,
    glContextRealize,
    #if GTK_CHECK_VERSION(3,20,0)
    glContextIsLegacy,
    #endif
    glContextMakeCurrent,
    glContextGetCurrent,
    glContextClearCurrent
  ) where

import Control.Monad    (liftM)
import Data.Maybe       (fromMaybe)

import System.Glib.FFI
import System.Glib.Flags                (toFlags)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Gdk.Enums#}
{#import Graphics.UI.Gtk.Gdk.Cursor#}
import Graphics.UI.Gtk.General.Structs
import System.Glib.GError       (propagateGError)
import System.Glib.Attributes

{# context lib="gdk" prefix="gdk" #}


--------------------
-- Methods


-- | Retrieves the 'Graphics.UI.Gtk.Gdk.Display.Display' the context is created for.
--
glContextGetDisplay :: GLContextClass self => self -> IO (Maybe Display)
glContextGetDisplay self = do
    maybeNull (wrapNewGObject mkDisplay) $
        {# call gdk_gl_context_get_display #} (toGLContext self)


-- | Retrieves the 'Graphics.UI.Gtk.Gdk.DrawWindow.DrawWindow' used by the context.
--
glContextGetWindow :: GLContextClass self => self -> IO (Maybe DrawWindow)
glContextGetWindow self = do
    maybeNull (wrapNewGObject mkDrawWindow) $
        {# call gdk_gl_context_get_window #} (toGLContext self)


-- | Retrieves the 'GLContext' that this context share data with.
--
glContextGetSharedContext :: GLContextClass self => self -> IO (Maybe GLContext)
glContextGetSharedContext self = do
    maybeNull (wrapNewGObject mkGLContext) $
        {# call gdk_gl_context_get_shared_context #} (toGLContext self)


-- | Retrieves the OpenGL version of the context.
--
-- The context must be realized prior to calling this function.
--
glContextGetVersion :: GLContextClass self => self -> IO (Int, Int)
glContextGetVersion self = do
    alloca $ \majorPtr -> alloca $ \minorPtr -> do
        {# call gdk_gl_context_get_version #} (toGLContext self) majorPtr minorPtr
        major <- peek majorPtr
        minor <- peek minorPtr
        return (fromIntegral major, fromIntegral minor)


-- | Sets the major and minor version of OpenGL to request.
--
-- Setting major and minor to zero will use the default values.
--
-- The 'GLContext' must not be realized or made current prior to calling this function.
--
glContextSetRequiredVersion :: GLContextClass self => self -> Int -> Int -> IO ()
glContextSetRequiredVersion self major minor =
    {# call gdk_gl_context_set_required_version #}
        (toGLContext self) (fromIntegral major) (fromIntegral minor)


-- | Retrieves the major and minor version requested by calling 'glContextSetRequiredVersion'.
--
glContextGetRequiredVersion :: GLContextClass self => self -> IO (Int, Int)
glContextGetRequiredVersion self = do
    alloca $ \majorPtr -> alloca $ \minorPtr -> do
        {# call gdk_gl_context_get_required_version #} (toGLContext self) majorPtr minorPtr
        major <- peek majorPtr
        minor <- peek minorPtr
        return (fromIntegral major, fromIntegral minor)


-- | Sets whether the 'GLContext' should perform extra validations and run time checking. This is
--  useful during development, but has additional overhead.
--
-- The 'GLContext' must not be realized or made current prior to calling this function.
--
glContextSetDebugEnabled :: GLContextClass self => self -> Bool -> IO ()
glContextSetDebugEnabled self enabled = do
    {# call gdk_gl_context_set_debug_enabled #} (toGLContext self) (fromBool enabled)


-- | Retrieves the value set using glContextSetDebugEnabled.
--
glContextGetDebugEnabled :: GLContextClass self => self -> IO Bool
glContextGetDebugEnabled self = do
    liftM toBool $ {# call gdk_gl_context_get_debug_enabled #} (toGLContext self)


-- | Sets whether the 'GLContext' should be forward compatible.
--
-- Forward compatibile contexts must not support OpenGL functionality that has been marked as
-- deprecated in the requested version; non-forward compatible contexts, on the other hand, must
-- support both deprecated and non deprecated functionality.
--
-- The 'GLContext' must not be realized or made current prior to calling this function.
--
glContextSetForwardCompatible :: GLContextClass self => self -> Bool -> IO ()
glContextSetForwardCompatible self compatible = do
    {# call gdk_gl_context_set_forward_compatible #} (toGLContext self) (fromBool compatible)


-- | Retrieves the value set using glContextSetForwardCompatible.
--
glContextGetForwardCompatible :: GLContextClass self => self -> IO Bool
glContextGetForwardCompatible self = do
    liftM toBool $ {# call gdk_gl_context_get_forward_compatible #} (toGLContext self)


#if GTK_CHECK_VERSION(3,20,0)
-- | Whether the 'GLContext' is in legacy mode or not.
--
-- The 'GLContext' must be realized before calling this function.
--
-- When realizing a GL context, GDK will try to use the OpenGL 3.2 core profile; this profile
-- removes all the OpenGL API that was deprecated prior to the 3.2 version of the specification.
-- If the realization is successful, this function will return False.
--
-- If the underlying OpenGL implementation does not support core profiles, GDK will fall back to
-- a pre-3.2 compatibility profile, and this function will return True.
--
-- You can use the value returned by this function to decide which kind of OpenGL API to use, or
-- whether to do extension discovery, or what kind of shader programs to load.
--
glContextIsLegacy :: GLContextClass self => self -> IO Bool
glContextIsLegacy self = do
    liftM toBool $ {# call gdk_gl_context_is_legacy #} (toGLContext self)
#endif


-- | Realizes the given 'GLContext'.
--
-- It is safe to call this function on a realized 'GLContext'.
--
glContextRealize :: GLContextClass self => self -> IO Bool
glContextRealize self =
    liftM toBool $
        propagateGError $ \errPtr ->
            {# call gdk_gl_context_realize #} (toGLContext self) errPtr


-- | Makes the context the current one.
--
glContextMakeCurrent :: GLContextClass self => self -> IO ()
glContextMakeCurrent self = do
    {# call gdk_gl_context_make_current #} (toGLContext self)


-- | Retrieves the current 'GLContext'.
--
glContextGetCurrent :: IO (Maybe GLContext)
glContextGetCurrent = do
    maybeNull (wrapNewGObject mkGLContext) $
        {# call gdk_gl_context_get_current #}


-- | Clears the current 'GLContext'.
--
-- Any OpenGL call after this function returns will be ignored until glContextMakeCurrent
-- is called.
--
glContextClearCurrent :: IO ()
glContextClearCurrent = do
    {# call gdk_gl_context_clear_current #}
