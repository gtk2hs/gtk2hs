{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget GLArea
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
-- A widget for custom drawing with OpenGL
--
module Graphics.UI.Gtk.Misc.GLArea (
-- * Detail
--
-- | The 'GLArea' is a widget that allows drawing with OpenGL.
--
-- GLArea sets up its own 'Graphics.UI.Gtk.Gdk.GLContext.GLContext' for the window it creates, and creates a
-- custom GL framebuffer that the widget will do GL rendering onto. It also
-- ensures that this framebuffer is the default GL rendering target when
-- rendering.
--
-- In order to draw, you have to connect to the 'glAreaRender' signal.
--
-- The GLArea widget ensures that the 'Graphics.UI.Gtk.Gdk.GLContext.GLContext' is associated with the
-- widget's drawing area, and it is kept updated when the size and position
-- of the drawing area changes.
--
-- If you need to initialize OpenGL state, e.g. buffer objects or shaders,
-- you should use the 'Graphics.UI.Gtk.Abstract.Widget.realize' signal; you can use the 'Graphics.UI.Gtk.Abstract.Widget.unrealize' signal
-- to clean up.
--
-- To receive mouse events on a drawing area, you will need to enable them
-- with 'Graphics.UI.Gtk.Abstract.Widget.widgetAddEvents'. To receive keyboard events, you will need to set the
-- 'Graphics.UI.Gtk.Abstract.Widget.widgetCanFocus' attribute on the drawing area.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----GLArea
-- @

-- * Types
  GLArea,
  GLAreaClass,
  castToGLArea, gTypeGLArea,
  toGLArea,

-- * Constructors
  glAreaNew,

-- * Methods
  glAreaGetContext,
  glAreaMakeCurrent,
  glAreaQueueRender,
  glAreaAttachBuffers,
  glAreaSetAutoRender,
  glAreaGetAutoRender,
  glAreaSetHasAlpha,
  glAreaGetHasAlpha,
  glAreaSetHasDepthBuffer,
  glAreaGetHasDepthBuffer,
  glAreaSetHasStencilBuffer,
  glAreaGetHasStencilBuffer,
  glAreaGetRequiredVersion,
  glAreaSetRequiredVersion,
  glAreaGetError,

-- * Attributes
  glAreaAutoRender,
  glAreaContext,
  glAreaHasAlpha,
  glAreaHasDepthBuffer,
  glAreaHasStencilBuffer,

-- * Signals
  glAreaResize,
  glAreaRender

  ) where

import Control.Monad    (liftM)
import Control.Monad.Trans              ( liftIO )
import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
import System.Glib.GError
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
import Graphics.UI.Gtk.Gdk.GLContext
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Creates a new GLArea widget.
--
glAreaNew :: IO GLArea
glAreaNew =
  makeNewObject mkGLArea $
  liftM (castPtr :: Ptr Widget -> Ptr GLArea) $
  {# call unsafe gl_area_new #}


--------------------
-- Methods


-- | Retrieves the 'Graphics.UI.Gtk.Gdk.GLContext.GLContext' used by area.
--
glAreaGetContext :: GLAreaClass self => self
    -> IO (Maybe GLContext)
glAreaGetContext self = do
    maybeNull (wrapNewGObject mkGLContext) $
        {# call gtk_gl_area_get_context #} (toGLArea self)


-- | Ensures that the 'Graphics.UI.Gtk.Gdk.GLContext.GLContext' used by area is associated with the GLArea.
--
-- This function is automatically called before emitting the 'glAreaRender' signal,
-- and doesn't normally need to be called by application code.
--
glAreaMakeCurrent :: GLAreaClass self => self -> IO ()
glAreaMakeCurrent self =
    {# call gtk_gl_area_make_current #} (toGLArea self)


-- | Marks the currently rendered data (if any) as invalid, and queues a redraw
-- of the widget, ensuring that the 'glAreaRender' signal is emitted during the draw.
--
-- This is only needed when 'glAreaSetAutoRender' has been called with a False
-- value. The default behaviour is to emit 'glAreaRender' on each draw.
--
glAreaQueueRender :: GLAreaClass self => self -> IO ()
glAreaQueueRender self =
    {# call gtk_gl_area_queue_render #} (toGLArea self)


-- | Ensures that the area framebuffer object is made the current draw and read
-- target, and that all the required buffers for the area are created and
-- bound to the frambuffer.
--
-- This function is automatically called before emitting the 'glAreaRender' signal,
-- and doesn't normally need to be called by application code.
--
glAreaAttachBuffers :: GLAreaClass self => self -> IO ()
glAreaAttachBuffers self =
    {# call gtk_gl_area_attach_buffers #} (toGLArea self)


-- | Gets the current error set on the area.
--
glAreaGetError :: GLAreaClass self => self -> IO (Maybe GError)
glAreaGetError self = do
    err <- {# call gtk_gl_area_get_error #} (toGLArea self)
    if err == nullPtr
        then return Nothing
        else liftM Just (peek $ castPtr err)


-- | If hasAlpha is True the buffer allocated by the widget will have an alpha channel component,
-- and when rendering to the window the result will be composited over whatever is below the
-- widget.
--
-- If hasAlpha is False there will be no alpha channel, and the buffer will fully replace anything
-- below the widget.
--
glAreaSetHasAlpha :: GLAreaClass self => self -> Bool -> IO ()
glAreaSetHasAlpha self hasAlpha =
  {# call gl_area_set_has_alpha #}
    (toGLArea self)
    (fromBool hasAlpha)


-- | Returns whether the area has an alpha component.
--
glAreaGetHasAlpha :: GLAreaClass self => self -> IO Bool
glAreaGetHasAlpha self =
  liftM toBool $
    {# call unsafe gl_area_get_has_alpha #} (toGLArea self)


-- | If hasDepthBuffer is True the widget will allocate and enable a depth buffer for the target
-- framebuffer. Otherwise there will be none.
--
glAreaSetHasDepthBuffer :: GLAreaClass self => self -> Bool -> IO ()
glAreaSetHasDepthBuffer self hasDepthBuffer =
  {# call gl_area_set_has_depth_buffer #}
    (toGLArea self)
    (fromBool hasDepthBuffer)


-- | Returns whether the area has a depth buffer.
--
glAreaGetHasDepthBuffer :: GLAreaClass self => self -> IO Bool
glAreaGetHasDepthBuffer self =
  liftM toBool $
    {# call unsafe gl_area_get_has_depth_buffer #} (toGLArea self)


-- | If hasStencilBuffer is True the widget will allocate and enable a stencil buffer for the target
-- framebuffer. Otherwise there will be none.
--
glAreaSetHasStencilBuffer :: GLAreaClass self => self -> Bool -> IO ()
glAreaSetHasStencilBuffer self hasStencilBuffer =
  {# call gl_area_set_has_stencil_buffer #}
    (toGLArea self)
    (fromBool hasStencilBuffer)


-- | Returns whether the area has a stencil buffer.
--
glAreaGetHasStencilBuffer :: GLAreaClass self => self -> IO Bool
glAreaGetHasStencilBuffer self =
  liftM toBool $
    {# call unsafe gl_area_get_has_stencil_buffer #} (toGLArea self)


-- | If autoRender is True the 'glAreaRender' signal will be emitted every time the widget draws. This is
-- the default and is useful if drawing the widget is faster.
--
-- If autoRender is False the data from previous rendering is kept around and will be used for
-- drawing the widget the next time, unless the window is resized. In order to force a rendering
-- 'glAreaQueueRender' must be called. This mode is useful when the scene changes seldomly, but takes
-- a long time to redraw.
--
glAreaSetAutoRender :: GLAreaClass self => self -> Bool -> IO ()
glAreaSetAutoRender self autoRender =
  {# call gl_area_set_auto_render #}
    (toGLArea self)
    (fromBool autoRender)

-- | Returns whether the area is in auto render mode or not.
--
glAreaGetAutoRender :: GLAreaClass self => self -> IO Bool
glAreaGetAutoRender self =
  liftM toBool $
  {# call unsafe gl_area_get_auto_render #}
    (toGLArea self)


-- | Retrieves the required version of OpenGL set using 'glAreaSetRequiredVersion'.
--
glAreaGetRequiredVersion :: GLAreaClass self => self -> IO (Int, Int)
glAreaGetRequiredVersion self = do
    alloca $ \majorPtr -> alloca $ \minorPtr -> do
        {# call gtk_gl_area_get_required_version #} (toGLArea self) majorPtr minorPtr
        major <- peek majorPtr
        minor <- peek minorPtr
        return (fromIntegral major, fromIntegral minor)


-- | Sets the required version of OpenGL to be used when creating the context for the widget.
--
-- This function must be called before the area has been realized.
--
glAreaSetRequiredVersion :: GLAreaClass self => self -> Int -> Int -> IO ()
glAreaSetRequiredVersion self major minor =
    {# call gtk_gl_area_set_required_version #}
        (toGLArea self) (fromIntegral major) (fromIntegral minor)


--------------------
-- Attributes

-- | If set to True the 'glAreaRender' signal will be emitted every time the widget draws. This is the
-- default and is useful if drawing the widget is faster.
--
-- If set to False the data from previous rendering is kept around and will be used for drawing the
-- widget the next time, unless the window is resized. In order to force a rendering
-- 'glAreaQueueRender' must be called. This mode is useful when the scene changes seldomly, but
-- takes a long time to redraw.
--
-- Default value: True
--
glAreaAutoRender :: GLAreaClass self => Attr self Bool
glAreaAutoRender = newAttr
  glAreaGetAutoRender
  glAreaSetAutoRender


-- | The 'Graphics.UI.Gtk.Gdk.GLContext.GLContext' used by the GLArea widget.
--
-- The GLArea widget is responsible for creating the 'Graphics.UI.Gtk.Gdk.GLContext.GLContext' instance. If you need to render with
-- other kinds of buffers (stencil, depth, etc), use render buffers.
--
glAreaContext :: GLAreaClass self => ReadAttr self (Maybe GLContext)
glAreaContext = readAttr glAreaGetContext


-- | If set to True the buffer allocated by the widget will have an alpha channel component, and
-- when rendering to the window the result will be composited over whatever is below the widget.
--
-- If set to False there will be no alpha channel, and the buffer will fully replace anything below
-- the widget.
--
-- Default value: False
--
glAreaHasAlpha :: GLAreaClass self => Attr self Bool
glAreaHasAlpha = newAttr
  glAreaGetHasAlpha
  glAreaSetHasAlpha


-- | If set to True the widget will allocate and enable a depth buffer for the target framebuffer.
--
-- Default value: False
--
glAreaHasDepthBuffer :: GLAreaClass self => Attr self Bool
glAreaHasDepthBuffer = newAttr
  glAreaGetHasDepthBuffer
  glAreaSetHasDepthBuffer


-- | If set to True the widget will allocate and enable a stencil buffer for the target framebuffer.
--
-- Default value: False
--
glAreaHasStencilBuffer :: GLAreaClass self => Attr self Bool
glAreaHasStencilBuffer = newAttr
  glAreaGetHasStencilBuffer
  glAreaSetHasStencilBuffer


--------------------
-- Signals


-- | The glAreaResize signal is emitted once when the widget is realized, and then each time the widget
-- is changed while realized. This is useful in order to keep GL state up to date with the widget
-- size, like for instance camera properties which may depend on the width/height ratio.
--
-- The GL context for the area is guaranteed to be current when this signal is emitted.
--
-- The default handler sets up the GL viewport.
--
glAreaResize :: GLAreaClass glac => Signal glac (Int -> Int -> IO ())
glAreaResize = Signal (connect_INT_INT__NONE "resize")


-- | The glAreaRender signal is emitted every time the contents of the GLArea should be redrawn.
--
-- The context is bound to the area prior to emitting this function, and the buffers are painted
-- to the window once the emission terminates.
--
glAreaRender :: GLAreaClass glac => Signal glac (Maybe GLContext -> IO (Bool))
glAreaRender = Signal (connect_BOXED__BOOL "render" unwrapGLContextPtr)


unwrapGLContextPtr :: Ptr GLContext -> IO (Maybe GLContext)
unwrapGLContextPtr ptr = do
    maybeNull (makeNewGObject mkGLContext) (return ptr)
