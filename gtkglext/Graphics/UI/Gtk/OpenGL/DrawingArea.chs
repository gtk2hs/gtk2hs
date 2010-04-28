{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) OpenGL Extension: DrawingArea Widget
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
-- 
--
module Graphics.UI.Gtk.OpenGL.DrawingArea (

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'DrawingArea'
-- |                     +----GLDrawingArea
-- @

-- * Types
  GLDrawingArea,

-- * Constructors
  glDrawingAreaNew,

-- * Methods
  withGLDrawingArea,
  glDrawingAreaGetGLConfig,
  glDrawingAreaGetGLContext,
  glDrawingAreaGetGLWindow,
  ) where

import Control.Monad	(liftM)

import System.Glib.FFI
import System.Glib.GObject			(makeNewGObject)
{#import Graphics.UI.Gtk.OpenGL.Types#}
import Graphics.UI.Gtk.Misc.DrawingArea		(drawingAreaNew)
import Graphics.UI.Gtk.OpenGL.Drawable		(glDrawableGLBegin, glDrawableWaitGL, glDrawableGLEnd)
import Graphics.UI.Gtk.OpenGL.Window		()
import Graphics.UI.Gtk.OpenGL.Context		(GLRenderType(..))

{# context lib="gtkglext" prefix="gtk" #}


--------------------
-- Types

newtype GLDrawingArea = GLDrawingArea DrawingArea

instance DrawingAreaClass GLDrawingArea
instance WidgetClass GLDrawingArea
instance ObjectClass GLDrawingArea
instance GObjectClass GLDrawingArea where
  toGObject (GLDrawingArea gd) = toGObject gd
  unsafeCastGObject = GLDrawingArea . unsafeCastGObject

--------------------
-- Constructors

glDrawingAreaNew :: GLConfig -> IO GLDrawingArea
glDrawingAreaNew glconfig = do
  drawingArea <- drawingAreaNew
  widgetSetGLCapability drawingArea glconfig Nothing True RGBAType
  return (GLDrawingArea drawingArea)


--------------------
-- Methods

withGLDrawingArea :: GLDrawingArea -> (GLWindow -> IO a) -> IO a
withGLDrawingArea glDrawingArea glAction = do
  glcontext <- glDrawingAreaGetGLContext glDrawingArea
  glwindow <- glDrawingAreaGetGLWindow glDrawingArea
  glDrawableGLBegin glwindow glcontext
  result <- glAction glwindow
  glDrawableWaitGL glwindow
  glDrawableGLEnd glwindow
  return result

-- | 
--
glDrawingAreaGetGLConfig :: GLDrawingArea -> IO GLConfig
glDrawingAreaGetGLConfig (GLDrawingArea widget) =
  makeNewGObject mkGLConfig $
  {# call gtk_widget_get_gl_config #}
    (toWidget widget)

-- | 
--
glDrawingAreaGetGLContext :: GLDrawingArea -> IO GLContext
glDrawingAreaGetGLContext (GLDrawingArea widget) =
  makeNewGObject mkGLContext $
  {# call gtk_widget_get_gl_context #}
    (toWidget widget)

-- | 
--
glDrawingAreaGetGLWindow :: GLDrawingArea -> IO GLWindow
glDrawingAreaGetGLWindow (GLDrawingArea widget) =
  makeNewGObject mkGLWindow $
  {# call gtk_widget_get_gl_window #}
    (toWidget widget)

-- | 
--
widgetSetGLCapability 
 :: WidgetClass widget
 => widget
 -> GLConfig
 -> Maybe GLContext
 -> Bool
 -> GLRenderType
 -> IO Bool
widgetSetGLCapability widget glconfig shareList direct renderType =
  liftM toBool $
  {# call gtk_widget_set_gl_capability #}
    (toWidget widget)
    (toGLConfig glconfig)
    (maybe (GLContext nullForeignPtr) toGLContext shareList)
    (fromBool direct)
    ((fromIntegral . fromEnum) renderType)
