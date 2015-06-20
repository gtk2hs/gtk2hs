{-# LANGUAGE CPP, OverloadedStrings #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget PrintContext
--
--  Author : Andy Stewart
--
--  Created: 28 Mar 2010
--
--  Copyright (C) 2010 Andy Stewart
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
-- Encapsulates context for drawing pages
--
-- * Module available since Gtk+ version 2.10
--
module Graphics.UI.Gtk.Printing.PrintContext (

-- * Detail
--
-- | A 'PrintContext' encapsulates context information that is required when
-- drawing pages for printing, such as the cairo context and important
-- parameters like page size and resolution. It also lets you easily create
-- 'PangoLayout' and 'Context' objects that match the font metrics of the cairo
-- surface.
--
-- 'PrintContext' objects gets passed to the 'beginPrint', 'endPrint',
-- 'requestPageSetup' and 'drawPage' signals on the 'PrintOperation'.
--
-- Printing support was added in Gtk+ 2.10.

-- * Class Hierarchy
--
-- |
-- @
-- |  'GObject'
-- |   +----PrintContext
-- @

#if GTK_CHECK_VERSION(2,10,0)
-- * Types
  PrintContext,
  PrintContextClass,
  castToPrintContext,
  toPrintContext,

-- * Methods
  printContextGetCairoContext,
  printContextSetCairoContext,
  printContextGetPageSetup,
  printContextGetWidth,
  printContextGetHeight,
  printContextGetDpiX,
  printContextGetDpiY,
  printContextGetPangoFontmap,
  printContextCreatePangoContext,
  printContextCreatePangoLayout,
#if GTK_CHECK_VERSION(2,20,0)
  printContextGetHardMargins,
#endif
#endif
  ) where

import Control.Monad    (liftM)
import Data.IORef (newIORef)

import System.Glib.FFI
import System.Glib.UTFString
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.Rendering.Pango.Types#}
{#import Graphics.Rendering.Pango.BasicTypes#}
{#import Graphics.Rendering.Cairo.Types#}

{# context lib="gtk" prefix="gtk" #}

#if GTK_CHECK_VERSION(2,10,0)
--------------------
-- Methods

-- | Obtains the cairo context that is associated with the 'PrintContext'.
--
printContextGetCairoContext :: PrintContextClass self => self
 -> IO Cairo -- ^ returns the cairo context of @context@
printContextGetCairoContext self =
  liftM Cairo $
  {# call gtk_print_context_get_cairo_context #} (toPrintContext self)

-- | Sets a new cairo context on a print context.
--
-- This function is intended to be used when implementing an internal print
-- preview, it is not needed for printing, since Gtk+ itself creates a suitable
-- cairo context in that case.
--
printContextSetCairoContext :: PrintContextClass self => self
 -> Cairo -- ^ @cr@ - the cairo context
 -> Double       -- ^ @dpiX@ - the horizontal resolution to use with @cr@
 -> Double       -- ^ @dpiY@ - the vertical resolution to use with @cr@
 -> IO ()
printContextSetCairoContext self cr dpiX dpiY =
  {# call gtk_print_context_set_cairo_context #}
    (toPrintContext self)
    cr
    (realToFrac dpiX)
    (realToFrac dpiY)

-- | Obtains the 'PageSetup' that determines the page dimensions of the
-- 'PrintContext'.
--
printContextGetPageSetup :: PrintContextClass self => self
 -> IO PageSetup -- ^ returns the page setup of @context@
printContextGetPageSetup self =
  makeNewGObject mkPageSetup $
  {# call gtk_print_context_get_page_setup #}
    (toPrintContext self)

-- | Obtains the width of the 'PrintContext', in pixels.
--
printContextGetWidth :: PrintContextClass self => self
 -> IO Double -- ^ returns the width of @context@
printContextGetWidth self =
  liftM realToFrac $
  {# call gtk_print_context_get_width #}
    (toPrintContext self)

-- | Obtains the height of the 'PrintContext', in pixels.
--
printContextGetHeight :: PrintContextClass self => self
 -> IO Double -- ^ returns the height of @context@
printContextGetHeight self =
  liftM realToFrac $
  {# call gtk_print_context_get_height #}
    (toPrintContext self)

-- | Obtains the horizontal resolution of the 'PrintContext', in dots per
-- inch.
--
printContextGetDpiX :: PrintContextClass self => self
 -> IO Double -- ^ returns the horizontal resolution of @context@
printContextGetDpiX self =
  liftM realToFrac $
  {# call gtk_print_context_get_dpi_x #}
    (toPrintContext self)

-- | Obtains the vertical resolution of the 'PrintContext', in dots per inch.
--
printContextGetDpiY :: PrintContextClass self => self
 -> IO Double -- ^ returns the vertical resolution of @context@
printContextGetDpiY self =
  liftM realToFrac $
  {# call gtk_print_context_get_dpi_y #}
    (toPrintContext self)

-- | Returns a 'FontMap' that is suitable for use with the 'PrintContext'.
--
printContextGetPangoFontmap :: PrintContextClass self => self
 -> IO FontMap -- ^ returns the font map of @context@
printContextGetPangoFontmap self =
  makeNewGObject mkFontMap $
  {# call gtk_print_context_get_pango_fontmap #}
    (toPrintContext self)

-- | Creates a new 'Context' that can be used with the 'PrintContext'.
--
printContextCreatePangoContext :: PrintContextClass self => self
 -> IO PangoContext -- ^ returns a new Pango context for @context@
printContextCreatePangoContext self =
  wrapNewGObject mkPangoContext $
  {# call gtk_print_context_create_pango_context #}
    (toPrintContext self)

-- | Creates a new 'PangoLayout' that is suitable for use with the
-- 'PrintContext'.
--
printContextCreatePangoLayout :: PrintContextClass self => self
 -> IO PangoLayout -- ^ returns a new Pango layout for @context@
printContextCreatePangoLayout self = do
  pl <- wrapNewGObject mkPangoLayoutRaw $
    {# call gtk_print_context_create_pango_layout #}
    (toPrintContext self)
  ps <- makeNewPangoString (""::DefaultGlibString)
  psRef <- newIORef ps
  return (PangoLayout psRef pl)

#if GTK_CHECK_VERSION(2,20,0)
printContextGetHardMargins :: PrintContextClass self => self
                           -> IO (Maybe (Double, Double, Double, Double))
                             -- ^ returns @(top, bottom, left, right)@
                             -- @top@ top hardware printer margin
                             -- @bottom@ bottom hardware printer margin
                             -- @left@ left hardware printer margin
                             -- @right@ right hardware printer margin
printContextGetHardMargins self =
  alloca $ \ topPtr ->
  alloca $ \ bottomPtr ->
  alloca $ \ leftPtr ->
  alloca $ \ rightPtr -> do
  success <- liftM toBool $ {#call gtk_print_context_get_hard_margins #}
               (toPrintContext self)
               topPtr
               bottomPtr
               leftPtr
               rightPtr
  if success
     then do
       top <- liftM realToFrac $ peek topPtr
       bottom <- liftM realToFrac $ peek bottomPtr
       left <- liftM realToFrac $ peek leftPtr
       right <- liftM realToFrac $ peek rightPtr
       return $ Just (top, bottom, left, right)
     else return Nothing
#endif

#endif
