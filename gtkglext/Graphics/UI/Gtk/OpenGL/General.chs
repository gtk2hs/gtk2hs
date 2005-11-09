-- -*-haskell-*-
--  GIMP Toolkit (GTK) OpenGL Extension: General
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
-- 
--
module Graphics.UI.Gtk.OpenGL.General (

-- * Methods
  initGL,
-- ** Query
  glQueryExtension,
  glQueryExtensionForDisplay,
  glQueryGLExtension,
  glQueryVersion,
  glQueryVersionForDisplay,
-- ** Fonts
  glFontUsePangoFont,
  glFontUsePangoFontForDisplay,
  ) where

import Monad	(liftM)
import System	(getProgName, getArgs)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GObject			(makeNewGObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Pango.Types#}
{#import Graphics.UI.Gtk.OpenGL.Types#}

{# context lib="gtkglext" prefix="gtk" #}

--------------------
-- Methods

initGL :: IO [String]
initGL = do
  prog <- getProgName
  args <- getArgs
  let allArgs = (prog:args)
  withMany withUTFString allArgs $ \addrs  ->
    withArrayLen       addrs   $ \argc argv ->
    with               argv    $ \argvp ->
    with               argc    $ \argcp -> do
      res <- {#call gtk_gl_init_check#} (castPtr argcp) (castPtr argvp)
      if (toBool res) then do
        argc'   <- peek argcp
        argv'   <- peek argvp
        _:addrs'  <- peekArray argc' argv'  -- drop the program name
        mapM peekUTFString addrs'
        else error "Cannot initialize OpenGL."

-- | 
--
glQueryExtension :: IO Bool
glQueryExtension =
  liftM toBool $
  {# call gdk_gl_query_extension #}

-- | 
--
glQueryExtensionForDisplay :: Display -> IO Bool
glQueryExtensionForDisplay display =
  liftM toBool $
  {# call gdk_gl_query_extension_for_display #}
    display

-- | 
--
glQueryGLExtension :: String -> IO Bool
glQueryGLExtension extension =
  liftM toBool $
  withUTFString extension $ \extensionPtr ->
  {# call gdk_gl_query_gl_extension #}
    extensionPtr

-- | 
--
glQueryVersion :: IO (Maybe (Int, Int))
glQueryVersion =
  alloca $ \majorPtr ->
  alloca $ \minorPtr ->
  {# call gdk_gl_query_version #}
    majorPtr
    minorPtr
  >>= \ok ->
  if toBool ok
    then peek majorPtr >>= \major ->
         peek minorPtr >>= \minor ->
         return (Just (fromIntegral major, fromIntegral minor))
    else return Nothing

-- | 
--
glQueryVersionForDisplay :: Display -> IO (Maybe (Int, Int))
glQueryVersionForDisplay display =
  alloca $ \majorPtr ->
  alloca $ \minorPtr ->
  {# call gdk_gl_query_version_for_display #}
    display
    majorPtr
    minorPtr
  >>= \ok ->
  if toBool ok
    then peek majorPtr >>= \major ->
         peek minorPtr >>= \minor ->
         return (Just (fromIntegral major, fromIntegral minor))
    else return Nothing

-- | 
--
glFontUsePangoFont :: FontDescription -> Int -> Int -> Int -> IO Font
glFontUsePangoFont fontDesc first count listBase =
  makeNewGObject mkFont $
  {# call gdk_gl_font_use_pango_font #}
    fontDesc
    (fromIntegral first)
    (fromIntegral count)
    (fromIntegral listBase)

-- | 
--
glFontUsePangoFontForDisplay :: Display -> FontDescription -> Int -> Int -> Int -> IO Font
glFontUsePangoFontForDisplay display fontDesc first count listBase =
  makeNewGObject mkFont $
  {# call gdk_gl_font_use_pango_font_for_display #}
    display
    fontDesc
    (fromIntegral first)
    (fromIntegral count)
    (fromIntegral listBase)
