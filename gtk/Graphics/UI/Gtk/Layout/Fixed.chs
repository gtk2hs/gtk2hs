-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Fixed
--
--  Author : Duncan Coutts
--
--  Created: 2 August 2004
--
--  Version $Revision: 1.2 $ from $Date: 2005/02/12 17:19:23 $
--
--  Copyright (C) 2004-2005 Duncan Coutts
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
-- A container which allows you to position widgets at fixed coordinates
--
module Graphics.UI.Gtk.Layout.Fixed (
-- * Description
-- 
-- | The "Fixed" widget is a container which can place child widgets at fixed
-- positions and with fixed sizes, given in pixels. "Fixed" performs no
-- automatic layout management.
--
-- For most applications, you should not use this container! It keeps you
-- from having to learn about the other GTK+ containers, but it results in
-- broken applications.
--
-- * Themes, which may change widget sizes.
--
-- * Fonts other than the one you used to write the app will of course
-- change the size of widgets containing text; keep in mind that users may use
-- a larger font because of difficulty reading the default, or they may be
-- using Windows or the framebuffer port of GTK+, where different fonts are
-- available.
--
-- * Translation of text into other languages changes its size. Also,
-- display of non-English text will use a different font in many cases.
--
-- In addition, the fixed widget can\'t properly be mirrored in
-- right-to-left languages such as Hebrew and Arabic. i.e. normally GTK+ will
-- flip the interface to put labels to the right of the thing they label, but
-- it can\'t do that with "Fixed". So your application will not be usable in
-- right-to-left languages.
--
-- Finally, fixed positioning makes it kind of annoying to add\/remove GUI
-- elements, since you have to reposition all the other elements. This is a
-- long-term maintenance problem for your application.
--
-- If you know none of these things are an issue for your application, and
-- prefer the simplicity of "Fixed", by all means use the widget. But you
-- should be aware of the tradeoffs.

  fixedNew,
  fixedPut,
  fixedMove,
  fixedSetHasWindow,
  fixedGetHasWindow
  ) where

import Monad	(liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}


-- | Creates a new 'Fixed' container.
--
fixedNew :: IO Fixed
fixedNew = makeNewObject mkFixed $ liftM castPtr $
  {#call unsafe fixed_new#}

-- | Adds a widget to a 'Fixed' container at the given position.
--
fixedPut :: (FixedClass obj, WidgetClass widget) => obj
         -> widget -> (Int, Int) -> IO ()
fixedPut obj widget (x, y) =
  {#call fixed_put#} (toFixed obj) (toWidget widget)
    (fromIntegral x) (fromIntegral y)

-- | Moves a child of a 'Fixed' container to the given position.
--
fixedMove :: (FixedClass obj, WidgetClass widget) => obj
         -> widget -> (Int, Int) -> IO ()
fixedMove obj widget (x, y) =
  {#call fixed_move#} (toFixed obj) (toWidget widget)
    (fromIntegral x) (fromIntegral y)

-- | Sets whether the 'Fixed' widget is created with a separate "DrawWindow" for
-- its window or not. (By default, it will be created with no separate
-- "DrawWindow"). This function must be called while the widget is not realized,
-- for instance, immediately after the window is created.
--
fixedSetHasWindow :: FixedClass obj => obj -> Bool -> IO ()
fixedSetHasWindow obj hasWindow =
  {#call fixed_set_has_window#} (toFixed obj) (fromBool hasWindow)

-- | Gets whether the 'Fixed' container has its own "DrawWindow". See
-- 'fixedSetHasWindow'.
--
fixedGetHasWindow :: FixedClass obj => obj -> IO Bool
fixedGetHasWindow obj = liftM toBool $
  {#call unsafe fixed_get_has_window#} (toFixed obj)
