{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Styles
--
--  Author : Axel Simon
--
--  Created: 13 February 2003
--
--  Copyright (C) 1999-2005 Axel Simon
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
-- Rendering UI elements
--
module Graphics.UI.Gtk.General.StyleContext (
-- * Description
--
-- | GtkStyleContext is an object that stores styling information
-- affecting a widget defined by @WidgetPath@.
--
-- In order to construct the final style information, @StyleContext@
-- queries information from all attached @StyleProviders@. Style
-- providers can be either attached explicitly to the context through
-- @styleContextAddProvider@, or to the screen through @styleContextAddProviderForScreen.
-- The resulting style is a combination of all providers' information
-- in priority order.
--
-- For GTK+ widgets, any @StyleContext@ returned by widgetGetStyleContext
-- will already have a @WidgetPath@, a @Screen@ and RTL/LTR information set.
-- The style context will be also updated automatically if any of these
-- settings change on the widget.
--
-- If you are using the theming layer standalone, you will need to set a
-- widget path and a screen yourself to the created style context through
-- @styleContextSetPath@ and @styleContextSetScreen@, as well as updating
-- the context yourself using @styleContextInvalidate@ whenever any of the
-- conditions change, such as a change in the "gtk-theme-name" setting or
-- a hierarchy change in the rendered widget.
--

#if GTK_MAJOR_VERSION >= 3
-- * Types
  StyleContext,
  StyleContextClass,
  castToStyleContext, gTypeStyleContext,
  toStyleContext,

-- * Constructors
  styleContextNew,

-- * Methods
  styleContextAddProvider,
  styleContextAddProviderForScreen,
#endif

  ) where

{# context prefix ="gtk" #}

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.UTFString
{#import Graphics.UI.Gtk.Types#}
import System.Glib.GError (GError(..), GErrorClass(..), GErrorDomain,
                           propagateGError)

#if GTK_MAJOR_VERSION >= 3
-- | Creates a standalone @StyleContext@, this style context won't be attached
-- to any widget, so you may want to call @styleContextSetPath@ yourself.
--
-- Note : This function is only useful when using the theming layer separated
-- from GTK+, if you are using @StyleContext@ to theme @Widgets@, use
-- @widgetGetStyleContext@ in order to get a style context ready to theme
-- the widget.
styleContextNew :: IO StyleContext -- ^ A newly created StyleContext
styleContextNew =
  wrapNewGObject mkStyleContext {# call gtk_style_context_new #}

-- | Adds a style provider to @context@, to be used in style construction.
-- Note that a style provider added by this function only affects the
-- style of the widget to which @context@ belongs. If you want to affect
-- the style of all widgets, use @styleContextAddProviderForScreen@.
--
-- Note : If both priorities are the same, A @StyleProvider@ added through
-- this function takes precedence over another added through
-- @styleContextAddProviderForScreen.
styleContextAddProvider :: (StyleContextClass context, StyleProviderClass provider)
                        => context  -- ^ @context@ a @StyleContext@
                        -> provider -- ^ @provider@ a @StyleProvider@
                        -> Int      -- ^ @priority@ : the priority of the style provider.
                                    -- The lower it is, the earlier it will be used in the
                                    -- style construction. Typically this will be in the
                                    -- range between GTK_STYLE_PROVIDER_PRIORITY_FALLBACK
                                    -- and GTK_STYLE_PROVIDER_PRIORITY_USER
                        -> IO ()
styleContextAddProvider context provider priority =
  {# call gtk_style_context_add_provider #}
  (toStyleContext context)
  (toStyleProvider provider)
  (fromIntegral priority)

-- | Adds a global style provider to @screen@, which will be used in style
-- construction for all @StyleContexts@ under @screen@.
--
-- GTK+ uses this to make styling information from @Settings@ available.
--
-- Note : If both priorities are the same, A @StyleProvider@ added through
-- @styleContextAddProvider@ takes precedence over another added through
-- this function.
styleContextAddProviderForScreen :: (ScreenClass screen, StyleProviderClass provider)
                        => screen   -- ^ @screen@ a @Screen@
                        -> provider -- ^ @provider@ a @StyleProvider@
                        -> Int      -- ^ @priority@ : the priority of the style provider.
                                    -- The lower it is, the earlier it will be used in the
                                    -- style construction. Typically this will be in the
                                    -- range between GTK_STYLE_PROVIDER_PRIORITY_FALLBACK
                                    -- and GTK_STYLE_PROVIDER_PRIORITY_USER
                        -> IO ()
styleContextAddProviderForScreen screen provider priority =
  {# call gtk_style_context_add_provider_for_screen #}
  (toScreen screen)
  (toStyleProvider provider)
  (fromIntegral priority)

#endif
