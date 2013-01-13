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
-- CSS-like styling for widgets.
--
module Graphics.UI.Gtk.General.CssProvider (
-- * Description
--
-- | @CssProvider@ is an object implementing the @StyleProvider@ interface.
-- It is able to parse CSS-like input in order to style widgets.
--

#if GTK_MAJOR_VERSION >= 3
-- * Types
  CssProvider,
  CssProviderClass,
  castToCssProvider, gTypeCssProvider,
  toCssProvider,

-- * Enums
  CssProviderError(..),

-- * Constructors
  cssProviderNew,

-- * Methods
  cssProviderGetDefault,
  cssProviderGetNamed,
  cssProviderLoadFromData,
  cssProviderLoadFromString,
  cssProviderLoadFromPath,
#if GTK_CHECK_VERSION(3,2,0)
  cssProviderToString,
#endif
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
{#enum CssProviderError {underscoreToCase} deriving (Bounded,Eq,Show)#}

--------------------
-- Interfaces

instance StyleProviderClass CssProvider

--------------------
-- Constructors

-- | Returns a newly created CssProvider
cssProviderNew :: IO CssProvider -- ^ A new CssProvider
cssProviderNew =
  wrapNewGObject mkCssProvider {# call gtk_css_provider_new #}

--------------------
-- Methods

-- | Returns the provider containing the style settings used as a fallback for all widgets.
cssProviderGetDefault :: IO CssProvider -- ^ The provider used for fallback styling
cssProviderGetDefault =
  makeNewGObject mkCssProvider {# call gtk_css_provider_get_default #}

-- | Loads a theme from the usual theme paths
cssProviderGetNamed :: String                 -- ^ @name@ : A theme name
                    -> Maybe String           -- ^ @variant@ to load, for example, "dark"
                    -> IO (Maybe CssProvider) -- ^ a @CssProvider@ with the theme loaded
cssProviderGetNamed name variant =
  maybeNull (makeNewGObject mkCssProvider) $
  withUTFString name $ \namePtr ->
  maybeWith withUTFString variant $ \variantPtr ->
  {# call gtk_css_provider_get_named #} namePtr variantPtr

-- | Loads @_data@ into @cssProvider@, making it clear any previously loaded information.
cssProviderLoadFromData :: CssProviderClass cssProvider
                      => cssProvider -- | ^ @cssProvider@ : a @CssProvider@
                      -> Ptr CChar   -- | ^ @_data@ : CSS data loaded in memory. [array length=length][element-type guint8]
                      -> Int         -- | ^ @length@ : the length of @_data@ in bytes, or -1 for NUL terminated
                                     -- strings. If @length@ is not -1, the code will assume it is
                                     -- not NUL terminated and will potentially do a copy.
                      -> IO ()
cssProviderLoadFromData cssProvider _data length =
  propagateGError $ \errPtrPtr ->
  {# call gtk_css_provider_load_from_data #}
  (toCssProvider cssProvider)
  _data
  (fromIntegral length)
  errPtrPtr >> return ()

-- | Loads @css@ into @cssProvider@, making it clear any previously loaded information.
cssProviderLoadFromString :: CssProviderClass cssProvider
                          => cssProvider -- | ^ @cssProvider@ : a @CssProvider@
                          -> String      -- | ^ @css@ : CSS data loaded in memory.
                          -> IO ()
cssProviderLoadFromString cssProvider css =
  withUTFStringLen css $ \(cssPtr, len) ->
  cssProviderLoadFromData cssProvider cssPtr len

-- |Loads the data contained in @path@ into @cssProvider@, making it clear any previously
-- loaded information
cssProviderLoadFromPath :: CssProviderClass cssProvider
                        => cssProvider -- | ^ @cssProvider@ a @CssProvider@
                        -> FilePath    -- | ^ the path of a filename to load
                        -> IO ()
cssProviderLoadFromPath cssProvider path =
  propagateGError $ \errPtrPtr ->
  withUTFString path $ \pathPtr ->
  {# call gtk_css_provider_load_from_path #}
  (toCssProvider cssProvider)
  pathPtr
  errPtrPtr >> return ()

#if GTK_CHECK_VERSION(3,2,0)
-- | Convertes the @provider@ into a string representation in CSS format.
--
-- Using @cssProviderLoadFromString@ with the return value from this function
-- on a new provider created with @cssProviderNew@ will basically create a
-- duplicate of this @provider@.
cssProviderToString :: CssProviderClass cssProvider
                    => cssProvider -- | ^ @provider@ a @CssProvider@
                    -> IO String
cssProviderToString provider =
  {# call gtk_css_provider_to_string #}
  (toCssProvider provider) >>= peekUTFString
#endif

#endif
