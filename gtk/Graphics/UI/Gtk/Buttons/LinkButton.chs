{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget LinkButton
--
--  Author : Andy Stewart
--
--  Created: 22 Mar 2010
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
-- Create buttons bound to a URL
--
-- * Module available since Gtk+ version 2.10
--
module Graphics.UI.Gtk.Buttons.LinkButton (

-- * Detail
--
-- | A 'LinkButton' is a 'Button' with a hyperlink, similar to the one used by
-- web browsers, which triggers an action when clicked. It is useful to show
-- quick links to resources.
--
-- A link button is created by calling either 'linkButtonNew' or
-- 'linkButtonNewWithLabel'. If using the former, the URI you pass to the
-- constructor is used as a label for the widget.
--
-- The URI bound to a 'LinkButton' can be set specifically using
-- \"set linkButton [linkButtonURI := uri]\", and retrieved using \"uri <- get linkButton linkButtonURI\".
--
-- 'LinkButton' offers a global hook, which is called when the used clicks
-- on it: see 'linkButtonSetURIHook'.
--
-- 'LinkButton' was added in Gtk+ 2.10.

-- * Class Hierarchy
--
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Bin'
-- |                           +----'Button'
-- |                                 +----LinkButton
-- @

#if GTK_CHECK_VERSION(2,10,0)
-- * Types
  LinkButton,
  LinkButtonClass,
  castToLinkButton,
  toLinkButton,

-- * Constructors
  linkButtonNew,
  linkButtonNewWithLabel,

-- * Methods
#if GTK_MAJOR_VERSION < 3
  linkButtonSetUriHook,
#endif

-- * Attributes
  linkButtonURI,
#if GTK_CHECK_VERSION(2,14,0)
  linkButtonVisited,
#endif
#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}

{# context lib="gtk" prefix="gtk" #}

#if GTK_CHECK_VERSION(2,10,0)

--------------------
-- Constructors

-- | Creates a new 'LinkButton' with the URI as its text.
--
linkButtonNew :: GlibString string
 => string -- ^ @uri@ - a valid URI
 -> IO LinkButton
linkButtonNew uri =
  makeNewObject mkLinkButton $
  liftM (castPtr :: Ptr Widget -> Ptr LinkButton) $
  withUTFString uri $ \uriPtr ->
  {# call gtk_link_button_new #}
    uriPtr

-- | Creates a new 'LinkButton' containing a label.
--
linkButtonNewWithLabel :: GlibString string
 => string -- ^ @uri@ - a valid URI
 -> string -- ^ @label@ - the text of the button
 -> IO LinkButton
linkButtonNewWithLabel uri label =
  makeNewObject mkLinkButton $
  liftM (castPtr :: Ptr Widget -> Ptr LinkButton) $
  withUTFString label $ \labelPtr ->
  withUTFString uri $ \uriPtr ->
  {# call gtk_link_button_new_with_label #}
    uriPtr
    labelPtr

--------------------
-- Methods
#if GTK_MAJOR_VERSION < 3
-- | Sets @func@ as the function that should be invoked every time a user
-- clicks a 'LinkButton'. This function is called before every callback
-- registered for the 'buttonClicked' signal.
--
-- If no uri hook has been set, Gtk+ defaults to calling 'showURI'.
--
-- Removed in Gtk3.
linkButtonSetUriHook :: (String -> IO ()) -> IO ()
linkButtonSetUriHook func = do
  pfPtr <- mkLinkButtonUriFunc $ \_ cstr _  -> do
      str <- peekCString cstr
      func str
  {# call link_button_set_uri_hook #} pfPtr (castFunPtrToPtr pfPtr) destroyFunPtr
  freeHaskellFunPtr pfPtr

{#pointer LinkButtonUriFunc#}

foreign import ccall "wrapper" mkLinkButtonUriFunc ::
  (Ptr LinkButton -> CString -> Ptr () -> IO ())
  -> IO LinkButtonUriFunc
#endif

--------------------
-- Attributes

-- | The URI bound to this button.
--
-- Default value: \"\"
--
-- * Available since Gtk+ version 2.10
--
linkButtonURI :: (LinkButtonClass self, GlibString string) => Attr self string
linkButtonURI = newAttrFromStringProperty "uri"

#if GTK_CHECK_VERSION(2,14,0)
-- | The 'visited' state of this button. A visited link is drawn in a different color.
--
-- Default value: 'False'
--
-- * Available since Gtk+ version 2.14
--
linkButtonVisited :: LinkButtonClass self => Attr self Bool
linkButtonVisited = newAttrFromBoolProperty "visited"
#endif
#endif
