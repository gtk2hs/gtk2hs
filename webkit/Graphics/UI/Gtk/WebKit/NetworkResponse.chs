{-# LANGUAGE CPP #-}
-- -*-haskell-*-
-----------------------------------------------------------------------------
--  Module      :  Graphics.UI.Gtk.WebKit.NetworkResponse
--  Author      :  Cjacker Huang
--  Copyright   :  (c) 2009 Cjacker Huang <jzhuang@redflag-linux.com>
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
-- The response given to a network request
-----------------------------------------------------------------------------

module Graphics.UI.Gtk.WebKit.NetworkResponse (
-- * Types
  NetworkResponse,

-- * Methods
  networkResponseSetUri,
  networkResponseGetUri,
) where

import Control.Monad		(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GList
import System.Glib.GError 
import Graphics.UI.Gtk.Gdk.Events

{#import Graphics.UI.Gtk.Abstract.Object#}	(makeNewObject)
{#import Graphics.UI.Gtk.Signals#}
{#import Graphics.UI.Gtk.WebKit.Types#}
{#import System.Glib.GObject#}

{#context lib="webkit" prefix ="webkit"#}

------------------
-- Constructors

-- | Set the URI of 'NetworkResponse'.
--
networkResponseSetUri :: 
    NetworkResponseClass self => self 
 -> String  -- ^ @uri@ - the uri will be set to the response.
 -> IO()
networkResponseSetUri response uri =
    withCString uri $ \uriPtr -> 
      {#call network_response_set_uri#} 
        (toNetworkResponse response)
        uriPtr


-- | Return the uri of 'NetworkResponse'.
networkResponseGetUri :: 
    NetworkResponseClass self => self 
 -> IO (Maybe String) -- ^ the URI or @Nothing@ in case failed.
networkResponseGetUri response = 
    {#call network_response_get_uri#} 
      (toNetworkResponse response) >>= 
      maybePeek peekCString

