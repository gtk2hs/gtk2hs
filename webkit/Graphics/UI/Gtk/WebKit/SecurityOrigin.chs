{-# LANGUAGE CPP #-}
-- -*-haskell-*-
-----------------------------------------------------------------------------
--  Module      :  Graphics.UI.Gtk.WebKit.WebSecurityOrigin
--  Author      :  Andy Stewart
--  Copyright   :  (c) 2010 Andy Stewart <lazycat.manatee@gmail.com>
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
-- Access to the WebKit Web SecurityOrigin
-----------------------------------------------------------------------------

module Graphics.UI.Gtk.WebKit.SecurityOrigin (
-- * Types
  SecurityOrigin,

-- * Methods  
  securityOriginGetAllWebDatabases,
  securityOriginGetHost,
  securityOriginGetPort,
  securityOriginGetProtocol,
  securityOriginGetWebDatabaseQuota,
  securityOriginSetWebDatabaseQuota,
  securityOriginGetWebDatabaseUsage,
) where

import Control.Monad		(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GList
import System.Glib.GError 
import Graphics.UI.Gtk.Gdk.Events

{#import Graphics.UI.Gtk.Abstract.Object#}	(makeNewObject)
{#import Graphics.UI.Gtk.WebKit.Types#}
{#import System.Glib.GObject#}

{#context lib="webkit" prefix ="webkit"#}

-- * Methods.

-- | Returns the frame's security origin.
securityOriginGetAllWebDatabases ::
   SecurityOriginClass self => self
 -> IO [WebDatabase]   
securityOriginGetAllWebDatabases so = do
  glist <- {#call security_origin_get_all_web_databases#} (toSecurityOrigin so)
  databasePtr <- fromGList glist
  mapM (makeNewGObject mkWebDatabase . return) databasePtr

-- | Returns the hostname for the security origin.
securityOriginGetHost ::
   SecurityOriginClass self => self
 -> IO String
securityOriginGetHost so =
  {#call security_origin_get_host#} (toSecurityOrigin so) >>= peekCString

-- | Returns the port for the security origin.
securityOriginGetPort ::
   SecurityOriginClass self => self
 -> IO Int
securityOriginGetPort so =
  liftM fromIntegral $ {#call security_origin_get_port#} (toSecurityOrigin so)

-- | Returns the protocol for the security origin.
securityOriginGetProtocol ::
   SecurityOriginClass self => self
 -> IO String
securityOriginGetProtocol so =
  {#call security_origin_get_protocol#} (toSecurityOrigin so) >>= peekCString

-- | Returns the quota for Web Database storage of the security origin in bytes.
securityOriginGetWebDatabaseQuota ::
   SecurityOriginClass self => self
 -> IO Int
securityOriginGetWebDatabaseQuota so =
    liftM fromIntegral $ {#call security_origin_get_web_database_quota#} (toSecurityOrigin so)
  
-- | Returns the usage for Web Database storage of the security origin in bytes.
securityOriginGetWebDatabaseUsage ::
   SecurityOriginClass self => self
 -> IO Int
securityOriginGetWebDatabaseUsage so =
    liftM fromIntegral $ {#call security_origin_get_web_database_usage#} (toSecurityOrigin so)
  
-- | Adjust the quota for Web Database storage of the security origin
securityOriginSetWebDatabaseQuota ::
   SecurityOriginClass self => self
 -> Int
 -> IO ()
securityOriginSetWebDatabaseQuota so quota =
  {#call security_origin_set_web_database_quota#} (toSecurityOrigin so) (fromIntegral quota)

