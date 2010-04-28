{-# LANGUAGE CPP #-}
-- -*-haskell-*-
-----------------------------------------------------------------------------
--  Module      :  Graphics.UI.Gtk.WebKit.WebDatabase
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
-- Access to the WebKit Web Database
-----------------------------------------------------------------------------

module Graphics.UI.Gtk.WebKit.WebDatabase (
-- * Types
  WebDatabase,

-- * Methods  
  webDatabaseGetDisplayName,
  webDatabaseGetExpectedSize,
  webDatabaseGetFilename,
  webDatabaseGetName,
  webDatabaseGetSecurityOrigin,
  webDatabaseGetSize,
  webDatabaseRemove,
) where

import Control.Monad		(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GList
import System.Glib.GError 
import Graphics.UI.Gtk.Gdk.Events

{#import Graphics.UI.Gtk.Abstract.Object#}	(makeNewObject)
{#import Graphics.UI.Gtk.WebKit.Signals#}
{#import Graphics.UI.Gtk.WebKit.Types#}
{#import System.Glib.GObject#}

{#context lib="webkit" prefix ="webkit"#}

-- | Returns the name of the 'WebDatabase' as seen by the user.
webDatabaseGetDisplayName ::
   WebDatabaseClass self => self
 -> IO String
webDatabaseGetDisplayName wd =
  {#call web_database_get_display_name#} (toWebDatabase wd) >>= peekCString
  
-- | Returns the expected size of the Database in bytes as defined by the web author. The Web Database standard allows web authors to
-- specify an expected size of the database to optimize the user experience.
webDatabaseGetExpectedSize ::  
   WebDatabaseClass self => self
 -> IO Int  
webDatabaseGetExpectedSize wd =
    liftM fromIntegral $ {#call web_database_get_expected_size#} (toWebDatabase wd)

-- | Returns the absolute filename to the WebKitWebDatabase file on disk.
webDatabaseGetFilename ::
   WebDatabaseClass self => self
 -> IO String
webDatabaseGetFilename wd = 
  {#call web_database_get_filename#} (toWebDatabase wd) >>= peekCString

-- | Returns the canonical name of the 'WebDatabase'.
webDatabaseGetName ::
   WebDatabaseClass self => self
 -> IO String
webDatabaseGetName wd = 
  {#call web_database_get_name#} (toWebDatabase wd) >>= peekCString

-- | Returns the security origin of the WebKitWebDatabase.
webDatabaseGetSecurityOrigin :: 
   WebDatabaseClass self => self
 -> IO SecurityOrigin   
webDatabaseGetSecurityOrigin wd =
  makeNewGObject mkSecurityOrigin $ {#call web_database_get_security_origin#} (toWebDatabase wd)

-- | Returns the actual size of the 'WebDatabase' space on disk in bytes.  
webDatabaseGetSize ::
   WebDatabaseClass self => self  
 -> IO Int
webDatabaseGetSize wd = 
    liftM fromIntegral $ {#call web_database_get_size#} (toWebDatabase wd)
  
-- | Removes the 'WebDatabase' from its security origin and destroys all data stored in the database.
webDatabaseRemove ::
   WebDatabaseClass self => self
 -> IO ()
webDatabaseRemove wd =
  {#call web_database_remove#} (toWebDatabase wd)

