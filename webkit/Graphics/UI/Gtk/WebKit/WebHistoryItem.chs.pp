-- -*-haskell-*-
-----------------------------------------------------------------------------
--  Module      :  Graphics.UI.Gtk.WebKit.WebHistoryItem
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
-- One item of the 'WebBackForwardList' and or global history
-----------------------------------------------------------------------------

module Graphics.UI.Gtk.WebKit.WebHistoryItem (
-- * Types
  WebHistoryItem,

-- * Constructors
  webHistoryItemNew,
  webHistoryItemNewWithData,

-- * Attributes
  webHistoryItemTitle,
  webHistoryItemAlternateTitle,
  webHistoryItemUri,
  webHistoryItemOriginalUri,
  webHistoryItemLastVisitedTime,

-- * Methods
  webHistoryItemGetTitle,
  webHistoryItemGetAlternateTitle,
  webHistoryItemSetAlternateTitle,
  webHistoryItemGetUri,
  webHistoryItemGetOriginalUri,
  webHistoryItemGetLastVisitedTime,
) where

import Control.Monad		(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GList
import System.Glib.GError 
import System.Glib.Attributes
import Graphics.UI.Gtk.Gdk.Events

{#import Graphics.UI.Gtk.Abstract.Object#}	(makeNewObject)
{#import Graphics.UI.Gtk.Signals#}
{#import Graphics.UI.Gtk.WebKit.Types#}
{#import System.Glib.GObject#}

{#context lib="webkit" prefix ="webkit"#}

------------------
-- Constructors



-- | Create a new 'WebHistoryItem' instance.
--
-- A history item consists out of a title and a uri, 
-- it can be part of the WebBackForwardList and the global history.
--
-- The global history is used for coloring the links of visited sites.
-- 'WebHistoryItem' constructed with 'webHistoryItemNew' are 
-- automatically added to the global history.
webHistoryItemNew :: IO WebHistoryItem
webHistoryItemNew = 
    constructNewGObject mkWebHistoryItem $ {#call web_history_item_new#}


-- | Create a new 'WebHistoryItem' instance with the given @uri@ and @title@.
-- 
-- 'WebHistoryItem' constructed with 'webHistoryItemNewWithData' are 
-- automatically added to the global history.
webHistoryItemNewWithData :: 
    String -- ^ @uri@ - the uri of the item
 -> String -- ^ @title@ - the title of the item
 -> IO WebHistoryItem
webHistoryItemNewWithData uri title = 
    withCString uri $ \uriPtr ->
    withCString title $ \titlePtr ->
    constructNewGObject mkWebHistoryItem $
      {#call web_history_item_new_with_data#} 
        uriPtr 
        titlePtr


-- | Return the title of 'WebHistoryItem'.
webHistoryItemGetTitle :: 
    WebHistoryItemClass self => self
 -> IO (Maybe String) -- ^ the title or @Nothing@ in case failed.
webHistoryItemGetTitle webhistoryitem = 
    {#call web_history_item_get_title#} 
      (toWebHistoryItem webhistoryitem) >>= 
      maybePeek peekCString

-- | Return the alternate title of WebHistoryItem.
webHistoryItemGetAlternateTitle :: 
    WebHistoryItemClass self => self 
 -> IO (Maybe String) -- ^ the alternate title or @Nothing@ in case failed.
webHistoryItemGetAlternateTitle webhistoryitem = 
    {#call web_history_item_get_alternate_title#} 
      (toWebHistoryItem webhistoryitem) >>= 
      maybePeek peekCString

-- | Set an alternate title for WebHistoryItem.
webHistoryItemSetAlternateTitle :: 
    WebHistoryItemClass self => self 
 -> (Maybe String)  -- ^ @title@ - the alternate title for this history item.
 -> IO()
webHistoryItemSetAlternateTitle webhistoryitem title =
    maybeWith withCString title $ \titlePtr -> 
    {#call web_history_item_set_alternate_title#} 
      (toWebHistoryItem webhistoryitem)
      titlePtr

-- | Return the URI of WebHistoryItem.
webHistoryItemGetUri :: 
    WebHistoryItemClass self => self 
 -> IO (Maybe String) -- ^ the URI or @Nothing@ in case failed.
webHistoryItemGetUri webhistoryitem = 
    {#call web_history_item_get_uri#} 
      (toWebHistoryItem webhistoryitem) >>= 
      maybePeek peekCString

-- | Return the original URI of WebHistoryItem.
webHistoryItemGetOriginalUri :: 
    WebHistoryItemClass self => self 
 -> IO (Maybe String) -- ^ the URI or @Nothing@ in case failed
webHistoryItemGetOriginalUri webhistoryitem = 
    {#call web_history_item_get_original_uri#} 
      (toWebHistoryItem webhistoryitem) >>= 
      maybePeek peekCString

-- | Return the last visited time of WebHistoryItem.
webHistoryItemGetLastVisitedTime :: 
    WebHistoryItemClass self => self 
 -> IO Double  -- ^ the last visited time of this history item.
webHistoryItemGetLastVisitedTime webhistoryitem = 
    liftM realToFrac $ 
      {#call web_history_item_get_last_visited_time#} 
        (toWebHistoryItem webhistoryitem)

-- | The title of the 'WebHistoryItem'
--
-- Default value: @Nothing@
webHistoryItemTitle :: (WebHistoryItemClass self) => ReadAttr self (Maybe String)
webHistoryItemTitle = readAttr webHistoryItemGetTitle

-- | The alternate title of the history item.
--
-- Default value: @Nothing@
webHistoryItemAlternateTitle :: (WebHistoryItemClass self) => Attr self (Maybe String)
webHistoryItemAlternateTitle = newAttr webHistoryItemGetAlternateTitle webHistoryItemSetAlternateTitle

-- | The URI of the history item.
--
-- Default value: @Nothing@
webHistoryItemUri :: (WebHistoryItemClass self) => ReadAttr self (Maybe String)
webHistoryItemUri = readAttr webHistoryItemGetUri

-- | The original URI of the history item.
--
-- Default value: @Nothing@
webHistoryItemOriginalUri :: (WebHistoryItemClass self) => ReadAttr self (Maybe String)
webHistoryItemOriginalUri = readAttr webHistoryItemGetOriginalUri

-- | The time at which the history item was last visited.
--
-- Allowed values: >= 0
--
-- Default value: 0
webHistoryItemLastVisitedTime :: (WebHistoryItemClass self) => ReadAttr self Double
webHistoryItemLastVisitedTime = readAttr webHistoryItemGetLastVisitedTime
 
