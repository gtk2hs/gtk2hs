{-# LANGUAGE CPP #-}
-- -*-haskell-*-
-----------------------------------------------------------------------------
--  Module      :  Graphics.UI.Gtk.WebKit.Download
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
-- The history of a 'WebView'
-----------------------------------------------------------------------------

module Graphics.UI.Gtk.WebKit.WebBackForwardList (
-- * Types
  WebBackForwardList,

-- * Constructors
  webBackForwardListNewWithWebView,

-- * Methods
  webBackForwardListGoForward,
  webBackForwardListGoBack,
  webBackForwardListContainsItem,
  webBackForwardListGoToItem,
  webBackForwardListGetBackItem,
  webBackForwardListGetCurrentItem,
  webBackForwardListGetForwardItem,
  webBackForwardListGetNthItem,
  webBackForwardListGetBackLength,
  webBackForwardListGetForwardLength,
  webBackForwardListGetLimit,
  webBackForwardListSetLimit,
  webBackForwardListAddItem,
  webBackForwardListGetForwardListWithLimit,
  webBackForwardListGetBackListWithLimit,
 
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

-- | Create an WebBackForwardList with a controlling WebView.
webBackForwardListNewWithWebView :: (WebViewClass webview) => webview -> IO WebBackForwardList
webBackForwardListNewWithWebView webview = 
    constructNewGObject mkWebBackForwardList $ 
      {#call web_back_forward_list_new_with_web_view#} 
        (toWebView webview)

-- | Steps forward in the back forward list.
webBackForwardListGoForward :: 
    WebBackForwardListClass self => self
 -> IO()
webBackForwardListGoForward webbackforwardlist = 
    {#call web_back_forward_list_go_forward#} (toWebBackForwardList webbackforwardlist)

-- | Steps back in the back forward list.
webBackForwardListGoBack :: 
    WebBackForwardListClass self => self
 -> IO()
webBackForwardListGoBack webbackforwardlist = 
    {#call web_back_forward_list_go_back#} (toWebBackForwardList webbackforwardlist)

-- | Check if an history item in the back forward list.
webBackForwardListContainsItem :: 
    (WebBackForwardListClass self, WebHistoryItemClass item) => self
 -> item
 -> IO Bool
webBackForwardListContainsItem webbackforwardlist webhistoryitem = 
    liftM toBool $ {#call web_back_forward_list_contains_item#} 
      (toWebBackForwardList webbackforwardlist)
      (toWebHistoryItem webhistoryitem)

-- | Go to the specified history item in the back forward list.
webBackForwardListGoToItem :: 
    (WebBackForwardListClass self,WebHistoryItemClass item) => self
 -> item
 -> IO()
webBackForwardListGoToItem webbackforwardlist webhistoryitem =
    {#call web_back_forward_list_go_to_item#} 
      (toWebBackForwardList webbackforwardlist)
      (toWebHistoryItem webhistoryitem)

-- | Return the history item that precedes the current history item.
webBackForwardListGetBackItem :: 
    WebBackForwardListClass self => self 
 -> IO (Maybe WebHistoryItem) -- ^ A 'WebHistoryItem' or @Nothing@ 
                              -- if there is nothing precedes the current item.
webBackForwardListGetBackItem webbackforwardlist = 
    maybeNull (makeNewGObject mkWebHistoryItem) $ 
      {#call web_back_forward_list_get_back_item#} 
        (toWebBackForwardList webbackforwardlist)

-- | Return the current history item of the back forward list
webBackForwardListGetCurrentItem :: 
    WebBackForwardListClass self => self
 -> IO WebHistoryItem
webBackForwardListGetCurrentItem webbackforwardlist = 
    makeNewGObject mkWebHistoryItem $ 
      {#call web_back_forward_list_get_current_item#} 
      (toWebBackForwardList webbackforwardlist)

-- | Return the item that succeeds the current item 

webBackForwardListGetForwardItem :: 
    WebBackForwardListClass self => self 
 -> IO (Maybe WebHistoryItem) -- ^ A 'WebHistoryItem' or @Nothing@ 
                              -- if there is nothing succeeds the current item.
webBackForwardListGetForwardItem webbackforwardlist = 
    maybeNull (makeNewGObject mkWebHistoryItem) $ 
      {#call web_back_forward_list_get_forward_item#} 
      (toWebBackForwardList webbackforwardlist)

-- | Return the history item at a given index relative to the current item.
webBackForwardListGetNthItem :: 
    WebBackForwardListClass self => self -- ^ @webbackforwardlist@ - a WebBackForwardList
 -> Int  -- ^ @index@ - the index of the item
 -> IO WebHistoryItem
webBackForwardListGetNthItem webbackforwardlist index =  
    makeNewGObject mkWebHistoryItem  $ 
      {#call web_back_forward_list_get_nth_item#} 
      (toWebBackForwardList webbackforwardlist)
      (fromIntegral index)

-- | Return the number of items that preced the current item.
webBackForwardListGetBackLength :: 
    WebBackForwardListClass self => self
 -> IO Int 
webBackForwardListGetBackLength webbackforwardlist = 
    liftM fromIntegral $ 
      {#call web_back_forward_list_get_back_length#} 
        (toWebBackForwardList webbackforwardlist)

-- | Return the number of items that succeed the current item.
webBackForwardListGetForwardLength :: 
    WebBackForwardListClass self => self
 -> IO Int
webBackForwardListGetForwardLength webbackforwardlist = 
    liftM fromIntegral $ 
      {#call web_back_forward_list_get_forward_length#} 
        (toWebBackForwardList webbackforwardlist)

-- | Return the maximum limit of the back forward list.
webBackForwardListGetLimit :: 
    WebBackForwardListClass self => self
 -> IO Int
webBackForwardListGetLimit webbackforwardlist = 
    liftM fromIntegral $ 
      {#call web_back_forward_list_get_limit#} 
        (toWebBackForwardList webbackforwardlist)

-- | Set the maximum limit of the back forward list. 
-- 
-- if the back forward list exceeds its capacity, 
-- items will be removed everytime a new item had been added.
--
webBackForwardListSetLimit :: 
    WebBackForwardListClass self => self
 -> Int
 -> IO()
webBackForwardListSetLimit webbackforwardlist limit = 
    {#call web_back_forward_list_set_limit#} 
      (toWebBackForwardList webbackforwardlist)
      (fromIntegral limit)

-- | Add the item to the back forward list.
webBackForwardListAddItem :: 
    (WebBackForwardListClass self,WebHistoryItemClass item) => self
 -> item
 -> IO ()
webBackForwardListAddItem webbackforwardlist webhistoryitem = 
    {#call web_back_forward_list_add_item#} 
      (toWebBackForwardList webbackforwardlist)
      (toWebHistoryItem webhistoryitem)

-- | Return a list of items that succeed the current item, limited by @limit@.
webBackForwardListGetForwardListWithLimit :: 
    WebBackForwardListClass self => self 
 -> Int  -- ^ the number of items to retrieve
 -> IO [WebHistoryItem] -- ^ a 'List' of items succeeding the current item, limited by limit.
webBackForwardListGetForwardListWithLimit webbackforwardlist limit = 
  {#call web_back_forward_list_get_forward_list_with_limit#} 
    (toWebBackForwardList webbackforwardlist)
    (fromIntegral limit)
    >>= fromGList 
    >>= mapM (makeNewGObject mkWebHistoryItem . return)

-- | Return a list of items that preced the current item.
-- limited by limit.
webBackForwardListGetBackListWithLimit :: 
    WebBackForwardListClass self => self 
 -> Int -- ^ the number of items to retrieve
 -> IO [WebHistoryItem] -- ^ a 'List' of items preceding the current item, limited by limit
webBackForwardListGetBackListWithLimit webbackforwardlist limit = 
  {#call web_back_forward_list_get_back_list_with_limit#} 
    (toWebBackForwardList webbackforwardlist)
    (fromIntegral limit)
    >>= fromGList 
    >>= mapM (makeNewGObject mkWebHistoryItem . return)

