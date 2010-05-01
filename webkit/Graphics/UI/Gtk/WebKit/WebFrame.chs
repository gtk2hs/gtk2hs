{-# LANGUAGE CPP #-}
-- -*-haskell-*-
-----------------------------------------------------------------------------
--  Module      :  Graphics.UI.Gtk.WebKit.WebFrame
--  Author      :  Cjacker Huang
--  Copyright   :  (c) 2009 Cjacker Huang <jzhuang@redflag-linux.com>
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
-- The content of a 'WebView'
--
-- Note:
-- Functon `webkit_web_frame_get_global_context` can't binding now, 
-- Because it need `JSGlobalContextRef` exist in JavaScriptCore.
--
-- Function `webkit_web_frame_print_full` can't binding now,
-- Because library `GtkPrintOperation` haven't binding.
--
-----------------------------------------------------------------------------

module Graphics.UI.Gtk.WebKit.WebFrame (
-- * Types
  WebFrame,

-- * Constructors
  webFrameNew,

-- * Methods
  webFrameGetWebView,
  webFrameGetName,
  webFrameGetTitle,
  webFrameGetUri,
  webFrameGetParent,
  webFrameGetLoadStatus,
  webFrameLoadUri,
  webFrameLoadString,
  webFrameLoadAlternateString,
  webFrameLoadRequest,
  webFrameStopLoading,
  webFrameReload,
  webFrameFindFrame,
  webFrameGetDataSource,
  webFrameGetHorizontalScrollbarPolicy,
  webFrameGetVerticalScrollbarPolicy,
  webFrameGetProvisionalDataSource,
  webFrameGetSecurityOrigin,
  webFramePrint,
) where

import Control.Monad		(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GList
import System.Glib.GError 
import Graphics.UI.Gtk.Gdk.Events
import Graphics.UI.Gtk.General.Enums

{#import Graphics.UI.Gtk.Abstract.Object#}	(makeNewObject)
{#import Graphics.UI.Gtk.WebKit.Types#}
{#import System.Glib.GObject#}

{#context lib="webkit" prefix ="webkit"#}

-- * Enums

{#enum LoadStatus {underscoreToCase}#}

------------------
-- Constructors


-- | Create a new 'WebFrame' instance with the given @webview@.
--
-- A 'WebFrame' contains the content of one URI.
webFrameNew :: 
    WebViewClass webview => webview  -- ^ @webview@ - the given webview
 -> IO WebFrame
webFrameNew webview =  
    constructNewGObject mkWebFrame $ {#call web_frame_new#} (toWebView webview)

-- | Return the 'WebView' that manages the given 'WebFrame'.
webFrameGetWebView :: 
    WebFrameClass self => self
 -> IO WebView
webFrameGetWebView webframe = 
    makeNewObject mkWebView $ liftM castPtr $ {#call web_frame_get_web_view#} (toWebFrame webframe)

-- | Return the name of the given 'WebFrame'.
webFrameGetName :: 
    WebFrameClass self => self
 -> IO (Maybe String) -- ^ the name string or @Nothing@ in case failed.
webFrameGetName webframe = 
    {#call web_frame_get_name#} (toWebFrame webframe) >>= maybePeek peekCString

-- | Return the title of the given 'WebFrame'.
webFrameGetTitle :: 
    WebFrameClass self => self 
 -> IO (Maybe String) -- ^ the title string or @Nothing@ in case failed.
webFrameGetTitle webframe = 
    {#call web_frame_get_title#} (toWebFrame webframe) >>= maybePeek peekCString

-- | Return the URI of the given 'WebFrame'.	
webFrameGetUri :: 
    WebFrameClass self => self 
 -> IO (Maybe String) -- ^ the URI string or @Nothing@ in case failed.
webFrameGetUri webframe = 
    {#call web_frame_get_uri#} (toWebFrame webframe) >>= maybePeek peekCString

-- | Return the 'WebFrame''s parent frame if it has one,
-- Otherwise return Nothing.
webFrameGetParent :: 
    WebFrameClass self => self 
 -> IO (Maybe WebFrame) -- ^ a 'WebFrame' or @Nothing@ in case failed.
webFrameGetParent webframe = 
    maybeNull (makeNewGObject mkWebFrame) $ {#call web_frame_get_parent#} (toWebFrame webframe)

-- | Determines the current status of the load.
--
-- frame :   a WebKitWebView 
--                          
-- * Since 1.1.7
webFrameGetLoadStatus ::
    WebFrameClass self => self
 -> IO LoadStatus    
webFrameGetLoadStatus ls =
    liftM (toEnum . fromIntegral) $ {#call web_frame_get_load_status#} (toWebFrame ls)

-- | Request loading of the specified URI string.
webFrameLoadUri :: 
    WebFrameClass self => self 
 -> String -- ^ @uri@ - an URI string. 
 -> IO ()
webFrameLoadUri webframe uri = 
    withCString uri $ \uriPtr -> {#call web_frame_load_uri#}
    (toWebFrame webframe)
    uriPtr

-- | Requests loading of the given @content@ 
-- with the specified @mime_type@, @encoding@ and @base_uri@.
-- 
-- If @mime_type@ is @Nothing@, \"text/html\" is assumed.
--
-- If @encoding@ is @Nothing@, \"UTF-8\" is assumed.
webFrameLoadString :: 
    WebFrameClass self => self 
 -> String -- ^ @content@ - the content string to be loaded.
 -> (Maybe String) -- ^ @mime_type@ - the MIME type or @Nothing@. 
 -> (Maybe String) -- ^ @encoding@ - the encoding or @Nothing@.
 -> String -- ^ @base_uri@ - the base URI for relative locations.
 -> IO()
webFrameLoadString webframe content mimetype encoding baseuri = 
    withCString content  $ \contentPtr ->
    maybeWith withCString mimetype $ \mimetypePtr ->
    maybeWith withCString encoding $ \encodingPtr ->
    withCString baseuri  $ \baseuriPtr ->
        {#call web_frame_load_string#} 
          (toWebFrame webframe) 
          contentPtr 
          mimetypePtr 
          encodingPtr 
          baseuriPtr

-- |Request loading of an alternate content for a URL that is unreachable.
--
-- Using this method will preserve the back-forward list.
-- The URI passed in @base_uri@ has to be an absolute URI.		
webFrameLoadAlternateString :: 
    WebFrameClass self => self 
 -> String  -- ^ @content@ - the alternate content to display 
            -- as the main page of the frame
 -> String  -- ^ @base_uri@ - the base URI for relative locations. 
 -> String  -- ^ @unreachable_url@ - the URL for the alternate page content.
 -> IO()
webFrameLoadAlternateString webframe content baseurl unreachableurl = 
    withCString content  $ \contentPtr ->
    withCString baseurl  $ \baseurlPtr ->
    withCString unreachableurl  $ \unreachableurlPtr ->
        {#call web_frame_load_alternate_string#}
          (toWebFrame webframe) 
          contentPtr
          baseurlPtr
          unreachableurlPtr

-- | Connects to a given URI by initiating an asynchronous client request.
--
-- Creates a provisional data source that will transition to a committed data source once any data has been received. 
-- Use 'webFrameStopLoading' to stop the load. 
-- This function is typically invoked on the main frame.
webFrameLoadRequest :: 
   (WebFrameClass self, NetworkRequestClass requ) => self -> requ
 -> IO ()
webFrameLoadRequest webframe request =
  {#call web_frame_load_request#} (toWebFrame webframe) (toNetworkRequest request) 

-- | Stops and pending loads on the given data source and those of its children.
webFrameStopLoading :: 
    WebFrameClass self => self
 -> IO()
webFrameStopLoading webframe = 
    {#call web_frame_stop_loading#} (toWebFrame webframe)

-- |Reloads the initial request.
webFrameReload :: 
    WebFrameClass self => self
 -> IO()
webFrameReload webframe = 
    {#call web_frame_reload#} (toWebFrame webframe)

-- |Return the 'WebFrame' associated with the given name 
-- or @Nothing@ in case none if found
-- 
-- For pre-defined names, return the given webframe if name is 
webFrameFindFrame:: 
    WebFrameClass self => self 
 -> String  -- ^ @name@ - the name of the frame to be found.
 -> IO (Maybe WebFrame)
webFrameFindFrame webframe name = 
    withCString name $ \namePtr ->
	maybeNull (makeNewGObject mkWebFrame) $ 
          {#call web_frame_find_frame#} (toWebFrame webframe) namePtr

-- | Returns the committed data source.
webFrameGetDataSource :: 
   WebFrameClass self => self
 -> IO WebDataSource
webFrameGetDataSource webframe =
  makeNewGObject mkWebDataSource $ {#call web_frame_get_data_source#} (toWebFrame webframe)

-- | Return the policy of horizontal scrollbar.
webFrameGetHorizontalScrollbarPolicy :: 
   WebFrameClass self => self
 -> IO PolicyType   
webFrameGetHorizontalScrollbarPolicy webframe = 
    liftM (toEnum.fromIntegral) $
    {#call web_frame_get_horizontal_scrollbar_policy#} (toWebFrame webframe)
  
-- | Return the policy of vertical scrollbar.
webFrameGetVerticalScrollbarPolicy :: 
   WebFrameClass self => self
 -> IO PolicyType   
webFrameGetVerticalScrollbarPolicy webframe = 
    liftM (toEnum.fromIntegral) $
    {#call web_frame_get_vertical_scrollbar_policy#} (toWebFrame webframe)

-- | You use the 'webFrameLoadRequest' method to initiate a request that creates a provisional data source. 
-- The provisional data source will transition to a committed data source once any data has been received. 
-- Use 'webFrameGetDataSource' to get the committed data source.
webFrameGetProvisionalDataSource :: 
   WebFrameClass self => self
 -> IO WebDataSource   
webFrameGetProvisionalDataSource webframe =
  makeNewGObject mkWebDataSource $ {#call web_frame_get_provisional_data_source#} (toWebFrame webframe)

-- | Returns the frame's security origin.
webFrameGetSecurityOrigin ::
   WebFrameClass self => self
 -> IO SecurityOrigin   
webFrameGetSecurityOrigin webframe = 
  makeNewGObject mkSecurityOrigin $ {#call web_frame_get_security_origin#} (toWebFrame webframe)

-- |Prints the given 'WebFrame'.
--
-- by presenting a print dialog to the user. 
webFramePrint:: 
    WebFrameClass self => self
 -> IO()
webFramePrint webframe = 
  {#call web_frame_print#} (toWebFrame webframe)

