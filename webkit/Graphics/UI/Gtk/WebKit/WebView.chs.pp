-- -*-haskell-*-
-----------------------------------------------------------------------------
--  Module      :  Graphics.UI.Gtk.WebKit.WebView
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
-- The central class of the WebKit
-----------------------------------------------------------------------------

module Graphics.UI.Gtk.WebKit.WebView (
-- * Types
  WebView,

-- * Constructors
  webViewNew,
-- * Methods
  LoadStatus(..),

  webViewLoadUri,
  webViewLoadHtmlString,
  webViewLoadRequest,
  webViewLoadString,

  webViewGetTitle,
  webViewGetUri,

  webViewCanGoBack,
  webViewCanGoForward,
  webViewGoBack,
  webViewGoForward,
  webViewGetBackForwardList,
  webViewSetMaintainsBackForwardList,
  webViewGoToBackForwardItem,
  webViewCanGoBackOrForward,
  webViewGoBackOrForward,


  webViewGetZoomLevel,
  webViewSetZoomLevel,
  webViewZoomIn,
  webViewZoomOut,
  webViewGetFullContentZoom,
  webViewSetFullContentZoom,

  webViewStopLoading,
  webViewReload,
  webViewReloadBypassCache,

  webViewExecuteScript,

  webViewCanCutClipboard,
  webViewCanCopyClipboard,
  webViewCanPasteClipboard,
  webViewCutClipboard,
  webViewCopyClipboard,
  webViewPasteClipboard,

  webViewCanRedo,
  webViewCanUndo,
  webViewRedo,
  webViewUndo,
  
  webViewCanShowMimeType,
  webViewGetEditable,
  webViewSetEditable,
  webViewGetInspector,
  webViewGetTransparent,
  webViewSetTransparent,
  webViewGetViewSourceMode,
  webViewSetViewSourceMode,

  webViewDeleteSelection,
  webViewHasSelection,
  webViewSelectAll,

  webViewGetEncoding,
  webViewSetCustomEncoding,
  webViewGetCustomEncoding,
  webViewGetProgress,

  webViewSearchText,
  webViewMarkTextMatches,
  webViewUnMarkTextMatches,
  webViewSetHighlightTextMatches,

  webViewGetMainFrame,
  webViewGetFocusedFrame,

  webViewSetWebSettings,
  webViewGetWebSettings,

-- * Attributes
  webViewZoomLevel,
  webViewFullContentZoom,
  webViewEncoding,
  webViewCustomEncoding,
  webViewLoadStatus,
  webViewProgress,
  webViewTitle,
  webViewInspector,
  webViewWebSettings,
  webViewViewSourceMode,
  webViewTransparent,
  webViewEditable,
    
 
-- * Signals
  onLoadStarted,
  onLoadCommitted,
  onProgressChanged,
  onLoadFinished,
  onLoadError,
  onTitleChanged,
  onHoveringOverLink,

  onCreateWebView,
  onWebViewReady,
  onCloseWebView,
  onDownloadRequested,
  onIconLoaded,
  onRedo,
  onUndo,

) where

import Control.Monad		(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GList
import System.Glib.Attributes
import System.Glib.GError 
import Graphics.UI.Gtk.Gdk.Events


{#import Graphics.UI.Gtk.Abstract.Object#}	(makeNewObject)
{#import Graphics.UI.Gtk.Signals#}
{#import Graphics.UI.Gtk.WebKit.Types#}
{#import System.Glib.GObject#}

{#context lib="webkit" prefix ="webkit"#}

{#enum LoadStatus {underscoreToCase}#}

------------------
-- Constructors


-- | Create a new 'WebView' widget.
-- 
-- It is a 'Widget' you can embed in a 'ScrolledWindow'.
-- 
-- You can load any URI into the 'WebView' or any kind of data string.
webViewNew :: IO WebView 
webViewNew =  do
  isGthreadInited <- liftM toBool {#call g_thread_get_initialized#}
  if not isGthreadInited then {#call g_thread_init#} nullPtr 
    else return ()
  makeNewObject mkWebView $ liftM castPtr {#call web_view_new#}


-- | Apply 'WebSettings' to a given 'WebView'
-- 
-- !!NOTE!!, currently lack of useful APIs of 'WebSettings' in webkitgtk.
-- If you want to set the encoding, font family or font size of the 'WebView',
-- please use related functions.

webViewSetWebSettings :: 
    (WebViewClass self, WebSettingsClass settings) => self
 -> settings
 -> IO ()
webViewSetWebSettings webview websettings = 
    {#call web_view_set_settings#} (toWebView webview) (toWebSettings websettings)

-- | Return the 'WebSettings' currently used by 'WebView'.
webViewGetWebSettings :: 
    WebViewClass self => self
 -> IO WebSettings
webViewGetWebSettings webview = 
    makeNewGObject mkWebSettings $ {#call web_view_get_settings#} (toWebView webview)

-- | Return the main 'WebFrame' of the given 'WebView'.
webViewGetMainFrame :: 
    WebViewClass self => self
 -> IO WebFrame
webViewGetMainFrame webview = 
    makeNewGObject mkWebFrame  $ {#call web_view_get_main_frame#} (toWebView webview)

-- | Return the focused 'WebFrame' of the given 'WebView'.
webViewGetFocusedFrame :: 
    WebViewClass self => self
 -> IO WebFrame
webViewGetFocusedFrame webview = 
    makeNewGObject mkWebFrame $ {#call web_view_get_focused_frame#} (toWebView webview)


-- |Requests loading of the specified URI string in a 'WebView'
webViewLoadUri :: 
    WebViewClass self => self 
 -> String  -- ^ @uri@ - an URI string.
 -> IO()
webViewLoadUri webview url =
    withCString url $ \urlPtr -> {#call web_view_load_uri#}
    (toWebView webview)
    urlPtr

-- |Determine whether 'WebView' has a previous history item.
webViewCanGoBack :: 
    WebViewClass self => self
 -> IO Bool -- ^ True if able to move back, False otherwise.
webViewCanGoBack webview = 
    liftM toBool $ {#call web_view_can_go_back#} (toWebView webview)

-- |Determine whether 'WebView' has a next history item.
webViewCanGoForward :: 
    WebViewClass self => self 
 -> IO Bool -- ^ True if able to move forward, False otherwise.
webViewCanGoForward webview = 
    liftM toBool $ {#call web_view_can_go_forward#} (toWebView webview)

-- |Loads the previous history item.
webViewGoBack :: 
    WebViewClass self => self
 -> IO () 
webViewGoBack webview =
    {#call web_view_go_back#} (toWebView webview)

-- |Loads the next history item.
webViewGoForward :: 
    WebViewClass self => self
 -> IO ()
webViewGoForward webview =
    {#call web_view_go_forward#} (toWebView webview)

-- |Set the 'WebView' to maintian a back or forward list of history items.
webViewSetMaintainsBackForwardList :: 
    WebViewClass self => self 
 -> Bool -- ^ @flag@ - to tell the view to maintain a back or forward list. 
 -> IO()
webViewSetMaintainsBackForwardList webview flag = 
    {#call web_view_set_maintains_back_forward_list#} 
      (toWebView webview)
      (fromBool flag)

-- |Return the 'WebBackForwardList'
webViewGetBackForwardList :: 
    WebViewClass self => self
 -> IO WebBackForwardList
webViewGetBackForwardList webview = 
    makeNewGObject mkWebBackForwardList $ 
      {#call web_view_get_back_forward_list#} 
        (toWebView webview)

-- |Go to the specified 'WebHistoryItem'

webViewGoToBackForwardItem :: 
    (WebViewClass self, WebHistoryItemClass item) => self 
 -> item
 -> IO Bool -- ^ True if loading of item is successful, False if not.
webViewGoToBackForwardItem webview item = 
    liftM toBool $ {#call web_view_go_to_back_forward_item#} (toWebView webview) (toWebHistoryItem item)

-- |Determines whether 'WebView' has a history item of @steps@.
--
-- Negative values represent steps backward while positive values
-- represent steps forward

webViewCanGoBackOrForward :: 
    WebViewClass self => self
 -> Int -- ^ @steps@ - the number of steps 
 -> IO Bool -- ^ True if able to move back or forward the given number of steps,
            -- False otherwise
webViewCanGoBackOrForward webview steps =
    liftM toBool $  
      {#call web_view_can_go_back_or_forward#} 
        (toWebView webview)
        (fromIntegral steps)

-- |Loads the history item that is the number of @steps@ away from the current item.
--
-- Negative values represent steps backward while positive values represent steps forward.

webViewGoBackOrForward :: 
    WebViewClass self => self
 -> Int
 -> IO ()
webViewGoBackOrForward webview steps =
    {#call web_view_go_back_or_forward#} 
      (toWebView webview)
      (fromIntegral steps)

-- |Determines whether or not it is currently possible to redo the last editing command in the view
webViewCanRedo :: 
    WebViewClass self => self
 -> IO Bool
webViewCanRedo webview = 
    liftM toBool $
      {#call web_view_can_redo#} (toWebView webview)
-- |Determines whether or not it is currently possible to undo the last editing command in the view
webViewCanUndo :: 
    WebViewClass self => self
 -> IO Bool
webViewCanUndo webview =
    liftM toBool $
      {#call web_view_can_undo#} (toWebView webview)

-- |Redoes the last editing command in the view, if possible.
webViewRedo :: 
    WebViewClass self => self
 -> IO()
webViewRedo webview =
    {#call web_view_redo#} (toWebView webview)

-- |Undoes the last editing command in the view, if possible.
webViewUndo :: 
    WebViewClass self => self
 -> IO()
webViewUndo webview =
    {#call web_view_undo#} (toWebView webview)

-- | Returns whether or not a @mimetype@ can be displayed using this view.
webViewCanShowMimeType :: 
    WebViewClass self => self
 -> String -- ^ @mimetype@ - a MIME type
 -> IO Bool -- ^ True if the @mimetype@ can be displayed, otherwise False
webViewCanShowMimeType webview mime =
    withCString mime $ \mimePtr ->
    liftM toBool $
      {#call web_view_can_show_mime_type#}
      (toWebView webview)
      mimePtr

-- | Returns whether the user is allowed to edit the document.
webViewGetEditable :: 
    WebViewClass self => self
 -> IO Bool
webViewGetEditable webview =
    liftM toBool $
      {#call web_view_get_editable#} (toWebView webview)

-- | Sets whether allows the user to edit its HTML document.
webViewSetEditable :: 
    WebViewClass self => self
 -> Bool
 -> IO ()
webViewSetEditable webview editable =
    {#call web_view_set_editable#} (toWebView webview) (fromBool editable)

-- | Returns whether 'WebView' is in view source mode
webViewGetViewSourceMode :: 
    WebViewClass self => self
 -> IO Bool
webViewGetViewSourceMode webview =
    liftM toBool $
      {#call web_view_get_view_source_mode#} (toWebView webview)

-- | Set whether the view should be in view source mode. 
--
-- Setting this mode to TRUE before loading a URI will display 
-- the source of the web page in a nice and readable format.
webViewSetViewSourceMode :: 
    WebViewClass self => self
 -> Bool
 -> IO ()
webViewSetViewSourceMode webview mode =
    {#call web_view_set_view_source_mode#} (toWebView webview) (fromBool mode)

-- | Returns whether the 'WebView' has a transparent background
webViewGetTransparent :: 
    WebViewClass self => self
 -> IO Bool
webViewGetTransparent webview =
    liftM toBool $
      {#call web_view_get_transparent#} (toWebView webview)
-- |Sets whether the WebKitWebView has a transparent background.
--
-- Pass False to have the 'WebView' draw a solid background (the default), 
-- otherwise pass True.
webViewSetTransparent :: 
    WebViewClass self => self
 -> Bool
 -> IO ()
webViewSetTransparent webview trans =
    {#call web_view_set_transparent#} (toWebView webview) (fromBool trans)

-- |Obtains the 'WebInspector' associated with the 'WebView'
webViewGetInspector :: 
    WebViewClass self => self
 -> IO WebInspector
webViewGetInspector webview =
    makeNewGObject mkWebInspector $ {#call web_view_get_inspector#} (toWebView webview)

-- |Requests loading of the specified asynchronous client request.
--
-- Creates a provisional data source that will transition to a committed data source once
-- any data has been received. 
-- use 'webViewStopLoading' to stop the load.

webViewLoadRequest :: 
    (WebViewClass self, NetworkRequestClass request) => self
 -> request
 -> IO()
webViewLoadRequest webview request =
    {#call web_view_load_request#} (toWebView webview) (toNetworkRequest request)




-- |Returns the zoom level of 'WebView'
--
-- i.e. the factor by which elements in the page are scaled with respect to their original size.

webViewGetZoomLevel :: 
    WebViewClass self => self
 -> IO Float -- ^ the zoom level of 'WebView'
webViewGetZoomLevel webview =
    liftM realToFrac $
	{#call web_view_get_zoom_level#} (toWebView webview)

-- |Sets the zoom level of 'WebView'.
webViewSetZoomLevel :: 
    WebViewClass self => self 
 -> Float -- ^ @zoom_level@ - the new zoom level 
 -> IO ()
webViewSetZoomLevel webview zlevel = 
    {#call web_view_set_zoom_level#} (toWebView webview) (realToFrac zlevel)

-- |Loading the @content@ string as html. The URI passed in base_uri has to be an absolute URI.

webViewLoadHtmlString :: 
    WebViewClass self => self 
 -> String  -- ^ @content@ - the html string
 -> String  -- ^ @base_uri@ - the base URI
 -> IO()
webViewLoadHtmlString webview htmlstr url =
    withCString htmlstr $ \htmlPtr ->
    withCString url  $ \urlPtr ->
        {#call web_view_load_html_string#} (toWebView webview) htmlPtr urlPtr

-- | Requests loading of the given @content@ with the specified @mime_type@, @encoding@ and @base_uri@.
-- 
-- If @mime_type@ is @Nothing@, "text/html" is assumed.
--
-- If @encoding@ is @Nothing@, "UTF-8" is assumed.
--
webViewLoadString :: 
    WebViewClass self => self 
 -> String -- ^ @content@ - the content string to be loaded.
 -> (Maybe String) -- ^ @mime_type@ - the MIME type or @Nothing@. 
 -> (Maybe String) -- ^ @encoding@ - the encoding or @Nothing@.
 -> String -- ^ @base_uri@ - the base URI for relative locations.
 -> IO()
webViewLoadString webview content mimetype encoding baseuri = 
    withCString content  $ \contentPtr ->
    maybeWith withCString mimetype $ \mimetypePtr ->
    maybeWith withCString encoding $ \encodingPtr ->
    withCString baseuri  $ \baseuriPtr ->
        {#call web_view_load_string#} 
          (toWebView webview)
          contentPtr
          mimetypePtr
          encodingPtr
          baseuriPtr

-- |Returns the 'WebView' document title
webViewGetTitle :: 
    WebViewClass self => self
 -> IO (Maybe String) -- ^ the title of 'WebView' or Nothing in case of failed.
webViewGetTitle webview =
    {#call web_view_get_title#} (toWebView webview) >>= maybePeek peekCString

-- |Returns the current URI of the contents displayed by the 'WebView'
webViewGetUri :: 
    WebViewClass self => self
 -> IO (Maybe String) -- ^ the URI of 'WebView' or Nothing in case of failed.
webViewGetUri webview = 
    {#call web_view_get_uri#} (toWebView webview) >>= maybePeek peekCString 


-- | Stops and pending loads on the given data source.
webViewStopLoading :: 
    WebViewClass self => self
 -> IO ()
webViewStopLoading webview = 
    {#call web_view_stop_loading#} (toWebView webview)

-- | Reloads the 'WebView'
webViewReload :: 
    WebViewClass self => self
 -> IO ()
webViewReload webview = 
    {#call web_view_reload#} (toWebView webview)

-- | Reloads the 'WebView' without using any cached data.
webViewReloadBypassCache :: 
    WebViewClass self => self
 -> IO()
webViewReloadBypassCache webview = 
    {#call web_view_reload_bypass_cache#} (toWebView webview)

-- | Increases the zoom level of 'WebView'.
webViewZoomIn :: 
    WebViewClass self => self
 -> IO()
webViewZoomIn webview = 
    {#call web_view_zoom_in#} (toWebView webview)

-- | Decreases the zoom level of 'WebView'.
webViewZoomOut :: 
    WebViewClass self => self
 -> IO()
webViewZoomOut webview = 
    {#call web_view_zoom_out#} (toWebView webview)

-- | Looks for a specified string inside 'WebView'
webViewSearchText :: 
    WebViewClass self => self
 -> String -- ^ @text@ - a string to look for
 -> Bool -- ^ @case_sensitive@ - whether to respect the case of text
 -> Bool -- ^ @forward@ - whether to find forward or not
 -> Bool -- ^ @wrap@ - whether to continue looking at beginning
         -- after reaching the end
 -> IO Bool -- ^ True on success or False on failure
webViewSearchText webview text case_sensitive forward wrap =
    withCString text $ \textPtr ->
	liftM toBool $
          {#call web_view_search_text#} 
            (toWebView webview)
            textPtr
            (fromBool case_sensitive) 
            (fromBool forward) 
            (fromBool wrap)

-- |Attempts to highlight all occurances of string inside 'WebView'
webViewMarkTextMatches :: 
    WebViewClass self => self
 -> String -- ^ @string@ - a string to look for
 -> Bool  -- ^ @case_sensitive@ - whether to respect the case of text
 -> Int  -- ^ @limit@ - the maximum number of strings to look for or 0 for all
 -> IO Int -- ^ the number of strings highlighted
webViewMarkTextMatches webview text case_sensitive limit = 
    withCString text $ \textPtr ->
	liftM fromIntegral $ 
          {#call web_view_mark_text_matches#} 
            (toWebView webview)
            textPtr
            (fromBool case_sensitive)
            (fromIntegral limit)

-- | Removes highlighting previously set by 'webViewMarkTextMarches'
webViewUnMarkTextMatches :: 
    WebViewClass self => self
 -> IO ()
webViewUnMarkTextMatches webview = 
    {#call web_view_unmark_text_matches#} (toWebView webview)

-- | Highlights text matches previously marked by 'webViewMarkTextMatches'
webViewSetHighlightTextMatches :: 
    WebViewClass self => self
 -> Bool -- ^ @highlight@ - whether to highlight text matches 
 -> IO ()
webViewSetHighlightTextMatches webview highlight =
    {#call web_view_set_highlight_text_matches#} 
      (toWebView webview)
      (fromBool highlight)

-- | Execute the script specified by @script@
webViewExecuteScript :: 
    WebViewClass self => self 
 -> String  -- ^ @script@ - script to be executed
 -> IO()
webViewExecuteScript webview script =
    withCString script $ \scriptPtr ->
	{#call web_view_execute_script#} (toWebView webview) scriptPtr

-- | Determines whether can cuts the current selection
-- inside 'WebView' to the clipboard
webViewCanCutClipboard :: 
    WebViewClass self => self
 -> IO Bool
webViewCanCutClipboard webview = 
    liftM toBool $ {#call web_view_can_cut_clipboard#} (toWebView webview)

-- | Determines whether can copies the current selection
-- inside 'WebView' to the clipboard
webViewCanCopyClipboard :: WebViewClass self => self -> IO Bool
webViewCanCopyClipboard webview = 
    liftM toBool $ {#call web_view_can_copy_clipboard#} (toWebView webview)

-- | Determines whether can pastes the current contents of the clipboard
-- to the 'WebView'
webViewCanPasteClipboard :: WebViewClass self => self -> IO Bool
webViewCanPasteClipboard webview = 
    liftM toBool $ {#call web_view_can_paste_clipboard#} (toWebView webview)

-- | Cuts the current selection inside 'WebView' to the clipboard.
webViewCutClipboard :: WebViewClass self => self -> IO()
webViewCutClipboard webview = 
    {#call web_view_cut_clipboard#} (toWebView webview)

-- | Copies the current selection inside 'WebView' to the clipboard.
webViewCopyClipboard :: WebViewClass self => self -> IO()
webViewCopyClipboard webview = 
    {#call web_view_copy_clipboard#} (toWebView webview)

-- | Pastes the current contents of the clipboard to the 'WebView'
webViewPasteClipboard :: WebViewClass self => self -> IO()
webViewPasteClipboard webview = 
    {#call web_view_paste_clipboard#} (toWebView webview)

-- | Deletes the current selection inside the 'WebView'
webViewDeleteSelection :: WebViewClass self => self -> IO ()
webViewDeleteSelection webview = 
    {#call web_view_delete_selection#} (toWebView webview)

-- | Determines whether text was selected
webViewHasSelection :: WebViewClass self => self -> IO Bool
webViewHasSelection webview = 
    liftM toBool $ {#call web_view_has_selection#} (toWebView webview)

-- | Attempts to select everything inside the 'WebView'
webViewSelectAll :: WebViewClass self => self -> IO ()
webViewSelectAll webview = 
    {#call web_view_select_all#} (toWebView webview)

-- | Returns whether the zoom level affects only text or all elements.
webViewGetFullContentZoom :: 
    WebViewClass self => self 
 -> IO Bool -- ^ False if only text should be scaled(the default)
            -- True if the full content of the view should be scaled.
webViewGetFullContentZoom webview = 
    liftM toBool $ {#call web_view_get_full_content_zoom#} (toWebView webview)

-- | Sets whether the zoom level affects only text or all elements.
webViewSetFullContentZoom :: 
    WebViewClass self => self 
 -> Bool -- ^ @full_content_zoom@ - False if only text should be scaled (the default)
         -- True if the full content of the view should be scaled. 
 -> IO ()
webViewSetFullContentZoom webview full =
    {#call web_view_set_full_content_zoom#} (toWebView webview) (fromBool full)

-- | Returns the default encoding of the 'WebView'
webViewGetEncoding :: 
    WebViewClass self => self 
 -> IO (Maybe String) -- ^ the default encoding or @Nothing@ in case of failed
webViewGetEncoding webview =
    {#call web_view_get_encoding#} (toWebView webview) >>= maybePeek peekCString

-- | Sets the current 'WebView' encoding, 
-- without modifying the default one, and reloads the page
webViewSetCustomEncoding :: 
    WebViewClass self => self
 -> (Maybe String) -- ^ @encoding@ - the new encoding, 
                   -- or @Nothing@ to restore the default encoding. 
 -> IO ()
webViewSetCustomEncoding webview encoding = 
    maybeWith withCString encoding $ \encodingPtr ->
	{#call web_view_set_custom_encoding#} (toWebView webview) encodingPtr

-- | Returns the current encoding of 'WebView',not the default encoding.
webViewGetCustomEncoding :: 
    WebViewClass self => self 
 -> IO (Maybe String) -- ^ the current encoding string
                      -- or @Nothing@ if there is none set.
webViewGetCustomEncoding webview = 
    {#call web_view_get_custom_encoding#} (toWebView webview) >>= maybePeek peekCString

-- | Determines the current status of the load.
webViewGetLoadStatus :: 
    WebViewClass self => self 
 -> IO LoadStatus -- ^ the current load status:'LoadStatus'
webViewGetLoadStatus webview = 
    liftM (toEnum . fromIntegral) $ {#call web_view_get_load_status#} (toWebView webview)

-- | Determines the current progress of the load
webViewGetProgress :: 
    WebViewClass self => self 
 -> IO Double -- ^ the load progress
webViewGetProgress webview =
    liftM realToFrac $ {#call web_view_get_progress#} (toWebView webview)

-- * Attibutes

-- | Zoom level of the 'WebView' instance
webViewZoomLevel :: WebViewClass self => Attr self Float
webViewZoomLevel = newAttr
   webViewGetZoomLevel
   webViewSetZoomLevel

-- | Whether the full content is scaled when zooming
--
-- Default value: False
webViewFullContentZoom :: WebViewClass self => Attr self Bool
webViewFullContentZoom = newAttr
   webViewGetFullContentZoom
   webViewSetFullContentZoom

-- | The default encoding of the 'WebView' instance
--
-- Default value: @Nothing@
webViewEncoding :: WebViewClass self => ReadAttr self (Maybe String)
webViewEncoding = readAttr webViewGetEncoding

-- | Determines the current status of the load.
--
-- Default value: @LoadFinished@
webViewLoadStatus :: WebViewClass self => ReadAttr self LoadStatus
webViewLoadStatus = readAttr webViewGetLoadStatus

-- |Determines the current progress of the load
--
-- Default Value: 1
webViewProgress :: WebViewClass self => ReadAttr self Double
webViewProgress = readAttr webViewGetProgress


-- | The associated webSettings of the 'WebView' instance
webViewWebSettings :: WebViewClass self => Attr self WebSettings
webViewWebSettings = newAttr
   webViewGetWebSettings
   webViewSetWebSettings


-- | Title of the 'WebView' instance
webViewTitle :: WebViewClass self => ReadAttr self (Maybe String)
webViewTitle = readAttr webViewGetTitle

-- | The associated webInspector instance of the 'WebView'
webViewInspector :: WebViewClass self => ReadAttr self WebInspector
webViewInspector = readAttr webViewGetInspector


-- | The custom encoding of the 'WebView' instance
--
-- Default value: @Nothing@
webViewCustomEncoding :: WebViewClass self => Attr self (Maybe String)
webViewCustomEncoding = newAttr
   webViewGetCustomEncoding
   webViewSetCustomEncoding

-- | view source mode of the 'WebView' instance
webViewViewSourceMode :: WebViewClass self => Attr self Bool
webViewViewSourceMode = newAttr
  webViewGetViewSourceMode
  webViewSetViewSourceMode

-- | transparent background of the 'WebView' instance
webViewTransparent :: WebViewClass self => Attr self Bool
webViewTransparent = newAttr
  webViewGetTransparent
  webViewSetTransparent

-- | Whether content of the 'WebView' can be modified by the user
--
-- Default value: @False@
webViewEditable :: WebViewClass self => Attr self Bool
webViewEditable = newAttr
  webViewGetEditable
  webViewSetEditable

-- * Signals

-- | When Document title changed, this signal is emitted.
--
-- It can be used to set the Application 'Window' title.
--
-- the user function signature is (WebFrame->String->IO())
--
-- webframe - which 'WebFrame' changes the document title.
--
-- title - current title string.
onTitleChanged :: WebViewClass self => self ->( WebFrame -> String -> IO() )-> IO (ConnectId self)
onTitleChanged = 
    connect_OBJECT_STRING__NONE "title-changed" True


-- | When the cursor is over a link, this signal is emitted.
-- 
-- the user function signature is (Maybe String -> Maybe String -> IO () )
-- 
-- title - the link's title or @Nothing@ in case of failure.
--
-- uri - the URI the link points to or @Nothing@ in case of failure.
onHoveringOverLink :: WebViewClass self => self -> (Maybe String -> Maybe String -> IO()) -> IO(ConnectId self)
onHoveringOverLink =
  connect_BOXED_BOXED__NONE "hovering-over-link" readMString True
  where
  readMString strPtr | strPtr==nullPtr = return Nothing
                     | otherwise = liftM Just $ peekUTFString strPtr

-- | When a 'WebFrame' begins to load, this signal is emitted
onLoadStarted :: WebViewClass self => self -> (WebFrame -> IO()) -> IO (ConnectId self)
onLoadStarted = 
    connect_OBJECT__NONE "load-started" True

-- | When a 'WebFrame' loaded the first data, this signal is emitted
onLoadCommitted :: WebViewClass self => self -> (WebFrame -> IO()) -> IO (ConnectId self)
onLoadCommitted = 
    connect_OBJECT__NONE "load-committed" True


-- | When the global progress changed, this signal is emitted
--
-- the global progress will be passed back to user function
onProgressChanged :: WebViewClass self => self -> (Int-> IO()) -> IO (ConnectId self)
onProgressChanged = 
    connect_INT__NONE "load-progress-changed" True

-- | When loading finished, this signal is emitted
onLoadFinished :: WebViewClass self => self -> (WebFrame -> IO()) -> IO (ConnectId self)
onLoadFinished = 
    connect_OBJECT__NONE "load-finished" True

-- | When An error occurred while loading. 
--
-- By default, if the signal is not handled,
-- the WebView will display a stock error page. 
--
-- You need to handle the signal
-- if you want to provide your own error page.
-- 
-- The URI that triggered the error and the 'GError' will be passed back to user function.
onLoadError :: WebViewClass self => self -> (WebFrame -> String -> GError -> IO Bool) -> IO (ConnectId self)
onLoadError = connect_OBJECT_STRING_BOXED__BOOL "load-error" peek True


-- |Emitted when the creation of a new window requested.
-- 
-- * A new 'WebView' instance should be created in the call back or an
--   existing 'WebView' can be returned. In either case, the URL and other
--   details of the returned 'WebView' will be modified once this signal
--   handler returns, thus, you should not modify the 'WebView' in this
--   signal handler. If you need to modify the new 'WebView', you can do so
--   in the 'webViewReady' signal or after the 'webViewReady' signal has
--   been emitted.
--
onCreateWebView :: WebViewClass self => self -> (WebFrame -> IO WebView) -> IO (ConnectId self)
onCreateWebView = \vw act
    connect_OBJECT__PTR "create-web-view" True vw act'
  where
  act' wf = do
    wv <- act wf
    -- We need to return a Ptr WebView to the caller of this signal. To
    -- prevent the garbage collector from freeing the WebView before this
    -- signal returns, we increment the reference count of the object and
    -- decrement it as soon as the 'webViewReady' signal is emitted which
    -- always happens after this signal returns.
    gObjectRef wv
    sidRef <- newIORef undefined
    sid <- onWebViewReady wv $ do
      gObjectUnref wv
      sid <- readIORef sidRef
      signalDisconnect wv sid
      return False
    writeIORef sidRef sid
    return unsafeForeignPtrToPtr (unWebView wv)


-- | Emitted when closing a WebView is requested. 
--
-- This occurs when a call is made from JavaScript's window.close function. 
-- The default signal handler does not do anything. 
-- It is the owner's responsibility to hide or delete the 'WebView', if necessary.
-- 
-- User function should return True to stop the handlers from being invoked for the event 
-- or False to propagate the event furter
onCloseWebView :: WebViewClass self => self -> (IO Bool) -> IO(ConnectId self)
onCloseWebView = 
    connect_NONE__BOOL "close-web-view" True


-- | Emitted after new 'WebView' instance had been created in 'onCreateWebView' user function
-- when the new 'WebView' should be displayed to the user.
-- 
-- All the information about how the window should look, 
-- including size,position,whether the location, status and scroll bars should be displayed, 
-- is ready set.
onWebViewReady:: WebViewClass self => self -> (IO Bool) -> IO(ConnectId self)
onWebViewReady =
    connect_NONE__BOOL "web-view-ready" True
   


-- | Emitted after A new 'Download' is being requested. 
--
-- By default, if the signal is not handled, the download is cancelled.
--  
-- Notice that while handling this signal you must set the target URI using 'downloadSetDestinationUri'
-- 
-- If you intend to handle downloads yourself, return False in user function.
onDownloadRequested :: WebViewClass self => self -> (Download -> IO Bool) -> IO(ConnectId self)
onDownloadRequested =
    connect_OBJECT__BOOL "download-requested" True

-- | Emitted after Icon loaded
onIconLoaded :: WebViewClass self => self -> (IO ()) -> IO(ConnectId self)
onIconLoaded =
    connect_NONE__NONE "icon-loaded" True

-- | The "redo" signal is a keybinding signal which gets emitted to redo the last editing command.
--
-- The default binding for this signal is Ctrl-Shift-z
onRedo :: WebViewClass self => self -> (IO ()) -> IO(ConnectId self)
onRedo =
   connect_NONE__NONE "redo" True

-- | The "undo" signal is a keybinding signal which gets emitted to undo the last editing command.
--
-- The default binding for this signal is Ctrl-z
onUndo :: WebViewClass self => self -> (IO ()) -> IO(ConnectId self)
onUndo =
   connect_NONE__NONE "undo" True 
