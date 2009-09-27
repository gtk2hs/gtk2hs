-- -*-haskell-*-
-----------------------------------------------------------------------------
--  Module      :  Graphics.UI.Gtk.WebKit.WebInspector
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
-- Access to the WebKit Inspector
-----------------------------------------------------------------------------

module Graphics.UI.Gtk.WebKit.WebInspector (
-- * Types
  WebInspector,

-- * Methods
  webInspectorGetInspectedUri,
  webInspectorGetWebView,

-- * Signals
  onAttachWindow,
  onCloseWindow,
  onDetachWindow,
  onFinished,
  onInspectWebView,
  onShowWindow,
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
-- |  Obtains the URI that is currently being inspected
webInspectorGetInspectedUri :: 
    WebInspectorClass self => self
 -> IO String
webInspectorGetInspectedUri inspector = 
    {#call web_inspector_get_inspected_uri#} (toWebInspector inspector) >>= peekCString

-- | Obtains the 'WebView' that is used to render the 'WebInspector'. 
-- 
-- The 'WebView' instance is created by the application,
-- by handling the "inspect-web-view" signal. 
-- This means that it may return @Nothing@ if the user hasn't inspected anything
webInspectorGetWebView :: 
    WebInspectorClass self => self
 -> IO (Maybe WebView)
webInspectorGetWebView inspector =
    maybeNull (makeNewObject mkWebView) $ liftM castPtr $
      {#call web_inspector_get_web_view#}
        (toWebInspector inspector)


-- * Signals
-- | Emitted when the 'WebInspector' should appear at the same window as the 'WebView' being inspected.
--
-- return True if the signal is handled.
onAttachWindow ::  WebInspectorClass self => self ->( IO Bool )-> IO (ConnectId self)
onAttachWindow =
    connect_NONE__BOOL "attach-window" True

-- | Emitted when the inspector window should be closed. 
--
-- return True if the signal is handled.
--
-- You can destroy the window or hide it so that it can be displayed again by handling 'onShowWindow' later on. 
-- Notice that the inspected 'WebView' may no longer exist when this signal is emitted.
-- Notice, too, that if you decide to destroy the window, 
-- 'onInspectWebView' will be emmited again, when the user inspects an element.

onCloseWindow :: WebInspectorClass self => self ->( IO Bool )-> IO (ConnectId self)
onCloseWindow =
    connect_NONE__BOOL "close-window" True

-- | Emitted when the inspector should appear in a separate window
--
-- return True if the signal is handled
onDetachWindow :: WebInspectorClass self => self ->( IO Bool )-> IO (ConnectId self)
onDetachWindow =
    connect_NONE__BOOL "detach-window" True

-- | Emitted when the inspection is done.
onFinished :: WebInspectorClass self => self ->( IO () )-> IO (ConnectId self)
onFinished =
    connect_NONE__NONE "finished" True

-- internal helper function used in onInspectWebView
webViewToWebViewPtr :: WebViewClass self => self -> IO (Ptr WebView)
webViewToWebViewPtr webview = do
    let webViewPtr = unsafeForeignPtrToPtr (unWebView (toWebView webview))
    -- objectRef webViewPtr
    return webViewPtr

-- | Emitted when the user activates the \'inspect\' context menu item to inspect a 'WebView'. 
--
-- The application which is interested in the inspector should create a window, 
-- or otherwise add the 'WebView' it creates to an existing window.
-- 
-- !!!NOTE!!!, you should return a 'WebView' in user function 
onInspectWebView :: WebInspectorClass self => self ->( WebView -> IO WebView )-> IO (ConnectId self)
onInspectWebView =
    connect_OBJECT__OBJECTPTR "inspect-web-view" True
    where connect_OBJECT__OBJECTPTR ::
              (GObjectClass a', GObjectClass obj) => SignalName ->
              ConnectAfter -> obj ->
              (a' -> IO WebView) ->
              IO (ConnectId obj)
          connect_OBJECT__OBJECTPTR signal after obj user =
            connectGeneric signal after obj action
              where action :: Ptr GObject -> Ptr GObject -> IO (Ptr WebView)
                    action _ obj1  =
                        failOnGError $
                        makeNewGObject mkGObject (return obj1) >>= \obj1' ->
                        user (unsafeCastGObject obj1') >>= webViewToWebViewPtr

-- | Emitted when the inspector window should be displayed. 
--
-- Notice that the window must have been created already by handling "inspect-web-view".
onShowWindow :: WebInspectorClass self => self ->( IO Bool )-> IO (ConnectId self)
onShowWindow =
    connect_NONE__BOOL "show-window" True

