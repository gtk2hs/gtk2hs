-- -*-haskell-*-
-----------------------------------------------------------------------------
--  Module      :  Graphics.UI.Gtk.WebKit.Internal
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
-- Access to the WebKit Internal
--
-- This module contain some functions for help binding Webkit.
--
-----------------------------------------------------------------------------

module Graphics.UI.Gtk.WebKit.Internal (
-- * Methods
-- Below functions just help binding use in internal.
-- Don't call those functions in your code.
  connect_OBJECT__OBJECTPTR,
  webViewToWebViewPtr,
) where

import System.Glib.FFI
import System.Glib.GList
import System.Glib.GError 
import Graphics.UI.Gtk.Gdk.Events

{#import Graphics.UI.Gtk.Abstract.Object#}	(makeNewObject)
{#import Graphics.UI.Gtk.Signals#}
{#import Graphics.UI.Gtk.WebKit.Types#}
{#import System.Glib.GObject#}

{#context lib="webkit" prefix ="webkit"#}

-- | Signal helper functions.
connect_OBJECT__OBJECTPTR ::
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

-- | Internal helper function for convert.
webViewToWebViewPtr :: WebViewClass self => self -> IO (Ptr WebView)
webViewToWebViewPtr webview = return $ unsafeForeignPtrToPtr (unWebView (toWebView webview))
