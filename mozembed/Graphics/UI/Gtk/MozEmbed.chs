-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget embedding the Mozilla browser engine (Gecko)
--
--  Author : Jonas Svensson
--
--  Created: 26 February 2002
--
--  Version $Revision: 1.6 $ from $Date: 2005/10/22 16:10:09 $
--
--  Copyright (c) 2002 Jonas Svensson
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Library General Public
--  License as published by the Free Software Foundation; either
--  version 2 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Library General Public License for more details.
--
--  Modified 2004 by Scott West for basic use in gtk2hs
--
--  Further modified 2004 by Wolfram Kahl:
--  * ported to gtk2hs/c2hs
--  * added additional interface functions
--  * circumvented render_data problem
--
-- | This widgets embeds Mozilla's browser engine (Gecko) into a Gtk+ widget.
--
-- See <http://www.mozilla.org/unix/gtk-embedding.html> for a more detailed API
-- reference.
--
module Graphics.UI.Gtk.MozEmbed (
-- * Types
  MozEmbed,

-- * Constructors
  mozEmbedNew,
  mozEmbedSetCompPath,
  mozEmbedSetProfilePath,
  mozEmbedPushStartup,
  mozEmbedPopStartup,

-- * Methods
  mozEmbedLoadUrl,
  mozEmbedStopLoad,
  
  mozEmbedRenderData,
  mozEmbedOpenStream,
  mozEmbedAppendData,
  mozEmbedCloseStream,

  mozEmbedGoBack,
  mozEmbedGoForward,
  mozEmbedCanGoBack,
  mozEmbedCanGoForward,
  mozEmbedGetTitle,
  mozEmbedGetLocation,
  mozEmbedGetLinkMessage,
  mozEmbedGetJsStatus,

-- * Signals
  onOpenURI,
  onKeyDown,
  onKeyPress,
  onKeyUp,
  onMouseDown,
  onMouseUp,
  onMouseClick,
  onMouseDoubleClick,
  onMouseOver,
  onMouseOut,
) where

import Monad		(liftM)

import System.Glib.FFI
import System.Glib.UTFString
{#import Graphics.UI.Gtk.Abstract.Object#}	(makeNewObject)
{#import Graphics.UI.Gtk.Signals#}		(ConnectId,
						connect_STRING__BOOL,
						connect_PTR__INT)
{#import Graphics.UI.Gtk.MozEmbed.Types#}
import Graphics.UI.Gtk.Abstract.Widget		(Widget)

{#context lib="gtkembedmoz" prefix ="gtk"#}

-- operations
-- ----------

-- | Create a new MozEmbed.
--
-- Note that the mozembed system must be initialised first using
-- 'mozEmbedSetCompPath'.
--
mozEmbedNew :: IO MozEmbed
mozEmbedNew = makeNewObject mkMozEmbed $ liftM castPtr {#call moz_embed_new#}

-- | This function must be called before the first widget is created.
--
-- It allows you to set the path to the mozilla components, however unless
-- you really know what you are doing, you should just use:
--
-- > mozEmbedSetCompPath ""
--
mozEmbedSetCompPath :: String -> IO ()
mozEmbedSetCompPath str =
  withCString str $ \strPtr ->
   {#call moz_embed_set_comp_path#}
   strPtr

mozEmbedSetProfilePath ::
    FilePath -- ^ profile directory
 -> String   -- ^ profile name
 -> IO ()
mozEmbedSetProfilePath dir name =
  withCString dir $ \dirPtr ->
  withCString name $ \namePtr ->
  {#call moz_embed_set_profile_path#} dirPtr namePtr

-- | This function starts loading a url in the embedding widget. All loads are
-- asynchronous. The url argument should be in the form of
-- @\"http:\/\/www.haskell.org\"@.
--
mozEmbedLoadUrl :: MozEmbed -> String -> IO ()
mozEmbedLoadUrl moz url =
  withCString url $ \urlPtr ->
   {#call moz_embed_load_url#}
   moz
   urlPtr

-- | This function will allow you to stop the load of a document that is being
-- loaded in the widget.
--
mozEmbedStopLoad :: MozEmbed -> IO ()
mozEmbedStopLoad moz = 
  {#call moz_embed_stop_load#} moz

-- | This function will go backwards one step in the document's navigation
-- history.
--
mozEmbedGoBack :: MozEmbed -> IO ()
mozEmbedGoBack moz =
  {#call moz_embed_go_back#} moz

-- | This function will go forward one step in the document's navigation
-- history.
--
mozEmbedGoForward :: MozEmbed -> IO ()
mozEmbedGoForward moz =
  {#call moz_embed_go_forward#} moz

-- | This function returns the current link message of the document if there is
-- one.
--
mozEmbedGetLinkMessage :: MozEmbed -> IO String
mozEmbedGetLinkMessage moz = do
  str <- {#call moz_embed_get_link_message#} moz
  readCString str

-- | This function returns the \"js_status\" message if there is one.
--
mozEmbedGetJsStatus :: MozEmbed -> IO String
mozEmbedGetJsStatus moz = do
  str <- {#call moz_embed_get_js_status#} moz
  readCString str

-- | This function will get the current title for a document.
--
mozEmbedGetTitle :: MozEmbed -> IO String
mozEmbedGetTitle moz = do
  str <- {#call moz_embed_get_title#} moz
  readCString str

-- |  This function will return the current location of the document.
--
mozEmbedGetLocation :: MozEmbed -> IO String
mozEmbedGetLocation moz = do
  str <- {#call moz_embed_get_location#} moz
  readCString str

-- | This function will return whether or not you can go backwards in the
-- document's navigation history. It will return @True@ if it can go backwards,
-- @False@ if it can't.
-- 
mozEmbedCanGoBack :: MozEmbed -> IO Bool
mozEmbedCanGoBack moz =
  liftM toBool $ 
  {#call moz_embed_can_go_back#} moz

-- | This function will return whether or not you can go forwards in the
-- document's navigation history. It will return @True@ if it can go forwards,
-- @False@ if it can't.
--
mozEmbedCanGoForward :: MozEmbed -> IO Bool
mozEmbedCanGoForward moz =
  liftM toBool $ 
  {#call moz_embed_can_go_forward#} moz

-- | Sadly undocumented
--
mozEmbedPushStartup :: IO ()
mozEmbedPushStartup =
  {#call moz_embed_push_startup#}

-- | Sadly undocumented
--
mozEmbedPopStartup :: IO ()
mozEmbedPopStartup =
  {#call moz_embed_pop_startup#}

{-
void         gtk_moz_embed_open_stream      (GtkMozEmbed *embed,
					     const char *base_uri,
					     const char *mime_type);
void         gtk_moz_embed_append_data      (GtkMozEmbed *embed,
					     const char *data, guint32 len);
void         gtk_moz_embed_close_stream     (GtkMozEmbed *embed);
-}

-- | This function is used to start loading a document from an external source
-- into the embedding widget. You need to pass in the base URI for resolving
-- internal links and and the mime type of the document.
--
mozEmbedOpenStream ::
    MozEmbed
 -> String -- ^ base URL
 -> String -- ^ mime type
 -> IO ()
mozEmbedOpenStream moz baseURI mimeType =
  withCString baseURI  $ \ basePtr ->
  withCString mimeType $ \ mtPtr ->
  {#call gtk_moz_embed_open_stream#} moz basePtr mtPtr

mozEmbedAppendDataInternal :: MozEmbed -> String -> IO ()
mozEmbedAppendDataInternal moz contents =
  withUTFStringLen contents $ \(dataPtr,len) ->
  {#call gtk_moz_embed_append_data#} moz dataPtr (fromIntegral len)

-- | This function closes the stream that you have been using to append data
-- manually to the embedding widget.
--
mozEmbedCloseStream :: MozEmbed -> IO ()
mozEmbedCloseStream moz =
  {#call gtk_moz_embed_close_stream#} moz

-- | This function allows you to append data to an already opened stream in the
-- widget. You need to pass in the data that you want to append to the document
-- and its length.
--
mozEmbedAppendData :: MozEmbed -> String -> IO ()
mozEmbedAppendData moz contents =
  mapM_ (mozEmbedAppendDataInternal moz) (chunks 32768 contents)

-- |  This function will allow you to take a chunk of random data and render it
-- into the document. You need to pass in the data and the length of the data.
-- The base URI is used to resolve internal references in the document and the
-- mime type is used to determine how to render the document internally.
--
mozEmbedRenderData :: 
    MozEmbed
 -> String -- ^ content
 -> String -- ^ base URI
 -> String -- ^ mime type
 -> IO ()
mozEmbedRenderData moz contents baseURI mimeType = do
  mozEmbedOpenStream moz baseURI mimeType
  mozEmbedAppendData moz contents
  mozEmbedCloseStream moz


chunks :: Int -> [a] -> [[a]]
chunks n [] = []
chunks n xs = let (ys, zs) = splitAt n xs in ys : chunks n zs

{-
void         gtk_moz_embed_render_data      (GtkMozEmbed *embed, 
                                             const char *data,
                                             guint32 len,
                                             const char *base_uri, 
                                             const char *mime_type)
-}

-- mozEmbedRenderDataInternal does not work for len' > 2^16
mozEmbedRenderDataInternal :: MozEmbed -> String -> String -> String -> IO ()
mozEmbedRenderDataInternal moz contents baseURI mimeType =
  withUTFStringLen contents $ \ (dataPtr,len) ->      -- alloca discouraged
  withCString baseURI  $ \ basePrt ->
  withCString mimeType $ \ mtPtr ->
  {#call gtk_moz_embed_render_data#} moz dataPtr (fromIntegral len) basePrt mtPtr

{-
struct _GtkMozEmbedClass
{
  [...]
  gint (* open_uri)            (GtkMozEmbed *embed, const char *aURI);
  [...]
}
-}

onOpenURI :: MozEmbed -> (String -> IO Bool) -> IO (ConnectId MozEmbed)
onOpenURI = connect_STRING__BOOL "open_uri" after
 where
-- Specify if the handler is to run before (False) or after (True) the
-- default handler.
  after = False


{-
More signals to investigate:

  gint (* dom_key_down)        (GtkMozEmbed *embed, gpointer dom_event);
  gint (* dom_key_press)       (GtkMozEmbed *embed, gpointer dom_event);
  gint (* dom_key_up)          (GtkMozEmbed *embed, gpointer dom_event);
  gint (* dom_mouse_down)      (GtkMozEmbed *embed, gpointer dom_event);
  gint (* dom_mouse_up)        (GtkMozEmbed *embed, gpointer dom_event);
  gint (* dom_mouse_click)     (GtkMozEmbed *embed, gpointer dom_event);
  gint (* dom_mouse_dbl_click) (GtkMozEmbed *embed, gpointer dom_event);
  gint (* dom_mouse_over)      (GtkMozEmbed *embed, gpointer dom_event);
  gint (* dom_mouse_out)       (GtkMozEmbed *embed, gpointer dom_event);

Unfortunateley these are not documented on

http://www.mozilla.org/unix/gtk-embedding.html

-}

onKeyDown, onKeyPress, onKeyUp,
 onMouseDown, onMouseUp, onMouseClick, onMouseDoubleClick,
 onMouseOver, onMouseOut
 :: MozEmbed
 -> (Ptr a -> IO Int)
 -> IO (ConnectId MozEmbed)
onKeyDown          = connect_PTR__INT "dom_key_down" False
onKeyPress         = connect_PTR__INT "dom_key_press" False
onKeyUp            = connect_PTR__INT "dom_key_up" False
onMouseDown        = connect_PTR__INT "dom_mouse_down" False
onMouseUp          = connect_PTR__INT "dom_mouse_up" False
onMouseClick       = connect_PTR__INT "dom_mouse_click" False
onMouseDoubleClick = connect_PTR__INT "dom_mouse_dbl_click" False
onMouseOver        = connect_PTR__INT "dom_mouse_over" False
onMouseOut         = connect_PTR__INT "dom_mouse_out" False
