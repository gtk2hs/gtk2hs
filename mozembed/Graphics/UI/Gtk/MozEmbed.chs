-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget embedding the Mozilla browser engine (Gecko)
--
--  Author : Jonas Svensson
--
--  Created: 26 February 2002
--
--  Version $Revision: 1.2 $ from $Date: 2004/12/18 20:45:50 $
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
-- | This widgets embeds Mozilla's browser engine (Gecko) into a GTK+ widget.
-- See <http://www.mozilla.org/unix/gtk-embedding.html> for an API reference.
--
module Graphics.UI.Gtk.MozEmbed (
  MozEmbed, MozEmbedClass,

  mozEmbedNew, mozEmbedSetCompPath,

  mozEmbedRenderData,
  mozEmbedOpenStream, mozEmbedAppendData, mozEmbedCloseStream,

  onOpenURI,

  mozEmbedLoadUrl,

  -- the functions below are untested.

  onKeyDown, onKeyPress, onKeyUp,
  onMouseDown, onMouseUp, onMouseClick, onMouseDoubleClick,
  onMouseOver, onMouseOut,

  mozEmbedSetProfilePath,
  mozEmbedStopLoad, mozEmbedGoBack, mozEmbedGoForward, mozEmbedGetLinkMessage,
  mozEmbedGetJsStatus, mozEmbedGetTitle, mozEmbedGetLocation,
  mozEmbedCanGoBack, mozEmbedCanGoForward, mozEmbedPushStartup,
  mozEmbedPopStartup
) where

import Monad		(liftM)
import FFI
import ForeignPtr
import Foreign.Marshal.Utils (toBool)

{#import Object#}    (makeNewObject)
{#import Signal#}    (ConnectId, connect_STRING__BOOL, connect_PTR__INT)
{#import Graphics.UI.Gtk.MozEmbedType #}
import Widget (Widget)

{#context lib="gtkembedmoz" prefix ="gtk"#}

-- operations
-- ----------

-- | Create a new MozEmbed
--
mozEmbedNew :: IO MozEmbed
mozEmbedNew = makeNewObject mkMozEmbed $ liftM castPtr {#call moz_embed_new#}

mozEmbedSetCompPath :: String -> IO ()
mozEmbedSetCompPath str =
  withCString str $ \strPtr ->
   {#call moz_embed_set_comp_path#}
   strPtr

mozEmbedSetProfilePath :: String -> String -> IO ()
mozEmbedSetProfilePath dir name =
  withCString dir $ \dirPtr ->
   withCString name $ \namePtr ->
    {#call moz_embed_set_profile_path#} dirPtr namePtr
  
mozEmbedLoadUrl :: MozEmbedClass m => m -> String -> IO ()
mozEmbedLoadUrl m url =
  withCString url $ \urlPtr ->
   {#call moz_embed_load_url#}
   (toMozEmbed m)
   urlPtr

mozEmbedStopLoad :: MozEmbedClass m => m -> IO ()
mozEmbedStopLoad m = 
  {#call moz_embed_stop_load#} (toMozEmbed m)

mozEmbedGoBack :: MozEmbedClass m => m -> IO ()
mozEmbedGoBack m =
  {#call moz_embed_go_back#} (toMozEmbed m)

mozEmbedGoForward :: MozEmbedClass m => m -> IO ()
mozEmbedGoForward m =
  {#call moz_embed_go_forward#} (toMozEmbed m)

mozEmbedGetLinkMessage :: MozEmbedClass m => m -> IO String
mozEmbedGetLinkMessage m = 
  do
   str <- {#call moz_embed_get_link_message#} (toMozEmbed m)
   peekCString str

mozEmbedGetJsStatus :: MozEmbedClass m => m -> IO String
mozEmbedGetJsStatus m =
  do
   str <- {#call moz_embed_get_js_status#} (toMozEmbed m)
   peekCString str

mozEmbedGetTitle :: MozEmbedClass m => m -> IO String
mozEmbedGetTitle m = 
  do
   str <- {#call moz_embed_get_title#} (toMozEmbed m)
   peekCString str

mozEmbedGetLocation :: MozEmbedClass m => m -> IO String
mozEmbedGetLocation m = 
  do
   str <- {#call moz_embed_get_location#} (toMozEmbed m)
   peekCString str

mozEmbedCanGoBack :: MozEmbedClass m => m -> IO Bool
mozEmbedCanGoBack m =
  liftM toBool $ 
   {#call moz_embed_can_go_back#} (toMozEmbed m)

mozEmbedCanGoForward :: MozEmbedClass m => m -> IO Bool
mozEmbedCanGoForward m =
  liftM toBool $ 
   {#call moz_embed_can_go_forward#} (toMozEmbed m)

mozEmbedPushStartup :: IO ()
mozEmbedPushStartup =
  {#call moz_embed_push_startup#}

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

mozEmbedOpenStream :: MozEmbedClass m => m -> String -> String -> IO ()
mozEmbedOpenStream m baseURI mimeType =
  withCString baseURI  $ \ basePtr ->
  withCString mimeType $ \ mtPtr ->
  {#call gtk_moz_embed_open_stream#} (toMozEmbed m) basePtr mtPtr

mozEmbedAppendDataInternal :: MozEmbedClass m => m -> String -> IO ()
mozEmbedAppendDataInternal m contents =
--  newCStringLen (toUTF contents) >>= \ (dataPtr,len) ->
  withUTFStringLen contents $ \ (dataPtr,len) ->      -- alloca discouraged?
  let len' = fromIntegral len in
  {#call gtk_moz_embed_append_data#} (toMozEmbed m) dataPtr len'
--  >> free dataPtr

mozEmbedCloseStream :: MozEmbedClass m => m -> IO ()
mozEmbedCloseStream m =
  {#call gtk_moz_embed_close_stream#} (toMozEmbed m)

mozEmbedAppendData :: MozEmbedClass m => m -> String -> IO ()
mozEmbedAppendData m contents =
  mapM_ (mozEmbedAppendDataInternal m) (chunks 32768 contents)

mozEmbedRenderData :: MozEmbedClass m => m -> String -> String -> String -> IO ()
mozEmbedRenderData m contents baseURI mimeType = do
  mozEmbedOpenStream m baseURI mimeType
  mozEmbedAppendData m contents
  mozEmbedCloseStream m


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
--  -- mozEmbedRenderDataInternal does not work for len' > 2^16
mozEmbedRenderDataInternal :: MozEmbedClass m => m -> String -> String -> String -> IO ()
mozEmbedRenderDataInternal m contents baseURI mimeType =
--  newCStringLen (toUTF contents) >>= \ (dataPtr,len) ->
  withUTFStringLen contents $ \ (dataPtr,len) ->      -- alloca discouraged
  let len' = fromIntegral len in
--  hPutStrLn stderr ("mozEmbedRenderData: " ++ shows len' " bytes") >>= \ _ ->
  withCString baseURI  $ \ basePrt ->
  withCString mimeType $ \ mtPtr ->
  {#call gtk_moz_embed_render_data#} (toMozEmbed m) dataPtr len' basePrt mtPtr
--  >> free dataPtr

{-
struct _GtkMozEmbedClass
{
  [...]
  gint (* open_uri)            (GtkMozEmbed *embed, const char *aURI);
  [...]
}
-}

onOpenURI :: MozEmbedClass m => m -> (String -> IO Bool) -> IO (ConnectId m)
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
 :: (Num n, Integral n, MozEmbedClass m)
 => m -> (Ptr a -> IO n) -> IO (ConnectId m)
onKeyDown          = connect_PTR__INT "dom_key_down" False
onKeyPress         = connect_PTR__INT "dom_key_press" False
onKeyUp            = connect_PTR__INT "dom_key_up" False
onMouseDown        = connect_PTR__INT "dom_mouse_down" False
onMouseUp          = connect_PTR__INT "dom_mouse_up" False
onMouseClick       = connect_PTR__INT "dom_mouse_click" False
onMouseDoubleClick = connect_PTR__INT "dom_mouse_dbl_click" False
onMouseOver        = connect_PTR__INT "dom_mouse_over" False
onMouseOut         = connect_PTR__INT "dom_mouse_out" False

