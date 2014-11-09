{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Clipboard
--
--  Author : Axel Simon
--
--  Created: 26 March 2007
--
--  Copyright (C) 2007 Axel Simon
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
-- I removed all definitions for the clipboard by Juergen Nicklisch since
-- the way the clipboards were selected didn't tie in with the Selection
-- module.
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Storing data on clipboards
--
module Graphics.UI.Gtk.General.Clipboard (

-- * Detail
--
-- | The 'Clipboard' object represents a clipboard of data shared between
-- different processes or between different widgets in the same process. Each
-- clipboard is identified by a 'SelectionTag' which itself is an 'Atom'. The
-- default clipboard corresponds to the 'selectionClipboard' tag; another
-- commonly used clipboard is the 'selectionPrimary' tag, which, in X,
-- traditionally contains the currently selected text.
--
-- To support having a number of different formats on the clipboard at the
-- same time, the clipboard mechanism allows providing callbacks instead of
-- the actual data. When you set the contents of the clipboard, you can either
-- supply the data directly (via functions like 'clipboardSetText'), or you
-- can supply a callback to be called at a later time when the data is needed
-- (via 'clipboardSetWithData'). Providing a callback also avoids having to
-- make copies of the data when it is not needed.
--
-- Setting clipboard data is done using 'clipboardSetWithData' and
-- 'clipboardSetWithOwner'. Both functions are quite similar; the choice
-- between the two depends mostly on which is more convenient in a particular
-- situation. The former is most useful when you want to have a blob of data
-- with callbacks to convert it into the various data types that you
-- advertise. When the @clearFunc@ you provided is called, you simply free the
-- data blob. The latter is more useful when the contents of clipboard reflect
-- the internal state of a 'GObject' (As an example, for the
-- 'selectionPrimary' clipboard, when an entry widget provides the clipboard's
-- contents the contents are simply the text within the selected region.) If
-- the contents change, the entry widget can call 'clipboardSetWithOwner' to
-- update the timestamp for clipboard ownership, without having to worry about
-- @clearFunc@ being called.
--
-- Requesting the data from the clipboard is essentially asynchronous. If the
-- contents of the clipboard are provided within the same process, then a
-- direct function call will be made to retrieve the data, but if they are
-- provided by another process, then the data needs to be retrieved from the
-- other process, which may take some time. To avoid blocking the user
-- interface, the call to request the selection, 'clipboardRequestContents'
-- takes a callback that will be called when the contents are received (or
-- when the request fails.) If you don't want to deal with providing a
-- separate callback, you can also use 'clipboardWaitForContents'. What this
-- does is run the GLib main loop recursively waiting for the contents. This
-- can simplify the code flow, but you still have to be aware that other
-- callbacks in your program can be called while this recursive mainloop is
-- running.
--
-- Along with the functions to get the clipboard contents as an arbitrary data
-- chunk, there are also functions to retrieve it as text,
-- 'clipboardRequestText' and 'clipboardWaitForText'. These functions take
-- care of determining which formats are advertised by the clipboard provider,
-- asking for the clipboard in the best available format and converting the
-- its content.

-- * Class Hierarchy
--
-- |
-- @
-- |  'GObject'
-- |   +----Clipboard
-- @

-- * Types
  Clipboard,
  ClipboardClass,
  castToClipboard, gTypeClipboard,
  toClipboard,

-- * Constants
  selectionPrimary,
  selectionSecondary,
  selectionClipboard,

-- * Methods
  clipboardGet,
#if GTK_CHECK_VERSION(2,2,0)
  clipboardGetForDisplay,
  clipboardGetDisplay,
#endif
  clipboardSetWithData,
{-
  clipboardSetWithOwner,
  clipboardGetOwner,
  clipboardClear,
-}
  clipboardSetText,
#if GTK_CHECK_VERSION(2,6,0)
  clipboardSetImage,
#endif
  clipboardRequestContents,
  clipboardRequestText,
#if GTK_CHECK_VERSION(2,6,0)
  clipboardRequestImage,
#endif
#if GTK_CHECK_VERSION(2,4,0)
  clipboardRequestTargets,
#if GTK_CHECK_VERSION(2,10,0)
  clipboardRequestRichText,
#endif
#endif
#if GTK_CHECK_VERSION(2,6,0)
  clipboardSetCanStore,
  clipboardStore,
#endif
  ) where

import System.Glib.FFI
import System.Glib.UTFString
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.General.DNDTypes#} (SelectionTag, TargetTag,
  Atom(..))
{#import Graphics.UI.Gtk.General.Selection#} (InfoId, SelectionDataM)
import Graphics.UI.Gtk.General.Structs (
  selectionPrimary,
  selectionSecondary,
  selectionClipboard,
  withTargetEntries)
import Control.Monad ( liftM )
import Control.Monad.Reader (runReaderT)
import Data.IORef ( newIORef, readIORef, writeIORef )

{# context lib="gtk" prefix="gtk" #}


--------------------
-- Methods

-- %hash c:d8d1 d:febf
-- | Returns the clipboard object for the given selection. See
-- 'clipboardGetForDisplay' for complete details.
--
clipboardGet ::
  SelectionTag  -- ^ @selection@ - a 'SelectionTag' which
                 -- identifies the clipboard to use.
 -> IO Clipboard -- ^ returns the appropriate clipboard object. If no
                 -- clipboard already exists, a new one will be created. Once a
                 -- clipboard object has been created, it is persistent.
clipboardGet (Atom selection) =
  makeNewGObject mkClipboard $
  {# call gtk_clipboard_get #} selection

#if GTK_CHECK_VERSION(2,2,0)
-- %hash c:251 d:39fa
-- | Returns the clipboard object for the given selection. Cut\/copy\/paste
-- menu items and keyboard shortcuts should use the default clipboard,
-- returned by passing 'selectionClipboard' for @selection@. The
-- currently-selected object or text should be provided on the clipboard
-- identified by 'selectionPrimary'. Cut\/copy\/paste menu items conceptually
-- copy the contents of the 'selectionPrimary' clipboard to the default
-- clipboard, i.e. they copy the selection to what the user sees as the
-- clipboard.
--
-- See
-- <http://www.freedesktop.org/Standards/clipboards-spec> for a detailed
-- discussion of the 'selectionClipboard' vs. 'selectionPrimary' selections
-- under the X window system. On Win32 the 'selectionPrimary' clipboard is
-- essentially ignored.
--
-- It's possible to have arbitrary named clipboards; if you do invent new
-- clipboards, you should prefix the selection name with an underscore
-- (because the ICCCM requires that nonstandard atoms are
-- underscore-prefixed), and namespace it as well. For example, if your
-- application called \"Foo\" has a special-purpose clipboard, you could
-- create it using 'Graphics.UI.Gtk.General.Selection.atomNew'
-- \"_FOO_SPECIAL_CLIPBOARD\".
--
-- * Available since Gtk+ version 2.2
--
clipboardGetForDisplay ::
    Display      -- ^ @display@ - the display for which the clipboard is to be
                 -- retrieved or created
 -> SelectionTag -- ^ @selection@ - a 'SelectionTag' which
                 -- identifies the clipboard to use.
 -> IO Clipboard -- ^ returns the appropriate clipboard object. If no
                 -- clipboard already exists, a new one will be created. Once a
                 -- clipboard object has been created, it is persistent.
clipboardGetForDisplay display (Atom selection) =
  makeNewGObject mkClipboard $
  {# call gtk_clipboard_get_for_display #}
    display selection

-- %hash c:3931 d:93f1
-- | Gets the 'Display' associated with @clipboard@
--
-- * Available since Gtk+ version 2.2
--
clipboardGetDisplay :: ClipboardClass self => self
 -> IO Display -- ^ returns the 'Display' associated with @clipboard@
clipboardGetDisplay self =
  makeNewGObject mkDisplay $
  {# call gtk_clipboard_get_display #}
    (toClipboard self)
#endif

-- The memory management of the ClipboardGetFunc and ClipboardClearFunc sucks badly
-- in that there is no consistent way in which the latter could free the function
-- closure of the former, since it is *not* called when the data of the same
-- object is changed. What we do is that we store the function pointers as attributes
-- of the Clipboard. Overwriting or finalizing these attributes will call their
-- destructors and thereby free them. Thus, by setting these attributes each time we
-- install new data functions, we cuningly finalized the previous closures. Hooray.

{-# NOINLINE getFuncQuark #-}
getFuncQuark :: Quark
getFuncQuark = unsafePerformIO $ quarkFromString ("hsClipboardGetFuncClosure"::DefaultGlibString)

{-# NOINLINE clearFuncQuark #-}
clearFuncQuark :: Quark
clearFuncQuark = unsafePerformIO $ quarkFromString ("hsClipboardClearFuncClosure"::DefaultGlibString)

-- %hash c:c65a d:b402
-- | Virtually sets the contents of the specified clipboard by providing a
-- list of supported formats for the clipboard data and a function to call to
-- get the actual data when it is requested.
--
clipboardSetWithData :: ClipboardClass self => self
 -> [(TargetTag, InfoId)]     -- ^ @targets@ - a list containing information
                              -- about the available forms for the clipboard
                              -- data
 -> (InfoId -> SelectionDataM ())
                              -- ^ @getFunc@ - function to call to get the
                              -- actual clipboard data, should call
                              -- 'selectionDataSet'.
 -> IO ()                     -- ^ @clearFunc@ - when the clipboard contents
                              -- are set again, this function will be called,
                              -- and @getFunc@ will not be subsequently called.
 -> IO Bool                   -- ^ returns @True@ if setting the clipboard
                              -- data succeeded.
clipboardSetWithData self targets getFunc clearFunc = do
  gFunPtr <- mkClipboardGetFunc
    (\_ sPtr info _ -> runReaderT (getFunc info) sPtr >> return ())
  cFunPtr <- mkClipboardClearFunc
    (\_ _ -> clearFunc)
  res <- withTargetEntries targets $ \nTargets targets ->
    liftM toBool $
    {# call gtk_clipboard_set_with_data #}
      (toClipboard self)
      targets
      (fromIntegral nTargets)
      gFunPtr
      cFunPtr
      nullPtr
  {#call unsafe g_object_set_qdata_full#} (toGObject self) getFuncQuark
     (castFunPtrToPtr gFunPtr) destroyFunPtr
  {#call unsafe g_object_set_qdata_full#} (toGObject self) clearFuncQuark
     (castFunPtrToPtr cFunPtr) destroyFunPtr
  return res

{#pointer ClipboardGetFunc#}
{#pointer ClipboardClearFunc#}

foreign import ccall "wrapper" mkClipboardGetFunc ::
  (Ptr Clipboard -> Ptr () -> {#type guint#} -> Ptr () -> IO ()) -> IO ClipboardGetFunc

foreign import ccall "wrapper" mkClipboardClearFunc ::
  (Ptr Clipboard -> Ptr () -> IO ()) -> IO ClipboardClearFunc

-- %hash c:e778 d:7b3f
-- | Virtually sets the contents of the specified clipboard by providing a
-- list of supported formats for the clipboard data and a function to call to
-- get the actual data when it is requested.
--
-- The difference between this function and 'clipboardSetWithData' is that
-- a 'GObject' is passed in.
--
_clipboardSetWithOwner :: (ClipboardClass self, GObjectClass owner) => self
 -> [(TargetTag, InfoId)]     -- ^ @targets@ - a list containing information
                              -- about the available forms for the clipboard
                              -- data
 -> (InfoId -> SelectionDataM ())
                              -- ^ @getFunc@ - function to call to get the
                              -- actual clipboard data, should call
                              -- 'selectionDataSet'.
 -> IO ()                     -- ^ @clearFunc@ - when the clipboard contents
                              -- are set again, this function will be called,
                              -- and @getFunc@ will not be subsequently called.
 -> owner                     -- ^ @owner@ - an object that \"owns\" the data.
 -> IO Bool                   -- ^ returns @True@ if setting the clipboard
                              -- data succeeded. If setting the clipboard data
                              -- failed the provided callback functions will be
                              -- ignored.
_clipboardSetWithOwner self targets getFunc clearFunc owner = do
  gFunPtr <- mkClipboardGetFunc
    (\_ sPtr info _ -> runReaderT (getFunc info) sPtr >> return ())
  cFunPtr <- mkClipboardClearFunc
    (\_ _ -> clearFunc)
  res <- withTargetEntries targets $ \nTargets targets ->
    liftM toBool $
    {# call gtk_clipboard_set_with_owner #}
      (toClipboard self)
      targets
      (fromIntegral nTargets)
      gFunPtr
      cFunPtr
      (toGObject owner)
  {#call unsafe g_object_set_qdata_full#} (toGObject self) getFuncQuark
     (castFunPtrToPtr gFunPtr) destroyFunPtr
  {#call unsafe g_object_set_qdata_full#} (toGObject self) clearFuncQuark
     (castFunPtrToPtr cFunPtr) destroyFunPtr
  return res

-- %hash c:dba2 d:efc2
-- | If the clipboard contents callbacks were set with
-- 'clipboardSetWithOwner', and the 'clipboardSetWithData' or 'clipboardClear'
-- has not subsequently called, returns the owner set by
-- 'clipboardSetWithOwner'.
--
_clipboardGetOwner :: ClipboardClass self => self
 -> IO (Maybe GObject) -- ^ returns the owner of the clipboard, if any; otherwise
                        -- @Nothing@.
_clipboardGetOwner self =
  maybeNull (makeNewGObject mkGObject) $
  {# call gtk_clipboard_get_owner #}
    (toClipboard self)

-- %hash c:d6f8 d:486
-- | Clears the contents of the clipboard. Generally this should only be
-- called between the time you call 'clipboardSetWithOwner' or
-- 'clipboardSetWithData', and when the @clearFunc@ you supplied is called.
-- Otherwise, the clipboard may be owned by someone else.
--
_clipboardClear :: ClipboardClass self => self -> IO ()
_clipboardClear self =
  {# call gtk_clipboard_clear #}
    (toClipboard self)

-- %hash c:5211 d:14c6
-- | Sets the contents of the clipboard to the given UTF-8 string. Gtk+ will
-- make a copy of the text and take responsibility for responding for requests
-- for the text, and for converting the text into the requested format.
--
clipboardSetText :: (ClipboardClass self, GlibString string) => self
 -> string -- ^ @text@ - the text to be set as clipboard content
 -> IO ()
clipboardSetText self text =
  withUTFStringLen text $ \(textPtr,len) ->
  {# call gtk_clipboard_set_text #}
    (toClipboard self)
    textPtr
    (fromIntegral len)

#if GTK_CHECK_VERSION(2,6,0)
-- %hash c:5172 d:e4dd
-- | Sets the contents of the clipboard to the given 'Pixbuf'. Gtk+ will take
-- responsibility for responding for requests for the image, and for converting
-- the image into the requested format.
--
-- * Available since Gtk+ version 2.6
--
clipboardSetImage :: ClipboardClass self => self
 -> Pixbuf -- ^ @pixbuf@ - a 'Pixbuf'
 -> IO ()
clipboardSetImage self pixbuf =
  {# call gtk_clipboard_set_image #}
    (toClipboard self)
    pixbuf
#endif

-- %hash c:22cd d:f72d
-- | Requests the contents of clipboard as the given target. When the results
-- of the result are later received the supplied callback will be called.
--
clipboardRequestContents :: ClipboardClass self => self
 -> TargetTag                    -- ^ @target@ - an atom representing the form
                                 -- into which the clipboard owner should
                                 -- convert the selection.
 -> SelectionDataM ()            -- ^ @callback@ - A function to call when the
                                 -- results are received (or the retrieval
                                 -- fails). If the retrieval fails,
                                 -- 'selectionDataIsValid' returns @False@.
 -> IO ()
clipboardRequestContents self (Atom target) callback = do
  cbRef <- newIORef nullFunPtr
  cbPtr <- mkClipboardReceivedFunc
    (\_ sPtr _ -> do
      freeHaskellFunPtr =<< readIORef cbRef
      runReaderT callback sPtr
      return ())
  writeIORef cbRef cbPtr
  {# call gtk_clipboard_request_contents #}
    (toClipboard self)
    target
    cbPtr
    nullPtr

{#pointer ClipboardReceivedFunc#}

foreign import ccall "wrapper" mkClipboardReceivedFunc ::
  (Ptr Clipboard -> Ptr () -> Ptr () -> IO ()) -> IO ClipboardReceivedFunc

-- %hash c:7bb1 d:4ef1
-- | Requests the contents of the clipboard as text. When the text is later
-- received, it will be converted if it is stored in a different character set
-- if necessary, and @callback@ will be called.
--
-- The @text@ parameter to @callback@ will contain the resulting text if the
-- request succeeded, or @Nothing@ if it failed. This could happen for various reasons, in
-- particular if the clipboard was empty or if the contents of the clipboard
-- could not be converted into text form.
--
clipboardRequestText :: (ClipboardClass self, GlibString string) => self
 -> (Maybe string -> IO ())          -- ^ @callback@ - a function to call when
                                     -- the text is received, or the retrieval
                                     -- fails. (It will always be called one
                                     -- way or the other.)
 -> IO ()
clipboardRequestText self callback = do
  cbRef <- newIORef nullFunPtr
  cbPtr <- mkClipboardTextReceivedFunc
    (\_ sPtr _ -> do
      freeHaskellFunPtr =<< readIORef cbRef
      mStr <- if sPtr==nullPtr then return Nothing else
        liftM Just $ peekUTFString sPtr
      callback mStr)
  writeIORef cbRef cbPtr
  {# call gtk_clipboard_request_text #}
    (toClipboard self)
    cbPtr
    nullPtr

{#pointer ClipboardTextReceivedFunc#}

foreign import ccall "wrapper" mkClipboardTextReceivedFunc ::
  (Ptr Clipboard -> CString -> Ptr () -> IO ()) -> IO ClipboardTextReceivedFunc


#if GTK_CHECK_VERSION(2,6,0)
-- %hash c:3207 d:e3c1
-- | Requests the contents of the clipboard as image. When the image is later
-- received, it will be converted to a 'Pixbuf', and @callback@ will be called.
--
-- The @pixbuf@ parameter to @callback@ will contain the resulting 'Pixbuf'
-- if the request succeeded, or @Nothing@ if it failed. This could happen for various
-- reasons, in particular if the clipboard was empty or if the contents of the
-- clipboard could not be converted into an image.
--
-- * Available since Gtk+ version 2.6
--
clipboardRequestImage :: ClipboardClass self => self
 -> (Maybe Pixbuf -> IO ())           -- ^ @callback@ - a function to call
                                      -- when the image is received, or the
                                      -- retrieval fails. (It will always be
                                      -- called one way or the other.)
 -> IO ()
clipboardRequestImage self callback = do
  cbRef <- newIORef nullFunPtr
  cbPtr <- mkClipboardImageReceivedFunc
    (\_ sPtr _ -> do
      freeHaskellFunPtr =<< readIORef cbRef
      mPixbuf <- maybeNull (makeNewGObject mkPixbuf) (return sPtr)
      callback mPixbuf)
  writeIORef cbRef cbPtr
  {# call gtk_clipboard_request_image #}
    (toClipboard self)
    cbPtr
    nullPtr

{#pointer ClipboardImageReceivedFunc#}

foreign import ccall "wrapper" mkClipboardImageReceivedFunc ::
  (Ptr Clipboard -> Ptr Pixbuf -> Ptr () -> IO ()) -> IO ClipboardImageReceivedFunc

#endif

#if GTK_CHECK_VERSION(2,4,0)
-- %hash c:63f6 d:c0e1
-- | Requests the contents of the clipboard as list of supported targets. When
-- the list is later received, @callback@ will be called.
--
-- The @targets@ parameter to @callback@ will contain the resulting targets
-- if the request succeeded, or @Nothing@ if it failed.
--
-- * Available since Gtk+ version 2.4
--
clipboardRequestTargets :: ClipboardClass self => self
 -> (Maybe [TargetTag] -> IO ())        -- ^ @callback@ - a function to call
                                        -- when the targets are received, or
                                        -- the retrieval fails. (It will always
                                        -- be called one way or the other.)
 -> IO ()
clipboardRequestTargets self callback = do
  cbRef <- newIORef nullFunPtr
  cbPtr <- mkClipboardTargetsReceivedFunc
    (\_ tPtr len _ -> do
      -- We must free Haskell pointer *in* the callback to avoid segfault.
      freeHaskellFunPtr =<< readIORef cbRef
      mTargets <- if tPtr==nullPtr then return Nothing else
        liftM (Just . map Atom) $ peekArray (fromIntegral len) tPtr
      callback mTargets)
  writeIORef cbRef cbPtr
  {# call gtk_clipboard_request_targets #}
    (toClipboard self)
    cbPtr
    nullPtr

{#pointer ClipboardTargetsReceivedFunc#}

foreign import ccall "wrapper" mkClipboardTargetsReceivedFunc ::
  (Ptr Clipboard -> Ptr (Ptr ()) -> {#type gint#} -> Ptr () -> IO ()) -> IO ClipboardTargetsReceivedFunc

#if GTK_CHECK_VERSION(2,10,0)
-- %hash c:5601 d:d6a6
-- | Requests the contents of the clipboard as rich text. When the rich text
-- is later received, @callback@ will be called.
--
-- The @text@ parameter to @callback@ will contain the resulting rich text if
-- the request succeeded, or @Nothing@ if it failed.  This function can fail
-- for various reasons, in particular if the clipboard was empty or if the
-- contents of the clipboard could not be converted into rich text form.
--
-- * Available since Gtk+ version 2.10
--
clipboardRequestRichText :: (ClipboardClass self, TextBufferClass buffer, GlibString string) => self
 -> buffer                               -- ^ @buffer@ - a 'TextBuffer' that determines the supported rich text formats
  -> (Maybe (TargetTag,string) -> IO ()) -- ^ @callback@ - a function to call
                                         -- when the text is received, or the
                                         -- retrieval fails. (It will always be
                                         -- called one way or the other.)
 -> IO ()
clipboardRequestRichText self buffer callback = do
  cbRef <- newIORef nullFunPtr
  cbPtr <- mkClipboardRichTextReceivedFunc
    (\_ tPtr sPtr len _ -> do
      freeHaskellFunPtr =<< readIORef cbRef
      mRes <- if sPtr==nullPtr then return Nothing else liftM Just $ do
        str <- peekUTFStringLen (castPtr sPtr,fromIntegral len)
        return (Atom tPtr, str)
      callback mRes)
  writeIORef cbRef cbPtr
  {# call gtk_clipboard_request_rich_text #}
    (toClipboard self)
    (toTextBuffer buffer)
    cbPtr
    nullPtr

{#pointer ClipboardRichTextReceivedFunc#}

foreign import ccall "wrapper" mkClipboardRichTextReceivedFunc ::
  (Ptr Clipboard -> Ptr () -> Ptr CUChar -> {#type gsize#} -> Ptr () -> IO ()) ->
  IO ClipboardRichTextReceivedFunc
#endif
#endif

#if GTK_CHECK_VERSION(2,6,0)
-- %hash c:6e6a d:f98a
-- | Hints that the clipboard data should be stored somewhere when the
-- application exits or when 'clipboardStore' is called.
--
-- This value is reset when the clipboard owner changes. Where the clipboard
-- data is stored is platform dependent, see 'displayStoreClipboard' for more
-- information.
--
-- * Available since Gtk+ version 2.6
--
clipboardSetCanStore :: ClipboardClass self => self
 -> Maybe [(TargetTag, InfoId)] -- ^ @targets@ - list containing information
                                -- about which forms should be stored or
                                -- @Nothing@ to indicate that all forms
                                -- should be stored.
 -> IO ()
clipboardSetCanStore self Nothing =
  {# call gtk_clipboard_set_can_store #}
    (toClipboard self)
    nullPtr
    0
clipboardSetCanStore self (Just targets) =
  withTargetEntries targets $ \nTargets targets ->
  {# call gtk_clipboard_set_can_store #}
    (toClipboard self)
    targets
    (fromIntegral nTargets)

-- %hash c:f98a d:ded8
-- | Stores the current clipboard data somewhere so that it will stay around
-- after the application has quit.
--
-- * Available since Gtk+ version 2.6
--
clipboardStore :: ClipboardClass self => self -> IO ()
clipboardStore self =
  {# call gtk_clipboard_store #}
    (toClipboard self)
#endif
