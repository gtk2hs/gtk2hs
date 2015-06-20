{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Assistant
--
--  Author : Andy Stewart
--
--  Created: 22 Mar 2010
--
--  Copyright (C) 2010 Andy Stewart
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
-- A widget used to guide users through multi-step operations
--
-- * Module available since Gtk+ version 2.10
--
module Graphics.UI.Gtk.Windows.Assistant (

-- * Detail
--
-- | A 'Assistant' is a widget used to represent a generally complex operation
-- splitted in several steps, guiding the user through its pages and
-- controlling the page flow to collect the necessary data.

-- ** GtkAssistant as GtkBuildable
--
-- | The 'Assistant' implementation of the 'Buildable' interface exposes the
-- @actionArea@ as internal children with the name \"action_area\".
--
-- To add pages to an assistant in 'Builder', simply add it as a \<child> to
-- the 'Assistant' object, and set its child properties as necessary.

-- * Class Hierarchy
--
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Bin'
-- |                           +----'Window'
-- |                                 +----Assistant
-- @

#if GTK_CHECK_VERSION(2,10,0)
-- * Types
  Assistant,
  AssistantClass,
  castToAssistant,
  toAssistant,

-- * Enums.
  AssistantPageType(..),

-- * Constructors
  assistantNew,

-- * Methods
  assistantGetNPages,
  assistantGetNthPage,
  assistantPrependPage,
  assistantAppendPage,
  assistantInsertPage,
  assistantSetForwardPageFunc,
  assistantAddActionWidget,
  assistantRemoveActionWidget,
  assistantUpdateButtonsState,
  assistantSetPageType,
  assistantGetPageType,
  assistantSetPageTitle,
  assistantGetPageTitle,
  assistantSetPageHeaderImage,
  assistantGetPageHeaderImage,
  assistantSetPageSideImage,
  assistantGetPageSideImage,
  assistantSetPageComplete,
  assistantGetPageComplete,
#if GTK_CHECK_VERSION(2,22,0)
  assistantCommit,
#endif

-- * Attributes
  assistantCurrentPage,

-- * Child Attributes
  assistantChildPageType,
  assistantChildTitle,
  assistantChildHeaderImage,
  assistantChildSidebarImage,
  assistantChildComplete,

-- * Signals
  assistantCancel,
  assistantPrepare,
  assistantApply,
  assistantClose,
#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

#if GTK_CHECK_VERSION(2,10,0)
--------------------
-- Enum
{#enum GtkAssistantPageType as AssistantPageType {underscoreToCase} deriving (Bounded,Eq,Show)#}

--------------------
-- Constructors

-- | Creates a new 'Assistant'.
--
-- * Available since Gtk+ version 2.10
--
assistantNew :: IO Assistant
assistantNew =
  makeNewObject mkAssistant $
  liftM (castPtr :: Ptr Widget -> Ptr Assistant) $
  {# call gtk_assistant_new #}

--------------------
-- Methods

-- | Returns the page number of the current page
--
-- * Available since Gtk+ version 2.10
--
assistantGetCurrentPage :: AssistantClass self => self
 -> IO Int -- ^ returns The index (starting from 0) of the current page in the
           -- @assistant@, if the @assistant@ has no pages, -1 will be returned
assistantGetCurrentPage self =
  liftM fromIntegral $
  {# call gtk_assistant_get_current_page #}
    (toAssistant self)

-- | Switches the page to @pageNum@. Note that this will only be necessary in custom buttons, as the
-- assistant flow can be set with 'assistantSetForwardPageFunc'.
--
-- * Available since Gtk+ version 2.10
--
assistantSetCurrentPage :: AssistantClass self => self
 -> Int -- ^ @pageNum@ - index of the page to switch to, starting from 0. If
        -- negative, the last page will be used. If greater than the number of
        -- pages in the @assistant@, nothing will be done.
 -> IO ()
assistantSetCurrentPage self pageNum =
  {# call gtk_assistant_set_current_page #}
    (toAssistant self)
    (fromIntegral pageNum)

-- | Returns the number of pages in the @assistant@
--
--
-- * Available since Gtk+ version 2.10
--
assistantGetNPages :: AssistantClass self => self
 -> IO Int -- ^ returns The number of pages in the @assistant@.
assistantGetNPages self =
  liftM fromIntegral $
  {# call gtk_assistant_get_n_pages #}
    (toAssistant self)

-- | Returns the child widget contained in page number @pageNum@.
--
--
-- * Available since Gtk+ version 2.10
--
assistantGetNthPage :: AssistantClass self => self
 -> Int       -- ^ @pageNum@ - The index of a page in the @assistant@, or -1
              -- to get the last page;
 -> IO (Maybe Widget)            -- ^ returns The child widget, or 'Nothing' if @pageNum@ is out of bounds.
assistantGetNthPage self pageNum =
  maybeNull (makeNewObject mkWidget) $
  {# call gtk_assistant_get_nth_page #}
    (toAssistant self)
    (fromIntegral pageNum)

-- | Prepends a page to the @assistant@.
--
--
-- * Available since Gtk+ version 2.10
--
assistantPrependPage :: (AssistantClass self, WidgetClass page) => self
 -> page   -- ^ @page@ - a 'Widget'
 -> IO Int -- ^ returns the index (starting at 0) of the inserted page
assistantPrependPage self page =
  liftM fromIntegral $
  {# call gtk_assistant_prepend_page #}
    (toAssistant self)
    (toWidget page)

-- | Appends a page to the @assistant@.
--
--
-- * Available since Gtk+ version 2.10
--
assistantAppendPage :: (AssistantClass self, WidgetClass page) => self
 -> page   -- ^ @page@ - a 'Widget'
 -> IO Int -- ^ returns the index (starting at 0) of the inserted page
assistantAppendPage self page =
  liftM fromIntegral $
  {# call gtk_assistant_append_page #}
    (toAssistant self)
    (toWidget page)

-- | Inserts a page in the @assistant@ at a given position.
--
--
-- * Available since Gtk+ version 2.10
--
assistantInsertPage :: (AssistantClass self, WidgetClass page) => self
 -> page   -- ^ @page@ - a 'Widget'
 -> Int    -- ^ @position@ - the index (starting at 0) at which to insert the
           -- page, or -1 to append the page to the @assistant@
 -> IO Int -- ^ returns the index (starting from 0) of the inserted page
assistantInsertPage self page position =
  liftM fromIntegral $
  {# call gtk_assistant_insert_page #}
    (toAssistant self)
    (toWidget page)
    (fromIntegral position)

-- | Sets the page forwarding function to be @pageFunc@, this function will be
-- used to determine what will be the next page when the user presses the
-- forward button. Setting @pageFunc@ to 'Nothing' will make the assistant to use the
-- default forward function, which just goes to the next visible page.
--
--
-- * Available since Gtk+ version 2.10
--
assistantSetForwardPageFunc :: AssistantClass self => self
 -> Maybe (Int -> IO Int)             -- ^ @pageFunc@ - the 'AssistantPage', or 'Nothing' to use the default one.
 -> IO ()
assistantSetForwardPageFunc self Nothing = do
  {# call gtk_assistant_set_forward_page_func #}
    (toAssistant self)
    nullFunPtr
    (castFunPtrToPtr nullFunPtr)
    destroyFunPtr

assistantSetForwardPageFunc self (Just pageFunc) = do
  pfPtr <- mkAssistantPageFunc $ \ c _  -> do
            result <- pageFunc (fromIntegral c)
            return $ fromIntegral result
  {# call gtk_assistant_set_forward_page_func #}
    (toAssistant self)
    pfPtr
    (castFunPtrToPtr pfPtr)
    destroyFunPtr

{#pointer AssistantPageFunc#}

foreign import ccall "wrapper" mkAssistantPageFunc ::
  ({#type gint#} -> Ptr () -> IO {#type gint#})
  -> IO AssistantPageFunc

-- | Sets the page type for @page@. The page type determines the page behavior
-- in the @assistant@.
--
--
-- * Available since Gtk+ version 2.10
--
assistantSetPageType :: (AssistantClass self, WidgetClass page) => self
 -> page              -- ^ @page@ - a page of @assistant@
 -> AssistantPageType -- ^ @type@ - the new type for @page@
 -> IO ()
assistantSetPageType self page type_ =
  {# call gtk_assistant_set_page_type #}
    (toAssistant self)
    (toWidget page)
    ((fromIntegral . fromEnum) type_)

-- | Gets the page type of @page@.
--
--
-- * Available since Gtk+ version 2.10
--
assistantGetPageType :: (AssistantClass self, WidgetClass page) => self
 -> page                 -- ^ @page@ - a page of @assistant@
 -> IO AssistantPageType -- ^ returns the page type of @page@.
assistantGetPageType self page =
  liftM (toEnum . fromIntegral) $
  {# call gtk_assistant_get_page_type #}
    (toAssistant self)
    (toWidget page)

-- | Sets a title for @page@. The title is displayed in the header area of the
-- assistant when @page@ is the current page.
--
--
-- * Available since Gtk+ version 2.10
--
assistantSetPageTitle :: (AssistantClass self, WidgetClass page, GlibString string) => self
 -> page   -- ^ @page@ - a page of @assistant@
 -> string -- ^ @title@ - the new title for @page@
 -> IO ()
assistantSetPageTitle self page title =
  withUTFString title $ \titlePtr ->
  {# call gtk_assistant_set_page_title #}
    (toAssistant self)
    (toWidget page)
    titlePtr

-- | Gets the title for @page@.
--
--
-- * Available since Gtk+ version 2.10
--
assistantGetPageTitle :: (AssistantClass self, WidgetClass page, GlibString string) => self
 -> page      -- ^ @page@ - a page of @assistant@
 -> IO string -- ^ returns the title for @page@.
assistantGetPageTitle self page =
  {# call gtk_assistant_get_page_title #}
    (toAssistant self)
    (toWidget page)
  >>= peekUTFString

-- | Sets a header image for @page@. This image is displayed in the header
-- area of the assistant when @page@ is the current page.
--
--
-- * Available since Gtk+ version 2.10
--
assistantSetPageHeaderImage :: (AssistantClass self, WidgetClass page) => self
 -> page   -- ^ @page@ - a page of @assistant@
 -> Pixbuf -- ^ @pixbuf@ - the new header image @page@
 -> IO ()
assistantSetPageHeaderImage self page pixbuf =
  {# call gtk_assistant_set_page_header_image #}
    (toAssistant self)
    (toWidget page)
    pixbuf

-- | Gets the header image for @page@.
--
--
-- * Available since Gtk+ version 2.10
--
assistantGetPageHeaderImage :: (AssistantClass self, WidgetClass page) => self
 -> page      -- ^ @page@ - a page of @assistant@
 -> IO (Maybe Pixbuf)            -- ^ returns the header image for @page@, or 'Nothing' if there's no header image for the page.
assistantGetPageHeaderImage self page =
  maybeNull (makeNewGObject mkPixbuf) $
  {# call gtk_assistant_get_page_header_image #}
    (toAssistant self)
    (toWidget page)

-- | Sets a header image for @page@. This image is displayed in the side area
-- of the assistant when @page@ is the current page.
--
--
-- * Available since Gtk+ version 2.10
--
assistantSetPageSideImage :: (AssistantClass self, WidgetClass page) => self
 -> page   -- ^ @page@ - a page of @assistant@
 -> Pixbuf -- ^ @pixbuf@ - the new header image @page@
 -> IO ()
assistantSetPageSideImage self page pixbuf =
  {# call gtk_assistant_set_page_side_image #}
    (toAssistant self)
    (toWidget page)
    pixbuf

-- | Gets the header image for @page@.
--
--
-- * Available since Gtk+ version 2.10
--
assistantGetPageSideImage :: (AssistantClass self, WidgetClass page) => self
 -> page      -- ^ @page@ - a page of @assistant@
 -> IO (Maybe Pixbuf)            -- ^ returns the side image for @page@, or 'Nothing' if there's no side image for the page.
assistantGetPageSideImage self page =
  maybeNull (makeNewGObject mkPixbuf) $
  {# call gtk_assistant_get_page_side_image #}
    (toAssistant self)
    (toWidget page)

-- | Sets whether @page@ contents are complete. This will make @assistant@
-- update the buttons state to be able to continue the task.
--
--
-- * Available since Gtk+ version 2.10
--
assistantSetPageComplete :: (AssistantClass self, WidgetClass page) => self
 -> page -- ^ @page@ - a page of @assistant@
 -> Bool -- ^ @complete@ - the completeness status of the page
 -> IO ()
assistantSetPageComplete self page complete =
  {# call gtk_assistant_set_page_complete #}
    (toAssistant self)
    (toWidget page)
    (fromBool complete)

-- | Gets whether @page@ is complete.
--
--
-- * Available since Gtk+ version 2.10
--
assistantGetPageComplete :: (AssistantClass self, WidgetClass page) => self
 -> page    -- ^ @page@ - a page of @assistant@
 -> IO Bool -- ^ returns @True@ if @page@ is complete.
assistantGetPageComplete self page =
  liftM toBool $
  {# call gtk_assistant_get_page_complete #}
    (toAssistant self)
    (toWidget page)

#if GTK_CHECK_VERSION(2,22,0)
-- | Erases the visited page history so the back button is not shown on the current page, and removes the
-- cancel button from subsequent pages.
--
-- Use this when the information provided up to the current page is hereafter deemed permanent and
-- cannot be modified or undone. For example, showing a progress page to track a long-running,
-- unreversible operation after the user has clicked apply on a confirmation page.
--
-- * Available since Gtk+ version 2.22
--
assistantCommit :: AssistantClass self => self -> IO ()
assistantCommit self =
  {#call gtk_assistant_commit#} (toAssistant self)
#endif

-- | Adds a widget to the action area of a 'Assistant'.
--
--
-- * Available since Gtk+ version 2.10
--
assistantAddActionWidget :: (AssistantClass self, WidgetClass child) => self
 -> child -- ^ @child@ - a 'Widget'
 -> IO ()
assistantAddActionWidget self child =
  {# call gtk_assistant_add_action_widget #}
    (toAssistant self)
    (toWidget child)

-- | Removes a widget from the action area of a 'Assistant'.
--
--
-- * Available since Gtk+ version 2.10
--
assistantRemoveActionWidget :: (AssistantClass self, WidgetClass child) => self
 -> child -- ^ @child@ - a 'Widget'
 -> IO ()
assistantRemoveActionWidget self child =
  {# call gtk_assistant_remove_action_widget #}
    (toAssistant self)
    (toWidget child)

-- | Forces @assistant@ to recompute the buttons state.
--
-- Gtk+ automatically takes care of this in most situations, e.g. when the
-- user goes to a different page, or when the visibility or completeness of a
-- page changes.
--
-- One situation where it can be necessary to call this function is when
-- changing a value on the current page affects the future page flow of the
-- assistant.
--
--
-- * Available since Gtk+ version 2.10
--
assistantUpdateButtonsState :: AssistantClass self => self -> IO ()
assistantUpdateButtonsState self =
  {# call gtk_assistant_update_buttons_state #}
    (toAssistant self)

--------------------
-- Attributes

-- | Switches the page to @pageNum@. Note that this will only be necessary in
-- custom buttons, as the @assistant@ flow can be set with
-- 'assistantSetForwardPageFunc'.
--
-- Returns the page number of the current page
--
--
-- * Available since Gtk+ version 2.10
--
assistantCurrentPage :: AssistantClass self => Attr self Int
assistantCurrentPage = newAttr
  assistantGetCurrentPage
  assistantSetCurrentPage

--------------------
-- Child Attributes

-- | The type of the assistant page.
--
-- Default value: 'AssistantPageContent'
--
--
-- * Available since Gtk+ version 2.10
--
assistantChildPageType :: AssistantClass self => Attr self AssistantPageType
assistantChildPageType =
    newAttrFromEnumProperty "page-type" {#call pure unsafe assistant_page_type_get_type#}

-- | The title that is displayed in the page header.
--
-- If title and header-image are both, no header is displayed.
--
--
-- * Available since Gtk+ version 2.10
--
assistantChildTitle :: (AssistantClass self, GlibString string) => Attr self string
assistantChildTitle = newAttrFromStringProperty "title"

-- | The image that is displayed next to the page.
--
--
-- * Available since Gtk+ version 2.10
--
assistantChildHeaderImage :: AssistantClass self => Attr self Pixbuf
assistantChildHeaderImage = newAttrFromObjectProperty "header-image"
                              {# call pure unsafe gdk_pixbuf_get_type #}

-- | Sidebar image for the assistant page.
--
--
-- * Available since Gtk+ version 2.10
--
assistantChildSidebarImage :: AssistantClass self => Attr self Pixbuf
assistantChildSidebarImage = newAttrFromObjectProperty "sidebar-image"
                               {# call pure unsafe gdk_pixbuf_get_type #}

-- | Setting the \"complete\" child property to @True@ marks a page as
-- complete (i.e.: all the required fields are filled out). Gtk+ uses this
-- information to control the sensitivity of the navigation buttons.
--
-- Default value: @False@
--
--
-- * Available since Gtk+ version 2.10
--
assistantChildComplete :: AssistantClass self => Attr self Bool
assistantChildComplete = newAttrFromBoolProperty "complete"

--------------------
-- Signals

-- | The ::assistantCancel signal is emitted when then the assistantCancel button is clicked.
--
--
-- * Available since Gtk+ version 2.10
--
assistantCancel :: AssistantClass self => Signal self (IO ())
assistantCancel = Signal (connect_NONE__NONE "cancel")

-- | The ::assistantPrepare signal is emitted when a new page is set as the assistant's
-- current page, before making the new page visible. A handler for this signal
-- can do any preparation which are necessary before showing @page@.
--
--
-- * Available since Gtk+ version 2.10
--
assistantPrepare :: AssistantClass self => Signal self (Widget -> IO ())
assistantPrepare = Signal (connect_OBJECT__NONE "prepare")

-- | The ::assistantApply signal is emitted when the assistantApply button is clicked. The
-- default behavior of the 'Assistant' is to switch to the page after the
-- current page, unless the current page is the last one.
--
-- A handler for the ::assistantApply signal should carry out the actions for which
-- the wizard has collected data. If the action takes a long time to complete,
-- you might consider to put a page of type 'AssistantPageProgress' after the
-- confirmation page and handle this operation within the 'assistantPrepare' signal of the progress page.
--
--
-- * Available since Gtk+ version 2.10
--
assistantApply :: AssistantClass self => Signal self (IO ())
assistantApply = Signal (connect_NONE__NONE "apply")

-- | The ::assistantClose signal is emitted either when the assistantClose button of a summary
-- page is clicked, or when the apply button in the last page in the flow (of
-- type 'AssistantPageConfirm') is clicked.
--
assistantClose :: AssistantClass self => Signal self (IO ())
assistantClose = Signal (connect_NONE__NONE "close")
#endif
