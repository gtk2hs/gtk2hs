{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Notebook
--
--  Author : Axel Simon, Andy Stewart
--
--  Created: 15 May 2001
--
--  Copyright (C) 1999-2005 Axel Simon
--  Copyright (C) 2009 Andy Stewart
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
-- TODO
--
-- Functions:
--    gtk_notebook_set_group
--    gtk_notebook_get_group
--    gtk_notebook_set_window_creation_hook
-- Attributes:
--    group
-- Signals:
--    focusTab
--
-- NOTE
--
--    Don't binding `group-id` attribute, even set/get_group_id functions is deprecated)
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- A tabbed notebook container
--
module Graphics.UI.Gtk.Layout.Notebook (
-- * Detail
--
-- | The 'Notebook' widget is a 'Container' whose children are pages that can
-- be switched between using tab labels along one edge.
--
-- There are many configuration options for 'Notebook'. Among other things,
-- you can choose on which edge the tabs appear (see 'notebookSetTabPos'),
-- whether, if there are too many tabs to fit the noteobook should be made
-- bigger or scrolling arrows added (see 'notebookSetScrollable'), and
-- whether there will be a popup menu allowing the users to switch pages. (see
-- 'notebookEnablePopup')

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----Notebook
-- @

-- * Types
  Notebook,
  NotebookClass,
#if GTK_MAJOR_VERSION < 3
  NotebookPage,
#endif
  castToNotebook, gTypeNotebook,
  toNotebook,

-- * Constructors
  notebookNew,

-- * Methods
  notebookAppendPage,
  notebookAppendPageMenu,
  notebookPrependPage,
  notebookPrependPageMenu,
  notebookInsertPage,
  notebookInsertPageMenu,
  notebookRemovePage,
  notebookPageNum,
  notebookSetCurrentPage,
  notebookNextPage,
  notebookPrevPage,
  notebookReorderChild,
  PositionType(..),
  notebookSetTabPos,
  notebookGetTabPos,
  notebookSetShowTabs,
  notebookGetShowTabs,
  notebookSetShowBorder,
  notebookGetShowBorder,
  notebookSetScrollable,
  notebookGetScrollable,
#if GTK_MAJOR_VERSION < 3
#ifndef DISABLE_DEPRECATED
  notebookSetTabBorder,
  notebookSetTabHBorder,
  notebookSetTabVBorder,
#endif
#endif
  notebookSetPopup,
  notebookGetCurrentPage,
  notebookSetMenuLabel,
  notebookGetMenuLabel,
  notebookSetMenuLabelText,
  notebookGetMenuLabelText,
  notebookGetNthPage,
#if GTK_CHECK_VERSION(2,2,0)
  notebookGetNPages,
#endif
  notebookGetTabLabel,
  notebookGetTabLabelText,
  Packing(..), PackType(..),
#if GTK_MAJOR_VERSION < 3
  notebookQueryTabLabelPacking,
  notebookSetTabLabelPacking,
#ifndef DISABLE_DEPRECATED
  notebookSetHomogeneousTabs,
#endif
#endif
  notebookSetTabLabel,
  notebookSetTabLabelText,
#if GTK_CHECK_VERSION(2,10,0)
  notebookSetTabReorderable,
  notebookGetTabReorderable,
  notebookSetTabDetachable,
  notebookGetTabDetachable,
#endif
#if GTK_CHECK_VERSION(2,20,0)
  notebookSetActionWidget,
  notebookGetActionWidget,
#endif

-- * Attributes
  notebookPage,
  notebookTabPos,
  notebookTabBorder,
  notebookTabHborder,
  notebookTabVborder,
  notebookShowTabs,
  notebookShowBorder,
  notebookScrollable,
  notebookEnablePopup,
  notebookHomogeneous,
  notebookCurrentPage,

-- * Child Attributes
  notebookChildTabLabel,
  notebookChildMenuLabel,
  notebookChildPosition,
  notebookChildTabPacking,
  notebookChildTabPackType,
  notebookChildDetachable,
  notebookChildReorderable,
  notebookChildTabExpand,
  notebookChildTabFill,

-- * Style Attributes
#if GTK_CHECK_VERSION(2,10,0)
  notebookStyleArrowSpacing,
#endif
  notebookStyleHasBackwardStepper,
  notebookStyleHasForwardStepper,
  notebookStyleHasSecondaryBackwardStepper,
  notebookStyleHasSecondaryForwardStepper,
#if GTK_CHECK_VERSION(2,10,0)
  notebookStyleTabCurvature,
  notebookStyleTabOverlap,
#endif

-- * Signals
  switchPage,
  pageAdded,
  pageRemoved,
  pageReordered,

-- * Deprecated
#ifndef DISABLE_DEPRECATED
  onSwitchPage,
  afterSwitchPage
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
import Graphics.UI.Gtk.Abstract.ContainerChildProperties
import Graphics.UI.Gtk.Display.Label    (labelNew)
import Graphics.UI.Gtk.General.Enums    (Packing(..), toPacking, fromPacking,
                                         PackType(..), PositionType(..))

{# context lib="gtk" prefix="gtk" #}

#if GTK_MAJOR_VERSION < 3
{#pointer *GtkNotebookPage as NotebookPage foreign newtype #}
_ignoreNotebookPage = NotebookPage
#endif

--------------------
-- Constructors

-- | Creates a new 'Notebook' widget with no pages.
--
notebookNew :: IO Notebook
notebookNew =
  makeNewObject mkNotebook $
  liftM (castPtr :: Ptr Widget -> Ptr Notebook) $
  {# call unsafe notebook_new #}

--------------------
-- Methods

#if GTK_CHECK_VERSION(2,4,0)
-- | Appends a page to @notebook@.
--
-- The given label will be used for the label widget of the new tab. In case
-- the context menu is enabled, this name will also appear in the popup menu.
-- If you want to specify something else to go in the tab, use
-- 'notebookAppendPageMenu'.
--
-- * This function returned @()@ in Gtk+ version 2.2.X and earlier
--
notebookAppendPage :: (NotebookClass self, WidgetClass child, GlibString string) => self
 -> child    -- ^ @child@ - the 'Widget' to use as the contents of the page.
 -> string   -- ^ @tabLabel@ - the label for the page
 -> IO Int   -- ^ returns the index (starting from 0) of the appended page in
             -- the notebook, or -1 if function fails
notebookAppendPage self child tabLabel = do
  tab <- labelNew (Just tabLabel)
  liftM fromIntegral $
   {# call notebook_append_page #}
    (toNotebook self)
    (toWidget child)
    (toWidget tab)

#else
-- | Appends a page to @notebook@.
--
-- The given label will be used for the label widget of the new tab. In case
-- the context menu is enabled, this name will also appear in the popup menu.
-- If you want to specify something else to go in the tab, use
-- 'notebookAppendPageMenu'.
--
-- * This function returns @Int@ in Gtk+ version 2.4.0 and later.
--
notebookAppendPage :: (NotebookClass self, WidgetClass child, GlibString string) => self
 -> child    -- ^ @child@ - the 'Widget' to use as the contents of the page.
 -> string   -- ^ @tabLabel@ - the label for the page
 -> IO ()
notebookAppendPage self child tabLabel = do
  tab <- labelNew (Just tabLabel)
  {# call notebook_append_page #}
    (toNotebook self)
    (toWidget child)
    (toWidget tab)
#endif

#if GTK_CHECK_VERSION(2,4,0)
-- | Appends a page to @notebook@, specifying the widget to use as the label
-- in the popup menu.
--
-- Like 'notebookAppendPage' but allows any widget to be used for the label of
-- the new tab and the entry in the page-switch popup menu.
--
-- * This function returned @()@ in Gtk version 2.2.X and earlier
--
notebookAppendPageMenu :: (NotebookClass self, WidgetClass child,
  WidgetClass tabLabel, WidgetClass menuLabel) => self
 -> child     -- ^ @child@ - the 'Widget' to use as the contents of the page.
 -> tabLabel  -- ^ @tabLabel@ - the 'Widget' to be used as the label for the
              -- page (usually a 'Label' widget).
 -> menuLabel -- ^ @menuLabel@ - the widget to use as a label for the
              -- page-switch menu, if that is enabled (usually a 'Label'
              -- widget).
 -> IO Int    -- ^ returns the index (starting from 0) of the appended page in
              -- the notebook, or -1 if function fails
notebookAppendPageMenu self child tabLabel menuLabel =
  liftM fromIntegral $
  {# call notebook_append_page_menu #}
    (toNotebook self)
    (toWidget child)
    (toWidget tabLabel)
    (toWidget menuLabel)

#else
-- | Appends a page to @notebook@, specifying the widget to use as the label
-- in the popup menu.
--
-- Like 'notebookAppendPage' but allows any widget to be used for the label of
-- the new tab and then entry in the page-switch popup menu.
--
-- * This function returns @Int@ in Gtk+ version 2.4.0 and later
--
notebookAppendPageMenu :: (NotebookClass self, WidgetClass child,
  WidgetClass tabLabel, WidgetClass menuLabel) => self
 -> child     -- ^ @child@ - the 'Widget' to use as the contents of the page.
 -> tabLabel  -- ^ @tabLabel@ - the 'Widget' to be used as the label for the
              -- page (usually a 'Label' widget).
 -> menuLabel -- ^ @menuLabel@ - the widget to use as a label for the
              -- page-switch menu, if that is enabled (usually a 'Label'
              -- widget).
 -> IO ()
notebookAppendPageMenu self child tabLabel menuLabel =
  {# call notebook_append_page_menu #}
    (toNotebook self)
    (toWidget child)
    (toWidget tabLabel)
    (toWidget menuLabel)
#endif

#if GTK_CHECK_VERSION(2,4,0)
-- | Prepends a page to @notebook@.
--
-- * The given label will be used for the label widget of the new tab. In case
-- the context menu is enabled, this name will also appear in the popup menu. If
-- you want to specify something else to go in the tab, use
-- 'notebookPrependPageMenu'.
--
-- * This function returned @()@ in Gtk version 2.2.X and earlier
--
notebookPrependPage :: (NotebookClass self, WidgetClass child, GlibString string) => self
 -> child    -- ^ @child@ - the 'Widget' to use as the contents of the page.
 -> string   -- ^ @tabLabel@ - the label for the page
 -> IO Int   -- ^ returns the index (starting from 0) of the prepended page in
             -- the notebook, or -1 if function fails
notebookPrependPage self child tabLabel = do
  tab <- labelNew (Just tabLabel)
  liftM fromIntegral $
   {# call notebook_prepend_page #}
    (toNotebook self)
    (toWidget child)
    (toWidget tab)

#else
-- | Prepends a page to @notebook@.
--
-- * The given label will be used for the label widget of the new tab. In case
-- the context popup menu is enabled, this name will also appear in the menu. If
-- you want to specify something else to go in the tab, use
-- 'notebookPrependPageMenu'.
--
-- * This function returns @Int@ in Gtk version 2.4.0 and later.
--
notebookPrependPage :: (NotebookClass self, WidgetClass child, GlibString string) => self
 -> child    -- ^ @child@ - the 'Widget' to use as the contents of the page.
 -> string   -- ^ @tabLabel@ - the label for the page
 -> IO ()
notebookPrependPage self child tabLabel = do
  tab <- labelNew (Just tabLabel)
  {# call notebook_prepend_page #}
    (toNotebook self)
    (toWidget child)
    (toWidget tab)
#endif

#if GTK_CHECK_VERSION(2,4,0)
-- | Prepends a page to @notebook@, specifying the widget to use as the label
-- in the popup menu.
--
-- Like 'notebookPrependPage' but allows any widget to be used for the label of
-- the new tab and then entry in the page-switch popup menu.
--
-- * This function returned @()@ in Gtk version 2.2.X and earlier
--
notebookPrependPageMenu :: (NotebookClass self, WidgetClass child,
 WidgetClass tabLabel, WidgetClass menuLabel) => self
 -> child     -- ^ @child@ - the 'Widget' to use as the contents of the page.
 -> tabLabel  -- ^ @tabLabel@ - the 'Widget' to be used as the label for the
              -- page (usually a 'Label' widget).
 -> menuLabel -- ^ @menuLabel@ - the widget to use as a label for the
              -- page-switch menu, if that is enabled (usually a 'Label'
              -- widget).
 -> IO Int    -- ^ returns the index (starting from 0) of the prepended page
              -- in the notebook, or -1 if function fails
notebookPrependPageMenu self child tabLabel menuLabel =
  liftM fromIntegral $
  {# call notebook_prepend_page_menu #}
    (toNotebook self)
    (toWidget child)
    (toWidget tabLabel)
    (toWidget menuLabel)
#else
-- | Prepends a page to @notebook@, specifying the widget to use as the label
-- in the popup menu.
--
-- Like 'notebookPrependPage' but allows any widget to be used for the label of
-- the new tab and then entry in the page-switch popup menu.
--
-- * This function returns @Int@ in Gtk version 2.4.0 and later
--
notebookPrependPageMenu :: (NotebookClass self, WidgetClass child,
 WidgetClass tabLabel, WidgetClass menuLabel) => self
 -> child     -- ^ @child@ - the 'Widget' to use as the contents of the page.
 -> tabLabel  -- ^ @tabLabel@ - the 'Widget' to be used as the label for the
              -- page (usually a 'Label' widget).
 -> menuLabel -- ^ @menuLabel@ - the widget to use as a label for the
              -- page-switch menu, if that is enabled (usually a 'Label'
              -- widget).
  -> IO ()
notebookPrependPageMenu self child tabLabel menuLabel =
  {# call notebook_prepend_page_menu #}
    (toNotebook self)
    (toWidget child)
    (toWidget tabLabel)
    (toWidget menuLabel)
#endif

#if GTK_CHECK_VERSION(2,4,0)
-- | Insert a page into @notebook@ at the given position.
--
-- * The given label will be used for the label widget of the new tab. In case
-- the context menu is enabled, this name will also appear in the popup menu. If
-- you want to specify something else to go in the tab, use
-- 'notebookInsertPageMenu'.
--
-- * This function returned @()@ in Gtk version 2.2.X and earlier
--
notebookInsertPage :: (NotebookClass self, WidgetClass child, GlibString string) => self
 -> child    -- ^ @child@ - the 'Widget' to use as the contents of the page.
 -> string   -- ^ @tabLabel@ - the label for the page
 -> Int      -- ^ @position@ - the index (starting at 0) at which to insert
             -- the page, or -1 to append the page after all other pages.
 -> IO Int   -- ^ returns the index (starting from 0) of the inserted page in
             -- the notebook, or -1 if function fails
notebookInsertPage self child tabLabel position = do
  tab <- labelNew (Just tabLabel)
  liftM fromIntegral $
   {# call notebook_insert_page #}
    (toNotebook self)
    (toWidget child)
    (toWidget tab)
    (fromIntegral position)

#else
-- | Insert a page into @notebook@ at the given position.
--
-- * The given label will be used for the label widget of the new tab. In case
-- the context menu is enabled, this name will also appear in the popup menu. If
-- you want to specify something else to go in the tab, use
-- 'notebookInsertPageMenu'.
--
-- * This function returns @Int@ in Gtk version 2.4.0 and later.
--
notebookInsertPage :: (NotebookClass self, WidgetClass child, GlibString string) => self
 -> child    -- ^ @child@ - the 'Widget' to use as the contents of the page.
 -> string   -- ^ @tabLabel@ - the label for the page
 -> Int      -- ^ @position@ - the index (starting at 0) at which to insert
             -- the page, or -1 to append the page after all other pages.
 -> IO ()
notebookInsertPage self child tabLabel position = do
  tab <- labelNew (Just tabLabel)
  {# call notebook_insert_page #}
    (toNotebook self)
    (toWidget child)
    (toWidget tab)
    (fromIntegral position)
#endif

#if GTK_CHECK_VERSION(2,4,0)
-- | Insert a page into @notebook@ at the given position, specifying the
-- widget to use as the label in the popup menu.
--
-- Like 'notebookInsertPage' but allows any widget to be used for the label of
-- the new tab and then entry in the page-switch popup menu.
--
-- * This function returned @()@ in Gtk version 2.2.X and earlier
--
notebookInsertPageMenu :: (NotebookClass self, WidgetClass child,
 WidgetClass tabLabel, WidgetClass menuLabel) => self
 -> child     -- ^ @child@ - the 'Widget' to use as the contents of the page.
 -> tabLabel  -- ^ @tabLabel@ - the 'Widget' to be used as the label for the
              -- page (usually a 'Label' widget).
 -> menuLabel -- ^ @menuLabel@ - the widget to use as a label for the
              -- page-switch menu, if that is enabled (usually a 'Label'
              -- widget).
 -> Int       -- ^ @position@ - the index (starting at 0) at which to insert
              -- the page, or -1 to append the page after all other pages.
 -> IO Int    -- ^ returns the index (starting from 0) of the inserted page in
              -- the notebook, or -1 if function fails
notebookInsertPageMenu self child tabLabel menuLabel position =
  liftM fromIntegral $
  {# call notebook_insert_page_menu #}
    (toNotebook self)
    (toWidget child)
    (toWidget tabLabel)
    (toWidget menuLabel)
    (fromIntegral position)
#else
-- | Insert a page into @notebook@ at the given position, specifying the
-- widget to use as the label in the popup menu.
--
-- Like 'notebookInsertPage' but allows any widget to be used for the label of
-- the new tab and then entry in the page-switch popup menu.
--
-- * This function returns @Int@ in Gtk version 2.4.0 and later
--
notebookInsertPageMenu ::(NotebookClass nb, WidgetClass child,
   WidgetClass tab, WidgetClass menu) => nb
  -> child  -- ^ Widget to use as the contents of the page
  -> tab    -- ^ Tab label widget for the page.
  -> menu   -- ^ Menu entry for this tab (usually a 'Label' widget).
  -> Int    -- ^ Position for the new page.
  -> IO ()
notebookInsertPageMenu self child tabLabel menuLabel position =
  {# call notebook_insert_page_menu #}
    (toNotebook self)
    (toWidget child)
    (toWidget tabLabel)
    (toWidget menuLabel)
    (fromIntegral position)
#endif

-- | Removes a page from the notebook given its index in the notebook.
--
notebookRemovePage :: NotebookClass self => self
 -> Int   -- ^ @pageNum@ - the index of a notebook page, starting from 0. If
          -- -1, the last page will be removed.
 -> IO ()
notebookRemovePage self pageNum =
  {# call notebook_remove_page #}
    (toNotebook self)
    (fromIntegral pageNum)

-- | Query the page the child widget is contained in.
--
-- * The function returns the page number if the child was found, Nothing
--   otherwise.
--
notebookPageNum :: (NotebookClass self, WidgetClass w) => self
 -> w
 -> IO (Maybe Int)
notebookPageNum nb child =
  liftM (\page -> if page==(-1) then Nothing else Just (fromIntegral page)) $
  {# call unsafe notebook_page_num #}
    (toNotebook nb)
    (toWidget child)

-- | Switches to the page number @pageNum@. Page numbers start from @0@.
--   Use @-1@ to request the last page.
--
-- * Note that due to historical reasons, GtkNotebook refuses
--   to switch to a page unless the child widget is visible.
--   Therefore, it is recommended to show child widgets before
--   adding them to a notebook.
--
notebookSetCurrentPage :: NotebookClass self => self
 -> Int   -- ^ @pageNum@ - index of the page to switch to, starting from 0. If
          -- negative, the last page will be used. If greater than the number
          -- of pages in the notebook, nothing will be done.
 -> IO ()
notebookSetCurrentPage self pageNum =
  {# call notebook_set_current_page #}
    (toNotebook self)
    (fromIntegral pageNum)

-- | Switches to the next page. Nothing happens if the current page is the
-- last page.
--
notebookNextPage :: NotebookClass self => self -> IO ()
notebookNextPage self =
  {# call notebook_next_page #}
    (toNotebook self)

-- | Switches to the previous page. Nothing happens if the current page is the
-- first page.
--
notebookPrevPage :: NotebookClass self => self -> IO ()
notebookPrevPage self =
  {# call notebook_prev_page #}
    (toNotebook self)

-- | Reorders the page containing @child@, so that it appears in position
-- @position@. If @position@ is greater than or equal to the number of children
-- in the list or negative, @child@ will be moved to the end of the list.
--
notebookReorderChild :: (NotebookClass self, WidgetClass child) => self
 -> child -- ^ @child@ - the child to move
 -> Int   -- ^ @position@ - the new position, or -1 to move to the end
 -> IO ()
notebookReorderChild self child position =
  {# call notebook_reorder_child #}
    (toNotebook self)
    (toWidget child)
    (fromIntegral position)

-- | Sets the edge at which the tabs for switching pages in the notebook are
-- drawn.
--
notebookSetTabPos :: NotebookClass self => self
 -> PositionType -- ^ @pos@ - the edge to draw the tabs at.
 -> IO ()
notebookSetTabPos self pos =
  {# call notebook_set_tab_pos #}
    (toNotebook self)
    ((fromIntegral . fromEnum) pos)

-- | Gets the edge at which the tabs for switching pages in the notebook are
-- drawn.
--
notebookGetTabPos :: NotebookClass self => self
 -> IO PositionType -- ^ returns the edge at which the tabs are drawn
notebookGetTabPos self =
  liftM (toEnum . fromIntegral) $
  {# call unsafe notebook_get_tab_pos #}
    (toNotebook self)

-- | Sets whether to show the tabs for the notebook or not.
--
notebookSetShowTabs :: NotebookClass self => self
 -> Bool  -- ^ @showTabs@ - @True@ if the tabs should be shown.
 -> IO ()
notebookSetShowTabs self showTabs =
  {# call notebook_set_show_tabs #}
    (toNotebook self)
    (fromBool showTabs)

-- | Returns whether the tabs of the notebook are shown. See
-- 'notebookSetShowTabs'.
--
notebookGetShowTabs :: NotebookClass self => self
 -> IO Bool -- ^ returns @True@ if the tabs are shown
notebookGetShowTabs self =
  liftM toBool $
  {# call unsafe notebook_get_show_tabs #}
    (toNotebook self)

-- | Sets whether a bevel will be drawn around the notebook pages. This only
-- has a visual effect when the tabs are not shown. See 'notebookSetShowTabs'.
--
notebookSetShowBorder :: NotebookClass self => self
 -> Bool  -- ^ @showBorder@ - @True@ if a bevel should be drawn around the
          -- notebook.
 -> IO ()
notebookSetShowBorder self showBorder =
  {# call notebook_set_show_border #}
    (toNotebook self)
    (fromBool showBorder)

-- | Returns whether a bevel will be drawn around the notebook pages. See
-- 'notebookSetShowBorder'.
--
notebookGetShowBorder :: NotebookClass self => self
 -> IO Bool -- ^ returns @True@ if the bevel is drawn
notebookGetShowBorder self =
  liftM toBool $
  {# call unsafe notebook_get_show_border #}
    (toNotebook self)

-- | Sets whether the tab label area will have arrows for scrolling if there
-- are too many tabs to fit in the area.
--
notebookSetScrollable :: NotebookClass self => self
 -> Bool  -- ^ @scrollable@ - @True@ if scroll arrows should be added
 -> IO ()
notebookSetScrollable self scrollable =
  {# call unsafe notebook_set_scrollable #}
    (toNotebook self)
    (fromBool scrollable)

-- | Returns whether the tab label area has arrows for scrolling. See
-- 'notebookSetScrollable'.
--
notebookGetScrollable :: NotebookClass self => self
 -> IO Bool -- ^ returns @True@ if arrows for scrolling are present
notebookGetScrollable self =
  liftM toBool $
  {# call unsafe notebook_get_scrollable #}
    (toNotebook self)

#if GTK_MAJOR_VERSION < 3
#ifndef DISABLE_DEPRECATED
-- | Sets the width the border around the tab labels in a notebook. This is
-- equivalent to calling @'notebookSetTabHBorder' notebook borderWidth@
-- followed by @'notebookSetTabVBorder' notebook borderWidth@.
--
-- * Warning: this function is deprecated and should not be used in
-- newly-written code.
--
-- Removed in Gtk3.
notebookSetTabBorder :: NotebookClass self => self
 -> Int   -- ^ @borderWidth@ - width of the border around the tab labels.
 -> IO ()
notebookSetTabBorder self borderWidth =
  {# call notebook_set_tab_border #}
    (toNotebook self)
    (fromIntegral borderWidth)

-- | Sets the width of the horizontal border of tab labels.
--
-- * Warning: this function is deprecated and should not be used in
-- newly-written code.
--
-- Removed in Gtk3.
notebookSetTabHBorder :: NotebookClass self => self
 -> Int   -- ^ @tabHborder@ - width of the horizontal border of tab labels.
 -> IO ()
notebookSetTabHBorder self tabHborder =
  {# call notebook_set_tab_hborder #}
    (toNotebook self)
    (fromIntegral tabHborder)

-- | Sets the width of the vertical border of tab labels.
--
-- * Warning: this function is deprecated and should not be used in
-- newly-written code.
--
-- Removed in Gtk3.
notebookSetTabVBorder :: NotebookClass self => self
 -> Int   -- ^ @tabVborder@ - width of the vertical border of tab labels.
 -> IO ()
notebookSetTabVBorder self tabVborder =
  {# call notebook_set_tab_vborder #}
    (toNotebook self)
    (fromIntegral tabVborder)
#endif
#endif

-- | Enables or disables the popup menu: if the user clicks with the right
-- mouse button on the bookmarks, a menu with all the pages will be popped up.
--
notebookSetPopup :: NotebookClass self => self -> Bool -> IO ()
notebookSetPopup self enable =
  (if enable
     then {#call notebook_popup_enable#}
     else {#call notebook_popup_disable#})
    (toNotebook self)

-- | Returns the page number of the current page.
--
notebookGetCurrentPage :: NotebookClass self => self
 -> IO Int -- ^ returns the index (starting from 0) of the current page in the
           -- notebook. If the notebook has no pages, then -1 will be returned.
notebookGetCurrentPage self =
  liftM fromIntegral $
  {# call unsafe notebook_get_current_page #}
    (toNotebook self)

-- | Changes the menu label for the page containing @child@.
--
notebookSetMenuLabel :: (NotebookClass self, WidgetClass child, WidgetClass menuLabel) => self
 -> child           -- ^ @child@ - the child widget
 -> Maybe menuLabel -- ^ @menuLabel@ - the menu label, or @Nothing@ for
                    -- default
 -> IO ()
notebookSetMenuLabel self child menuLabel =
  {# call notebook_set_menu_label #}
    (toNotebook self)
    (toWidget child)
    (maybe (Widget nullForeignPtr) toWidget menuLabel)

-- | Retrieves the menu label widget of the page containing @child@.
--
notebookGetMenuLabel :: (NotebookClass self, WidgetClass child) => self
 -> child             -- ^ @child@ - a widget contained in a page of
                      -- @notebook@
 -> IO (Maybe Widget) -- ^ returns the menu label, or @Nothing@ if the
                      -- notebook page does not have a menu label other than
                      -- the default (the tab label).
notebookGetMenuLabel self child =
  maybeNull (makeNewObject mkWidget) $
  {# call unsafe notebook_get_menu_label #}
    (toNotebook self)
    (toWidget child)

-- | Creates a new label and sets it as the menu label of @child@.
--
notebookSetMenuLabelText :: (NotebookClass self, WidgetClass child, GlibString string) => self
 -> child  -- ^ @child@ - the child widget
 -> string -- ^ @menuText@ - the label text
 -> IO ()
notebookSetMenuLabelText self child menuText =
  withUTFString menuText $ \menuTextPtr ->
  {# call notebook_set_menu_label_text #}
    (toNotebook self)
    (toWidget child)
    menuTextPtr

-- | Retrieves the text of the menu label for the page containing @child@.
--
notebookGetMenuLabelText :: (NotebookClass self, WidgetClass child, GlibString string) => self
 -> child             -- ^ @child@ - the child widget of a page of the
                      -- notebook.
 -> IO (Maybe string) -- ^ returns value: the text of the tab label, or
                      -- @Nothing@ if the widget does not have a menu label
                      -- other than the default menu label, or the menu label
                      -- widget is not a 'Label'.
notebookGetMenuLabelText self child =
  {# call unsafe notebook_get_menu_label_text #}
    (toNotebook self)
    (toWidget child)
  >>= maybePeek peekUTFString

-- | Returns the child widget contained in page number @pageNum@.
--
notebookGetNthPage :: NotebookClass self => self
 -> Int               -- ^ @pageNum@ - the index of a page in the noteobok, or
                      -- -1 to get the last page.
 -> IO (Maybe Widget) -- ^ returns the child widget, or @Nothing@ if @pageNum@
                      -- is out of bounds.
notebookGetNthPage self pageNum =
  maybeNull (makeNewObject mkWidget) $
  {# call unsafe notebook_get_nth_page #}
    (toNotebook self)
    (fromIntegral pageNum)

#if GTK_CHECK_VERSION(2,2,0)
-- | Gets the number of pages in a notebook.
--
-- * Available since Gtk version 2.2
--
notebookGetNPages :: NotebookClass self => self -> IO Int
notebookGetNPages self =
  liftM fromIntegral $
  {# call unsafe notebook_get_n_pages #}
    (toNotebook self)
#endif

-- | Returns the tab label widget for the page @child@. @Nothing@ is returned
-- if @child@ is not in @notebook@ or if no tab label has specifically been set
-- for @child@.
--
notebookGetTabLabel :: (NotebookClass self, WidgetClass child) => self
 -> child             -- ^ @child@ - the page
 -> IO (Maybe Widget) -- ^ returns the tab label
notebookGetTabLabel self child =
  maybeNull (makeNewObject mkWidget) $
  {# call unsafe notebook_get_tab_label #}
    (toNotebook self)
    (toWidget child)

-- | Retrieves the text of the tab label for the page containing @child@.
--
notebookGetTabLabelText :: (NotebookClass self, WidgetClass child, GlibString string) => self
 -> child             -- ^ @child@ - a widget contained in a page of
                      -- @notebook@
 -> IO (Maybe string) -- ^ returns value: the text of the tab label, or
                      -- @Nothing@ if the tab label widget is not a 'Label'.
notebookGetTabLabelText self child =
  {# call unsafe notebook_get_tab_label_text #}
    (toNotebook self)
    (toWidget child)
  >>= maybePeek peekUTFString

#if GTK_MAJOR_VERSION < 3
-- | Query the packing attributes for the tab label of the page containing
-- @child@.
--
-- Removed in Gtk3.
notebookQueryTabLabelPacking :: (NotebookClass self, WidgetClass child) => self
 -> child            -- ^ @child@ - the page
 -> IO (Packing,PackType)
notebookQueryTabLabelPacking self child =
  alloca $ \expPtr ->
  alloca $ \fillPtr ->
  alloca $ \packPtr -> do
  {# call unsafe notebook_query_tab_label_packing #}
    (toNotebook self)
    (toWidget child)
    expPtr
    fillPtr
    packPtr
  expand <- liftM toBool $ peek expPtr
  fill <- liftM toBool $ peek fillPtr
  pt <- liftM (toEnum . fromIntegral) $ peek packPtr
  return (toPacking expand fill, pt)

-- | Sets the packing parameters for the tab label of the page containing
-- @child@. See 'Graphics.UI.Gtk.Abstract.Box.boxPackStart' for the exact
-- meaning of the parameters.
--
-- Removed in Gtk3.
notebookSetTabLabelPacking :: (NotebookClass self, WidgetClass child) => self
 -> child    -- ^ @child@ - the child widget
 -> Packing
 -> PackType -- ^ @packType@ - the position of the bookmark
 -> IO ()
notebookSetTabLabelPacking self child pack packType =
  {# call notebook_set_tab_label_packing #}
    (toNotebook self)
    (toWidget child)
    (fromBool expand)
    (fromBool fill)
    ((fromIntegral . fromEnum) packType)
  where (expand, fill) = fromPacking pack

#ifndef DISABLE_DEPRECATED
-- | Sets whether the tabs must have all the same size or not.
--
-- * Warning: this function is deprecated and should not be used in
-- newly-written code.
--
-- Removed in Gtk3.
notebookSetHomogeneousTabs :: NotebookClass self => self
 -> Bool  -- ^ @homogeneous@ - @True@ if all tabs should be the same size.
 -> IO ()
notebookSetHomogeneousTabs self homogeneous =
  {# call notebook_set_homogeneous_tabs #}
    (toNotebook self)
    (fromBool homogeneous)
#endif
#endif

-- | Changes the tab label for @child@.
--
notebookSetTabLabel :: (NotebookClass self, WidgetClass child, WidgetClass tabLabel) => self
 -> child    -- ^ @child@ - the page
 -> tabLabel -- ^ @tabLabel@ - the tab label widget to use
 -> IO ()
notebookSetTabLabel self child tabLabel =
  {# call notebook_set_tab_label #}
    (toNotebook self)
    (toWidget child)
    (toWidget tabLabel)

-- | Creates a new label and sets it as the tab label for the page containing
-- @child@.
--
notebookSetTabLabelText :: (NotebookClass self, WidgetClass child, GlibString string) => self
 -> child  -- ^ @child@ - the page
 -> string -- ^ @tabText@ - the label text
 -> IO ()
notebookSetTabLabelText self child tabText =
  withUTFString tabText $ \tabTextPtr ->
  {# call notebook_set_tab_label_text #}
    (toNotebook self)
    (toWidget child)
    tabTextPtr

#if GTK_CHECK_VERSION(2,10,0)
-- | Sets whether the notebook tab can be reordered via drag and drop or not.
--
-- * Available since Gtk version 2.10
--
notebookSetTabReorderable :: (NotebookClass self, WidgetClass child) => self
 -> child   -- ^ @child@ - a child page
 -> Bool   -- ^ @reorderable@ - whether the tab is reorderable or not.
 -> IO ()
notebookSetTabReorderable self child reorderable =
  {# call notebook_set_tab_reorderable #}
    (toNotebook self)
    (toWidget child)
    (fromBool reorderable)

-- | Gets whether the tab can be reordered via drag and drop or not.
--
-- * Available since Gtk version 2.10
--
notebookGetTabReorderable :: (NotebookClass self, WidgetClass child) => self
 -> child  -- ^ @child@ - the child page
 -> IO Bool  -- ^ return @True@ if the tab is reorderable.
notebookGetTabReorderable self child = liftM toBool $
  {# call notebook_get_tab_reorderable #}
    (toNotebook self)
    (toWidget child)

-- | Sets whether the tab can be detached from notebook to another notebook or widget.
--
-- Note that 2 notebooks must share a common group identificator (see gtk_notebook_set_group_id()) to allow automatic tabs interchange between them.
--
-- If you want a widget to interact with a notebook through DnD (i.e.: accept dragged tabs from it) it must be set as a drop destination and accept the target "GTK_NOTEBOOK_TAB".
-- The notebook will fill the selection with a GtkWidget** pointing to the child widget that corresponds to the dropped tab.
--
-- If you want a notebook to accept drags from other widgets, you will have to set your own DnD code to do it.
--
-- * Available since Gtk version 2.10
--
notebookSetTabDetachable :: (NotebookClass self, WidgetClass child) => self
 -> child  -- ^ @child@ - the child page
 -> Bool  -- ^ @detachable@ - whether the tab is detachable or not
 -> IO ()
notebookSetTabDetachable self child detachable =
  {# call notebook_set_tab_detachable #}
    (toNotebook self)
    (toWidget child)
    (fromBool detachable)

-- | Returns whether the tab contents can be detached from notebook.
--
-- * Available since Gtk version 2.10
--
notebookGetTabDetachable :: (NotebookClass self, WidgetClass child) => self
 -> child  -- ^ @child@ - the child page
 -> IO Bool  -- ^ return @True@ if the tab is detachable.
notebookGetTabDetachable self child = liftM toBool $
  {# call notebook_get_tab_detachable #}
    (toNotebook self)
    (toWidget child)
#endif

#if GTK_CHECK_VERSION(2,20,0)
-- | Sets widget as one of the action widgets. Depending on the pack type the widget will be placed
-- before or after the tabs. You can use a 'Box' if you need to pack more than one widget on the same
-- side.
--
-- Note that action widgets are "internal" children of the notebook and thus not included in the list
-- returned from 'containerForeach'.
--
-- * Available since Gtk version 2.20
--
notebookSetActionWidget :: (NotebookClass self, WidgetClass widget) => self
                        -> widget
                        -> PackType -- ^ @packType@ pack type of the action widget
                        -> IO ()
notebookSetActionWidget self widget packType =
  {#call gtk_notebook_set_action_widget #}
    (toNotebook self)
    (toWidget widget)
    ((fromIntegral . fromEnum) packType)

-- | Gets one of the action widgets. See 'notebookSetActionWidget'.
--
-- * Available since Gtk version 2.20
--
notebookGetActionWidget :: NotebookClass self => self
                        -> PackType -- ^ @packType@ pack type of the action widget to receive
                        -> IO (Maybe Widget)
notebookGetActionWidget self packType =
    maybeNull (makeNewObject mkWidget) $
    {#call gtk_notebook_get_action_widget #}
      (toNotebook self)
      ((fromIntegral . fromEnum) packType)
#endif

--------------------
-- Attributes

-- | The index of the current page.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
notebookPage :: NotebookClass self => Attr self Int
notebookPage = newAttrFromIntProperty "page"

-- | Which side of the notebook holds the tabs.
--
-- Default value: 'PosTop'
--
notebookTabPos :: NotebookClass self => Attr self PositionType
notebookTabPos = newAttr
  notebookGetTabPos
  notebookSetTabPos

-- | Width of the border around the tab labels.
--
-- Default value: 2
--
notebookTabBorder :: NotebookClass self => WriteAttr self Int
notebookTabBorder = writeAttrFromUIntProperty "tab-border"

-- | Width of the horizontal border of tab labels.
--
-- Default value: 2
--
notebookTabHborder :: NotebookClass self => Attr self Int
notebookTabHborder = newAttrFromUIntProperty "tab-hborder"

-- | Width of the vertical border of tab labels.
--
-- Default value: 2
--
notebookTabVborder :: NotebookClass self => Attr self Int
notebookTabVborder = newAttrFromUIntProperty "tab-vborder"

-- | Whether tabs should be shown or not.
--
-- Default value: @True@
--
notebookShowTabs :: NotebookClass self => Attr self Bool
notebookShowTabs = newAttr
  notebookGetShowTabs
  notebookSetShowTabs

-- | Whether the border should be shown or not.
--
-- Default value: @True@
--
notebookShowBorder :: NotebookClass self => Attr self Bool
notebookShowBorder = newAttr
  notebookGetShowBorder
  notebookSetShowBorder

-- | If @True@, scroll arrows are added if there are too many tabs to fit.
--
-- Default value: @False@
--
notebookScrollable :: NotebookClass self => Attr self Bool
notebookScrollable = newAttr
  notebookGetScrollable
  notebookSetScrollable

-- | If @True@, pressing the right mouse button on the notebook pops up a menu
-- that you can use to go to a page.
--
-- Default value: @False@
--
notebookEnablePopup :: NotebookClass self => Attr self Bool
notebookEnablePopup = newAttrFromBoolProperty "enable-popup"

-- | Whether tabs should have homogeneous sizes.
--
-- Default value: @False@
--
notebookHomogeneous :: NotebookClass self => Attr self Bool
notebookHomogeneous = newAttrFromBoolProperty "homogeneous"

-- | Switches to the page number @pageNum@.
--
-- Note that due to historical reasons, 'Notebook' refuses to switch to a
-- page unless the child widget is visible. Therefore, it is recommended to
-- show child widgets before adding them to a notebook.
--
-- Returns the page number of the current page.
--
notebookCurrentPage :: NotebookClass self => Attr self Int
notebookCurrentPage = newAttr
  notebookGetCurrentPage
  notebookSetCurrentPage

--------------------
-- Child Attributes

-- | The string displayed on the child's tab label.
--
-- Default value: @Nothing@
--
notebookChildTabLabel :: (NotebookClass self, WidgetClass child, GlibString string) => child -> Attr self string
notebookChildTabLabel = newAttrFromContainerChildStringProperty "tab-label"

-- | The string displayed in the child's menu entry.
--
-- Default value: @Nothing@
--
notebookChildMenuLabel :: (NotebookClass self, WidgetClass child, GlibString string) => child -> Attr self string
notebookChildMenuLabel = newAttrFromContainerChildStringProperty "menu-label"

-- | The index of the child in the parent.
--
-- Allowed values: >= -1
--
-- Default value: 0
--
notebookChildPosition :: (NotebookClass self, WidgetClass child) => child -> Attr self Int
notebookChildPosition = newAttrFromContainerChildIntProperty "position"

-- | The packing style of the child's tab.
--
-- Default value: 'PackGrow'
--
notebookChildTabPacking :: (NotebookClass self, WidgetClass child) => child -> Attr self Packing
notebookChildTabPacking child = newAttr
  (\container -> do
     expand <- containerChildGetPropertyBool "tab-expand" child container
     fill   <- containerChildGetPropertyBool "tab-fill"   child container
     return (toPacking expand fill))
  (\container packing ->
     case fromPacking packing of
       (expand, fill) -> do
         containerChildSetPropertyBool "tab-expand" child container expand
         containerChildSetPropertyBool "tab-fill"   child container fill)

-- | A 'PackType' indicating whether the child is packed with reference to the
-- start or end of the parent.
--
-- Default value: 'PackStart'
--
notebookChildTabPackType :: (NotebookClass self, WidgetClass child) => child -> Attr self PackType
notebookChildTabPackType = newAttrFromContainerChildEnumProperty "tab-pack"
                         {# call pure unsafe gtk_pack_type_get_type #}

-- | Whether the tab is detachable.
--
-- Default value: @False@
--
notebookChildDetachable :: NotebookClass self => Attr self Bool
notebookChildDetachable = newAttrFromBoolProperty "detachable"

-- | Whether the tab is reorderable by user action or not.
--
-- Default value: @False@
--
notebookChildReorderable :: NotebookClass self => Attr self Bool
notebookChildReorderable = newAttrFromBoolProperty "reorderable"

-- | Whether to expand the child's tab or not.
--
-- Default value : @False@
--
notebookChildTabExpand :: NotebookClass self => Attr self Bool
notebookChildTabExpand = newAttrFromBoolProperty "tab-expand"

-- | Whether the child's tab should fill the allocated area or not.
--
-- Default value : @False@
--
notebookChildTabFill :: NotebookClass self => Attr self Bool
notebookChildTabFill = newAttrFromBoolProperty "tab-fill"

#if GTK_CHECK_VERSION(2,10,0)
-- | The 'notebookStyleArrowSpacing' property defines the spacing between the scroll arrows and the tabs.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
-- * Available since Gtk version 2.10
--
notebookStyleArrowSpacing :: NotebookClass self => ReadAttr self Bool
notebookStyleArrowSpacing = readAttrFromBoolProperty "arrow-spacing"
#endif

-- | The 'notebookStyleHasBackwardStepper' property determines whether the standard backward arrow button is displayed.
--
-- Default value: @True@
--
-- * Available since Gtk version 2.4
--
notebookStyleHasBackwardStepper :: NotebookClass self => ReadAttr self Bool
notebookStyleHasBackwardStepper = readAttrFromBoolProperty "has-backward-stepper"

-- | The 'notebookStyleHasForwardStepper' property determines whether the standard forward arrow button is displayed.
--
-- Default value : @True@
--
-- * Available since Gtk version 2.4
--
notebookStyleHasForwardStepper :: NotebookClass self => ReadAttr self Bool
notebookStyleHasForwardStepper = readAttrFromBoolProperty "has-forward-stepper"

-- | The 'notebookStyleHasSecondaryBackwardStepper' property determines whether a second backward arrow button is displayed on the opposite end of the tab area.
--
-- Default value: @False@
--
-- * Available since Gtk version 2.4
--
notebookStyleHasSecondaryBackwardStepper :: NotebookClass self => ReadAttr self Bool
notebookStyleHasSecondaryBackwardStepper = readAttrFromBoolProperty "has-secondary-backward-stepper"

-- | The 'notebookStyleHasSecondaryForwardStepper' property determines whether a second forward arrow button is displayed on the opposite end of the tab area.
--
-- Default value: @False@
--
-- * Available since Gtk version 2.4
--
notebookStyleHasSecondaryForwardStepper :: NotebookClass self => ReadAttr self Bool
notebookStyleHasSecondaryForwardStepper = readAttrFromBoolProperty "has-secondary-forward-stepper"

#if GTK_CHECK_VERSION(2,10,0)
-- | The 'notebookStyleTabCurvature' property defines size of tab curvature.
--
-- Allowed values: >= 0
--
-- Default value: 1
--
-- * Available since Gtk version 2.10
--
notebookStyleTabCurvature :: NotebookClass self => ReadAttr self Int
notebookStyleTabCurvature = readAttrFromIntProperty "tab-curvature"

-- | The 'notebookStyleTabOverlap' property defines size of tab overlap area.
--
-- Default value: 2
--
-- * Available since Gtk version 2.10
--
notebookStyleTabOverlap :: NotebookClass self => ReadAttr self Int
notebookStyleTabOverlap = readAttrFromIntProperty "tab-overlap"
#endif

--------------------
-- Signals

-- | Emitted when the user or a function changes the current page.
--
switchPage :: NotebookClass self => Signal self (Int -> IO ())
switchPage = Signal (\after obj act ->
                     connect_PTR_WORD__NONE "switch-page" after obj
                     (\_ page -> act (fromIntegral page)))

#if GTK_CHECK_VERSION(2,10,0)
-- | The 'pageReordered' signal is emitted in the notebook right after a page has been reordered.
--
-- * Available since Gtk+ version 2.10
--
pageReordered :: NotebookClass self => Signal self (Widget -> Int -> IO ())
pageReordered = Signal (connect_OBJECT_INT__NONE "page-reordered")

-- | The 'pageRemoved' signal is emitted in the notebook right after a page is removed from the notebook.
--
-- * Available since Gtk+ version 2.10
--
pageRemoved :: NotebookClass self => Signal self (Widget -> Int -> IO ())
pageRemoved = Signal (connect_OBJECT_INT__NONE "page-removed")

-- | The 'pageAdded' signal is emitted in the notebook right after a page is added to the notebook.
--
-- * Available since Gtk+ version 2.10
--
pageAdded :: NotebookClass self => Signal self (Widget -> Int -> IO ())
pageAdded = Signal (connect_OBJECT_INT__NONE "page-added")
#endif

-- * Deprecated
#ifndef DISABLE_DEPRECATED

-- | This signal is emitted when a new page is
-- selected.
--
onSwitchPage, afterSwitchPage :: NotebookClass nb => nb -> (Int -> IO ()) ->
                                 IO (ConnectId nb)
onSwitchPage nb fun = connect_BOXED_WORD__NONE "switch-page"
                      (const $ return ()) False nb
                      (\_ page -> fun (fromIntegral page))
afterSwitchPage nb fun = connect_BOXED_WORD__NONE "switch-page"
                         (const $ return ()) True nb
                         (\_ page -> fun (fromIntegral page))

#endif
