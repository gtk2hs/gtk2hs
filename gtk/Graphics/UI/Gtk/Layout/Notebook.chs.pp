-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Notebook
--
--  Author : Axel Simon
--
--  Created: 15 May 2001
--
--  Version $Revision: 1.3 $ from $Date: 2005/02/25 01:11:34 $
--
--  Copyright (C) 1999-2005 Axel Simon
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
-- The signals focus-tab and select-page are not bound because it is unclear
--   what they mean. As far as I can see they are not emitted anywhere.
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- A tabbed notebook container.
--
module Graphics.UI.Gtk.Layout.Notebook (
-- * Description
-- 
-- | The 'Notebook' widget is a 'Container' whose children are pages that can
-- be switched between using tab labels along one edge.
--
-- There are many configuration options for 'Notebook'. Among other things,
-- you can choose on which edge the tabs appear (see 'notebookSetTabPos'),
-- whether, if there are too many tabs to fit the noteobook should be made
-- bigger or scrolling arrows added (see 'notebookSetScrollable'), and
-- whether there will be a popup menu allowing the users to switch pages. (see
-- 'notebookEnablePopup', 'noteobookDisablePopup')

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
  castToNotebook,

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
  notebookSetScrollable,
  notebookGetScrollable,
#ifndef DISABLE_DEPRECATED
  notebookSetTabBorder,
  notebookSetTabHBorder,
  notebookSetTabVBorder,
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
  notebookQueryTabLabelPacking,
  notebookSetTabLabelPacking,
#ifndef DISABLE_DEPRECATED
  notebookSetHomogeneousTabs,
#endif
  notebookSetTabLabel,
  notebookSetTabLabelText,

-- * Signals
  onSwitchPage,
  afterSwitchPage
  ) where

import Monad	(liftM)
import Maybe	(maybe)

import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.Display.Label	(labelNew)
import Graphics.UI.Gtk.General.Enums	(Packing(..), PackType(..), PositionType(..))

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Create a new notebook.
--
notebookNew :: IO Notebook
notebookNew  = makeNewObject mkNotebook $ 
  liftM castPtr {#call unsafe notebook_new#}

--------------------
-- Methods

#if GTK_CHECK_VERSION(2,4,0)
-- | Insert a new tab to the right of the existing tabs.
--
-- * The given label will be used for the label widget of the new tab. In case
-- the context menu is enabled, this name will also appear in the popup menu. If
-- you want to specify something else to go in the tab, use
-- 'notebookAppendPageMenu'.
--
-- * Returns index (starting from 0) of the appended page in the notebook, or -1
-- if the function fails.
--
-- * This function returned @()@ in Gtk version 2.2.X and earlier
--
notebookAppendPage :: (NotebookClass nb, WidgetClass child) => nb
                   -> child   -- ^ Widget to use as the contents of the page
                   -> String  -- ^ Label for the page.
                   -> IO Int
notebookAppendPage nb child tabLabel = do
  tab <- labelNew (Just tabLabel)
  liftM fromIntegral $
    {#call notebook_append_page#} (toNotebook nb) (toWidget child) 
      (toWidget tab)

#else
-- | Insert a new tab to the right of the existing tabs.
--
-- * The given label will be used for the label widget of the new tab. In case
-- the context popup menu is enabled, this name will also appear in the menu. If
-- you want to specify something else to go in the tab, use
-- 'notebookAppendPageMenu'.
--
-- * This function returns @Int@ in Gtk version 2.4.0 and later.
--
notebookAppendPage :: (NotebookClass nb, WidgetClass child) => nb
                   -> child   -- ^ Widget to use as the contents of the page
                   -> String  -- ^ Label for the page.
                   -> IO ()
notebookAppendPage nb child tabLabel = do
  tab <- labelNew (Just tabLabel)
  {#call notebook_append_page#} (toNotebook nb) (toWidget child) (toWidget tab)
#endif

#if GTK_CHECK_VERSION(2,4,0)
-- | Insert a new tab to the right of the existing tabs.
--
-- Like 'notebookAppendPage' but allows any widget to be used for the label of
-- the new tab and then entry in the page-switch popup menu.
--
-- * Returns the index (starting from 0) of the appended page in the notebook,
-- or -1 if the function fails.
--
-- * This function returned @()@ in Gtk version 2.2.X and earlier
--
notebookAppendPageMenu :: (NotebookClass nb, WidgetClass child,
   WidgetClass tab, WidgetClass menu) => nb
  -> child  -- ^ Widget to use as the contents of the page
  -> tab    -- ^ Tab label widget for the page.
  -> menu   -- ^ Menu entry for this tab (usually a 'Label' widget).
  -> IO Int
notebookAppendPageMenu nb child tabWidget menuWidget = liftM fromIntegral $
  {#call notebook_append_page_menu#} (toNotebook nb) (toWidget child)
    (toWidget tabWidget) (toWidget menuWidget)

#else
-- | Insert a new tab to the right of the existing tabs.
--
-- Like 'notebookAppendPage' but allows any widget to be used for the label of
-- the new tab and then entry in the page-switch popup menu.
--
-- * This function returns @Int@ in Gtk version 2.4.0 and later
--
notebookAppendPageMenu :: (NotebookClass nb, WidgetClass child,
   WidgetClass tab, WidgetClass menu) => nb
  -> child  -- ^ Widget to use as the contents of the page
  -> tab    -- ^ Tab label widget for the page.
  -> menu   -- ^ Menu entry for this tab (usually a 'Label' widget).
  -> IO ()
notebookAppendPageMenu nb child tabWidget menuWidget =
  {#call notebook_append_page_menu#} (toNotebook nb) (toWidget child)
    (toWidget tabWidget) (toWidget menuWidget)
#endif

#if GTK_CHECK_VERSION(2,4,0)
-- | Insert a new tab to the left of the existing tabs.
--
-- * The given label will be used for the label widget of the new tab. In case
-- the context menu is enabled, this name will also appear in the popup menu. If
-- you want to specify something else to go in the tab, use
-- 'notebookPrependPageMenu'.
--
-- * Returns index (starting from 0) of the prepended page in the notebook, or -1
-- if the function fails.
--
-- * This function returned @()@ in Gtk version 2.2.X and earlier
--
notebookPrependPage :: (NotebookClass nb, WidgetClass child) => nb
                   -> child   -- ^ Widget to use as the contents of the page
                   -> String  -- ^ Label for the page.
                   -> IO Int
notebookPrependPage nb child tabLabel = do
  tab <- labelNew (Just tabLabel)
  liftM fromIntegral $
    {#call notebook_prepend_page#} (toNotebook nb) (toWidget child) 
      (toWidget tab)

#else
-- | Insert a new tab to the left of the existing tabs.
--
-- * The given label will be used for the label widget of the new tab. In case
-- the context popup menu is enabled, this name will also appear in the menu. If
-- you want to specify something else to go in the tab, use
-- 'notebookPrependPageMenu'.
--
-- * This function returns @Int@ in Gtk version 2.4.0 and later.
--
notebookPrependPage :: (NotebookClass nb, WidgetClass child) => nb
                   -> child   -- ^ Widget to use as the contents of the page
                   -> String  -- ^ Label for the page.
                   -> IO ()
notebookPrependPage nb child tabLabel = do
  tab <- labelNew (Just tabLabel)
  {#call notebook_prepend_page#} (toNotebook nb) (toWidget child) 
    (toWidget tab)
#endif

#if GTK_CHECK_VERSION(2,4,0)
-- | Insert a new tab to the left of the existing tabs.
--
-- Like 'notebookPrependPage' but allows any widget to be used for the label of
-- the new tab and then entry in the page-switch popup menu.
--
-- * Returns the index (starting from 0) of the prepended page in the notebook,
-- or -1 if the function fails.
--
-- * This function returned @()@ in Gtk version 2.2.X and earlier
--
notebookPrependPageMenu :: (NotebookClass nb, WidgetClass child,
   WidgetClass tab, WidgetClass menu) => nb
  -> child  -- ^ Widget to use as the contents of the page
  -> tab    -- ^ Tab label widget for the page.
  -> menu   -- ^ Menu entry for this tab (usually a 'Label' widget).
  -> IO Int
notebookPrependPageMenu nb child tabWidget menuWidget = liftM fromIntegral $
  {#call notebook_prepend_page_menu#} (toNotebook nb) (toWidget child)
    (toWidget tabWidget) (toWidget menuWidget)

#else
-- | Insert a new tab to the left of the existing tabs.
--
-- Like 'notebookPrependPage' but allows any widget to be used for the label of
-- the new tab and then entry in the page-switch popup menu.
--
-- * This function returns @Int@ in Gtk version 2.4.0 and later
--
notebookPrependPageMenu :: (NotebookClass nb, WidgetClass child,
   WidgetClass tab, WidgetClass menu) => nb
  -> child  -- ^ Widget to use as the contents of the page
  -> tab    -- ^ Tab label widget for the page.
  -> menu   -- ^ Menu entry for this tab (usually a 'Label' widget).
  -> IO ()
notebookPrependPageMenu nb child tabWidget menuWidget =
  {#call notebook_prepend_page_menu#} (toNotebook nb) (toWidget child)
    (toWidget tabWidget) (toWidget menuWidget)
#endif

#if GTK_CHECK_VERSION(2,4,0)
-- | Insert a new tab at the specified position. That is between @pos@ and
-- @pos@+1, or -1 to append the page after all other pages.
--
-- * The given label will be used for the label widget of the new tab. In case
-- the context menu is enabled, this name will also appear in the popup menu. If
-- you want to specify something else to go in the tab, use
-- 'notebookInsertPageMenu'.
--
-- * Returns index (starting from 0) of the inserted page in the notebook, or -1
-- if the function fails.
--
-- * This function returned @()@ in Gtk version 2.2.X and earlier
--
notebookInsertPage :: (NotebookClass nb, WidgetClass child) => nb
                   -> child  -- ^ Widget to use as the contents of the page
                   -> String -- ^ Label for the page.
                   -> Int    -- ^ Position for the new page.
                   -> IO Int
notebookInsertPage nb child tabLabel pos = do
  tab <- labelNew (Just tabLabel)
  liftM fromIntegral $
    {#call notebook_insert_page#} (toNotebook nb) (toWidget child) 
      (toWidget tab) (fromIntegral pos)

#else
-- | Insert a new tab at the specified position. That is between @pos@ and
-- @pos@+1, or -1 to append the page after all other pages.
--
-- * The given label will be used for the label widget of the new tab. In case
-- the context menu is enabled, this name will also appear in the popup menu. If
-- you want to specify something else to go in the tab, use
-- 'notebookInsertPageMenu'.
--
-- * This function returns @Int@ in Gtk version 2.4.0 and later.
--
notebookInsertPage :: (NotebookClass nb, WidgetClass child) => nb
                   -> child  -- ^ Widget to use as the contents of the page
                   -> String -- ^ Label for the page.
                   -> Int    -- ^ Position for the new page.
                   -> IO ()
notebookInsertPage nb child tabLabel pos = do
  tab <- labelNew (Just tabLabel)
  {#call notebook_insert_page#} (toNotebook nb) (toWidget child) 
    (toWidget tab) (fromIntegral pos)
#endif

#if GTK_CHECK_VERSION(2,4,0)
-- | Insert a new tab at the specified position. That is between @pos@ and
-- @pos@+1, or -1 to append the page after all other pages.
--
-- Like 'notebookInsertPage' but allows any widget to be used for the label of
-- the new tab and then entry in the page-switch popup menu.
--
-- * Returns the index (starting from 0) of the inserted page in the notebook,
-- or -1 if the function fails.
--
-- * This function returned @()@ in Gtk version 2.2.X and earlier
--
notebookInsertPageMenu ::(NotebookClass nb, WidgetClass child, 
   WidgetClass tab, WidgetClass menu) => nb
  -> child  -- ^ Widget to use as the contents of the page
  -> tab    -- ^ Tab label widget for the page.
  -> menu   -- ^ Menu entry for this tab (usually a 'Label' widget).
  -> Int    -- ^ Position for the new page.
  -> IO Int
notebookInsertPageMenu nb child tabWidget menuWidget pos = liftM fromIntegral $
  {#call notebook_insert_page_menu#} (toNotebook nb) (toWidget child)
    (toWidget tabWidget) (toWidget menuWidget) (fromIntegral pos)

#else
-- | Insert a new tab at the specified position. That is between @pos@ and
-- @pos@+1, or -1 to append the page after all other pages.
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
notebookInsertPageMenu nb child tabWidget menuWidget pos =
  {#call notebook_insert_page_menu#} (toNotebook nb) (toWidget child)
    (toWidget tabWidget) (toWidget menuWidget) (fromIntegral pos)
#endif

-- | Remove a specific page from the notebook, counting from 0.
--
notebookRemovePage :: NotebookClass nb => nb -> Int -> IO ()
notebookRemovePage nb pos = 
  {#call notebook_remove_page#} (toNotebook nb) (fromIntegral pos)

-- | Query the page the child widget is contained in.
--
-- * The function returns the page number if the child was found, Nothing
--   otherwise.
--
notebookPageNum :: (NotebookClass nb, WidgetClass w) => nb -> w ->
                   IO (Maybe Int)
notebookPageNum nb child = 
  liftM (\page -> if page==(-1) then Nothing else Just (fromIntegral page)) $
  {#call unsafe notebook_page_num#} (toNotebook nb) (toWidget child)

-- | Move to the specified page of the notebook.
--
-- * If the position is out of range (e.g. negative) select the last page.
--
notebookSetCurrentPage :: NotebookClass nb => nb -> Int -> IO ()
notebookSetCurrentPage nb pos =
  {#call notebook_set_current_page#} (toNotebook nb) (fromIntegral pos)

-- | Move to the right neighbour of the current page.
--
-- * Nothing happens if there is no such page.
--
notebookNextPage :: NotebookClass nb => nb -> IO ()
notebookNextPage nb = {#call notebook_next_page#} (toNotebook nb)

-- | Move to the left neighbour of the current page.
--
-- * Nothing happens if there is no such page.
--
notebookPrevPage :: NotebookClass nb => nb -> IO ()
notebookPrevPage nb = {#call notebook_prev_page#} (toNotebook nb)

-- | Move a page withing the notebook.
--
notebookReorderChild :: (NotebookClass nb, WidgetClass w) => nb -> w -> Int ->
                        IO ()
notebookReorderChild nb child pos = {#call notebook_reorder_child#}
  (toNotebook nb) (toWidget child) (fromIntegral pos)

-- | Specify at which border the tabs should be drawn.
--
notebookSetTabPos :: NotebookClass nb => nb -> PositionType -> IO ()
notebookSetTabPos nb pt = {#call notebook_set_tab_pos#}
  (toNotebook nb) ((fromIntegral.fromEnum) pt)

-- | Gets the edge at which the tabs for switching pages in the notebook are
-- drawn.
--
notebookGetTabPos :: NotebookClass nb => nb -> IO PositionType
notebookGetTabPos nb = liftM (toEnum.fromIntegral) $
  {#call unsafe notebook_get_tab_pos#} (toNotebook nb)

-- | Show or hide the tabs of a notebook.
--
notebookSetShowTabs :: NotebookClass nb => nb -> Bool -> IO ()
notebookSetShowTabs nb showTabs = {#call notebook_set_show_tabs#}
  (toNotebook nb) (fromBool showTabs)

-- | Returns whether the tabs of the notebook are shown.
--
notebookGetShowTabs :: NotebookClass nb => nb -> IO Bool
notebookGetShowTabs nb =
  liftM toBool $ {#call unsafe notebook_get_show_tabs#} (toNotebook nb)

-- | In case the tabs are not shown, specify whether to draw a border around
-- the notebook.
--
notebookSetShowBorder :: NotebookClass nb => nb -> Bool -> IO ()
notebookSetShowBorder nb showBorder = {#call notebook_set_show_border#}
  (toNotebook nb) (fromBool showBorder)

-- | Returns whether a bevel will be drawn around the notebook pages.
--
notebookGetShowBorder :: NotebookClass nb => nb -> IO Bool
notebookGetShowBorder nb =
  liftM toBool $ {#call unsafe notebook_get_show_border#} (toNotebook nb)

-- | Set whether scroll bars will be added in case the notebook has too many
-- tabs to fit the widget size.
--
notebookSetScrollable :: NotebookClass nb => nb -> Bool -> IO ()
notebookSetScrollable nb scrollable = {#call unsafe notebook_set_scrollable#}
  (toNotebook nb) (fromBool scrollable)

-- | Returns whether the tab label area has arrows for scrolling.
--
notebookGetScrollable :: NotebookClass nb => nb -> IO Bool
notebookGetScrollable nb = liftM toBool $
  {#call unsafe notebook_get_scrollable#} (toNotebook nb)

#ifndef DISABLE_DEPRECATED
-- | Set the width of the borders of the tab labels.
--
-- * Sets both vertical and horizontal widths.
--
notebookSetTabBorder :: NotebookClass nb => nb -> Int -> IO ()
notebookSetTabBorder nb width = {#call notebook_set_tab_border#}
  (toNotebook nb) (fromIntegral width)

-- | Set the width of the borders of the tab labels.
--
-- * Sets horizontal widths.
--
notebookSetTabHBorder :: NotebookClass nb => nb -> Int -> IO ()
notebookSetTabHBorder nb width = {#call notebook_set_tab_hborder#}
  (toNotebook nb) (fromIntegral width)

-- | Set the width of the borders of the tab labels.
--
-- * Sets vertical widths.
--
notebookSetTabVBorder :: NotebookClass nb => nb -> Int -> IO ()
notebookSetTabVBorder nb width = {#call notebook_set_tab_vborder#}
  (toNotebook nb) (fromIntegral width)
#endif

-- | Enable or disable context menus with all tabs in it.
--
notebookSetPopup :: NotebookClass nb => nb -> Bool -> IO ()
notebookSetPopup nb enable = (if enable 
  then {#call notebook_popup_enable#} else {#call notebook_popup_disable#})
  (toNotebook nb)

-- | Query the currently selected page.
--
-- * Returns -1 if notebook has no pages.
--
notebookGetCurrentPage :: NotebookClass nb => nb -> IO Int
notebookGetCurrentPage nb = liftM fromIntegral $
  {#call unsafe notebook_get_current_page#} (toNotebook nb)

-- | Changes the menu label for the page containing the given child widget.
--
notebookSetMenuLabel :: (NotebookClass nb, WidgetClass ch, WidgetClass label)
                     => nb -> ch -> Maybe label -> IO ()
notebookSetMenuLabel nb child label =
  {#call notebook_set_menu_label#} (toNotebook nb) (toWidget child)
    (maybe (Widget nullForeignPtr) toWidget label)

-- | Extract the menu label from the given @child@.
--
-- * Returns Nothing if @child@ was not found.
--
notebookGetMenuLabel :: (NotebookClass nb, WidgetClass w) => nb -> w ->
                        IO (Maybe Label)
notebookGetMenuLabel nb child = do
  wPtr <- {#call unsafe notebook_get_menu_label#} 
    (toNotebook nb) (toWidget child)
  if wPtr==nullPtr then return Nothing else liftM Just $
    makeNewObject mkLabel $ return $ castPtr wPtr

-- | Creates a new label and sets it as the menu label of the given child
-- widget.
--
notebookSetMenuLabelText :: (NotebookClass nb, WidgetClass ch)
                         => nb -> ch -> String -> IO ()
notebookSetMenuLabelText nb child label =
  withUTFString label $ \labelPtr ->
  {#call notebook_set_menu_label_text#} (toNotebook nb)
    (toWidget child) labelPtr

-- | Retrieves the text of the menu label for the page containing the given
-- child widget.
--
notebookGetMenuLabelText :: (NotebookClass nb, WidgetClass ch)
                         => nb -> ch -> IO (Maybe String)
notebookGetMenuLabelText nb child = do
  labelPtr <- {#call unsafe notebook_get_menu_label_text#} (toNotebook nb)
    (toWidget child)
  maybePeek peekUTFString labelPtr

-- | Retrieve the child widget at the given position (starting from 0).
--
-- * Returns Nothing if the index is out of bounds.
--
notebookGetNthPage :: NotebookClass nb => nb -> Int -> IO (Maybe Widget)
notebookGetNthPage nb pos = do
  wPtr <- {#call unsafe notebook_get_nth_page#} 
    (toNotebook nb) (fromIntegral pos)
  if wPtr==nullPtr then return Nothing else liftM Just $
    makeNewObject mkWidget $ return wPtr

#if GTK_CHECK_VERSION(2,2,0)

-- | Get the number of pages in a notebook.
--
-- * Only available in Gtk 2.2 and higher.
--
notebookGetNPages :: NotebookClass nb => nb -> IO Int
notebookGetNPages nb = liftM fromIntegral $
  {#call unsafe notebook_get_n_pages#} (toNotebook nb)

#endif

-- | Extract the tab label from the given @child@.
--
-- * Nothing is returned if no tab label has specifically been set for the
-- child.
--
notebookGetTabLabel :: (NotebookClass nb, WidgetClass w) => nb -> w ->
                       IO (Maybe Widget)
notebookGetTabLabel nb child = do
  wPtr <- {#call unsafe notebook_get_tab_label#} 
    (toNotebook nb) (toWidget child)
  if wPtr==nullPtr then return Nothing else liftM Just $
    makeNewObject mkWidget $ return wPtr

-- | Retrieves the text of the tab label for the page containing the given child
-- widget.
--
notebookGetTabLabelText :: (NotebookClass nb, WidgetClass w) => nb -> w ->
                       IO (Maybe String)
notebookGetTabLabelText nb child = do
  labelPtr <- {#call unsafe notebook_get_tab_label_text#} (toNotebook nb)
    (toWidget child)
  maybePeek peekUTFString labelPtr

-- | Query the packing attributes of the given child.
--
notebookQueryTabLabelPacking :: (NotebookClass nb, WidgetClass w) => nb -> w ->
                                IO (Packing,PackType)
notebookQueryTabLabelPacking nb child = 
  alloca $ \expPtr -> alloca $ \fillPtr -> alloca $ \packPtr -> do
    {#call unsafe notebook_query_tab_label_packing#} (toNotebook nb)
      (toWidget child) expPtr fillPtr packPtr
    expand <- liftM toBool $ peek expPtr
    fill <- liftM toBool $ peek fillPtr
    pt <- liftM (toEnum.fromIntegral) $ peek packPtr
    return (if fill then PackGrow else 
             (if expand then PackRepel else PackNatural),
	    pt)

-- | Set the packing attributes of the given child.
--
notebookSetTabLabelPacking :: (NotebookClass nb, WidgetClass w) => nb -> w ->
                              Packing -> PackType -> IO ()
notebookSetTabLabelPacking nb child pack pt = 
  {#call notebook_set_tab_label_packing#} (toNotebook nb) (toWidget child)
    (fromBool $ pack/=PackNatural) (fromBool $ pack==PackGrow) 
    ((fromIntegral.fromEnum) pt)

#ifndef DISABLE_DEPRECATED
-- | Sets whether the tabs must have all the same size or not.
--
notebookSetHomogeneousTabs :: NotebookClass nb => nb -> Bool -> IO ()
notebookSetHomogeneousTabs nb hom = {#call notebook_set_homogeneous_tabs#}
  (toNotebook nb) (fromBool hom)
#endif

-- | Set a new tab label for a given page.
--
notebookSetTabLabel :: (NotebookClass nb, WidgetClass ch, WidgetClass tab) => 
                       nb -> ch -> tab -> IO ()
notebookSetTabLabel nb child tab = {#call notebook_set_tab_label#}
  (toNotebook nb) (toWidget child) (toWidget tab)

-- | Creates a new label and sets it as the tab label for the given page.
--
notebookSetTabLabelText :: (NotebookClass nb, WidgetClass ch) => 
                       nb -> ch -> String -> IO ()
notebookSetTabLabelText nb child label =
  withUTFString label $ \labelPtr ->
  {#call notebook_set_tab_label_text#} (toNotebook nb) (toWidget child) labelPtr

--------------------
-- Signals

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
