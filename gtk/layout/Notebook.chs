-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget Notebook@
--
--  Author : Axel Simon
--          
--  Created: 15 May 2001
--
--  Version $Revision: 1.4 $ from $Date: 2003/07/03 05:19:36 $
--
--  Copyright (c) 1999..2002 Axel Simon
--
--  This file is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This file is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
-- @description@ --------------------------------------------------------------
--
-- * This widget can display several pages of widgets. Each page can be 
--   selected by a tab at the top of the widget. It is useful in dialogs where
--   a lot of information has to be displayed.
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------
--
-- * check if it is sensible and possible to have something else than a Label
--   widget in the context menu. If so, change the notebook*PageMenu function
--   and add notebookSetMenuLabel and notebookSetMenuText.
--
-- * notebookSetTabLabelText is not bound.
--
-- * The signals focus-tab and select-page are not bound because it is unclear
--   what they mean. As far as I can see they are not emitted anywhere.
--
module Notebook(
  Notebook,
  NotebookClass,
  castToNotebook,
  notebookNew,
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
  notebookSetShowTabs,
  notebookSetShowBorder,
  notebookSetScrollable,
  notebookSetTabBorder,
  notebookSetTabHBorder,
  notebookSetTabVBorder,
  notebookSetPopup,
  notebookGetCurrentPage,
  notebookGetMenuLabel,
  notebookGetNthPage,
  notebookGetNPages,
  notebookGetTabLabel,
  Packing(..), PackType(..),
  notebookQueryTabLabelPacking,
  notebookSetTabLabelPacking,
  notebookSetHomogeneousTabs,
  notebookSetTabLabel,
  onSwitchPage,
  afterSwitchPage
  ) where

import Monad	(liftM)
import Maybe	(maybe)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Label	(labelNew)
import Enums	(Packing(..), PackType(..), PositionType(..))

{# context lib="gtk" prefix="gtk" #}

-- methods

-- @constructor notebookNew@ Create a new notebook.
--
notebookNew :: IO Notebook
notebookNew  = makeNewObject mkNotebook $ 
  liftM castPtr {#call unsafe notebook_new#}

-- @method notebookAppendPage@ Insert a new tab to the right of the existing
-- tabs.
--
-- * The @ref arg tabName@ will be inserted as a Label widget. In case the
--   context menu is enabled, this name will appear in the menu. If you want
--   to specify something else to go in the tab, use
--   @ref method notebookAppendPageMenu@ and specify some suitable widget for
--   @ref arg menuLabel@.
--
notebookAppendPage :: (NotebookClass nb, WidgetClass child) => nb -> child ->
                      String -> IO ()
notebookAppendPage nb child tabLabel = do
  tab <- labelNew (Just tabLabel)
  {#call notebook_append_page#} (toNotebook nb) (toWidget child) 
    (toWidget tab)

-- @method notebookAppendPageMenu@ Insert a new tab to the right of the
-- existing tabs. @ref arg menuLabel@ is the label for the context menu
-- (useful if @ref arg tabLabel@ is not a Label widget).
--
notebookAppendPageMenu ::(NotebookClass nb, WidgetClass child, 
   WidgetClass tab) => nb -> child -> tab -> String -> IO ()
notebookAppendPageMenu nb child tabWidget menuLabel = do
  menu <- labelNew (Just menuLabel)
  {#call notebook_append_page_menu#} (toNotebook nb) (toWidget child)
    (toWidget tabWidget) (toWidget menu)

-- @method notebookPrependPage@ Insert a new tab to the left of the existing
-- tabs.
--
-- * The @ref arg tabName@ will be inserted as a Label widget. In case the
--   context menu is enabled, this name will appear in the menu. If you want
--   to specify something else to go in the tab, use
--   @ref method notebookPrependPageMenu@ and specify some suitable widget for
--   @ref arg menuLabel@.
--
notebookPrependPage :: (NotebookClass nb, WidgetClass child) => nb -> child ->
                       String -> IO ()
notebookPrependPage nb child tabLabel = do
  tab <- labelNew (Just tabLabel)
  {#call notebook_prepend_page#} (toNotebook nb) (toWidget child) (toWidget tab)

-- @method notebookPrependPageMenu@ Insert a new tab to the left of the
-- existing tabs. @ref arg menuLabel@ is the label for the context menu
-- (useful if @ref arg tabLabel@ is not a Label widget).
--
notebookPrependPageMenu ::(NotebookClass nb, WidgetClass child, 
   WidgetClass tab) => nb -> child -> tab -> String -> IO ()
notebookPrependPageMenu nb child tabWidget menuLabel = do
  menu <- labelNew (Just menuLabel)
  {#call notebook_prepend_page_menu#} (toNotebook nb) (toWidget child)
    (toWidget tabWidget) (toWidget menu)


-- @method notebookInsertPage@ Insert a new tab at the specified position.
--
-- * The @ref arg tabName@ will be inserted as a Label widget. In case the
--   context menu is enabled, this name will appear in the menu. If you want
--   to specify something else to go in the tab, use
--   @ref method notebookInsertPageMenu@ and specify some suitable widget for
--   @ref arg menuLabel@.
--
notebookInsertPage :: (NotebookClass nb, WidgetClass child) => nb -> child ->
                      String -> Int -> IO ()
notebookInsertPage nb child tabLabel pos = do
  lbl <- labelNew (Just tabLabel)
  {#call notebook_insert_page#} (toNotebook nb) (toWidget child) 
     (toWidget lbl) (fromIntegral pos)

-- @method notebookInsertPageMenu@ Insert a new tab between the tab no.
-- @ref arg pos@ and @ref arg pos@+1. @ref arg menuLabel@ is the label for the
-- context menu (useful if @ref arg tabLabel@ is not a Label widget).
--
notebookInsertPageMenu ::(NotebookClass nb, WidgetClass child, 
   WidgetClass tab) => nb -> child -> tab -> String -> Int -> IO ()
notebookInsertPageMenu nb child tabWidget menuLabel pos = do
  menu <- labelNew (Just menuLabel)
  {#call notebook_insert_page_menu#} (toNotebook nb) (toWidget child)
    (toWidget tabWidget) (toWidget menu) (fromIntegral pos)

-- @method notebookRemovePage@ Remove a specific page from the notebook,
-- counting from 0.
--
notebookRemovePage :: NotebookClass nb => nb -> Int -> IO ()
notebookRemovePage nb pos = 
  {#call notebook_remove_page#} (toNotebook nb) (fromIntegral pos)

-- @method notebookPageNum@ Query the page the @ref arg child@ widget is
-- contained in.
--
-- * The function returns the page number if the child was found, Nothing
--   otherwise.
--
notebookPageNum :: (NotebookClass nb, WidgetClass w) => nb -> w ->
                   IO (Maybe Int)
notebookPageNum nb child = 
  liftM (\page -> if page==(-1) then Nothing else Just (fromIntegral page)) $
  {#call unsafe notebook_page_num#} (toNotebook nb) (toWidget child)

-- @method notebookSetCurrentPage@ Move to the specified page of the notebook.
--
-- * If @ref arg pos@ is out of range (e.g. negative) select the last page.
--
notebookSetCurrentPage :: NotebookClass nb => nb -> Int -> IO ()
notebookSetCurrentPage nb pos =
  {#call notebook_set_current_page#} (toNotebook nb) (fromIntegral pos)

-- @method notebookNextPage@ Move to the right neighbour of the current page.
--
-- * Nothing happens if there is no such page.
--
notebookNextPage :: NotebookClass nb => nb -> IO ()
notebookNextPage nb = {#call notebook_next_page#} (toNotebook nb)

-- @method notebookPrevPage@ Move to the left neighbour of the current page.
--
-- * Nothing happens if there is no such page.
--
notebookPrevPage :: NotebookClass nb => nb -> IO ()
notebookPrevPage nb = {#call notebook_prev_page#} (toNotebook nb)

-- @method notebookReorderChild@ Move a page withing the notebook.
--
notebookReorderChild :: (NotebookClass nb, WidgetClass w) => nb -> w -> Int ->
                        IO ()
notebookReorderChild nb child pos = {#call notebook_reorder_child#}
  (toNotebook nb) (toWidget child) (fromIntegral pos)

-- @method notebookSetTabPos@ Specify at which border the tabs should be
-- drawn.
--
notebookSetTabPos :: NotebookClass nb => nb -> PositionType -> IO ()
notebookSetTabPos nb pt = {#call notebook_set_tab_pos#}
  (toNotebook nb) ((fromIntegral.fromEnum) pt)

-- @method notebookSetShowTabs@ Show or hide the tabs of a notebook.
--
notebookSetShowTabs :: NotebookClass nb => nb -> Bool -> IO ()
notebookSetShowTabs nb showTabs = {#call notebook_set_show_tabs#}
  (toNotebook nb) (fromBool showTabs)

-- @method notebookSetShowBorder@ In case the tabs are not shown, specify
-- whether to draw a border around the notebook.
--
notebookSetShowBorder :: NotebookClass nb => nb -> Bool -> IO ()
notebookSetShowBorder nb showBorder = {#call notebook_set_show_border#}
  (toNotebook nb) (fromBool showBorder)

-- @method notebookSetScrollable@ Set whether scroll bars will be added in
-- case the notebook has too many tabs to fit the widget size.
--
notebookSetScrollable :: NotebookClass nb => nb -> Bool -> IO ()
notebookSetScrollable nb scrollable = {#call unsafe notebook_set_scrollable#}
  (toNotebook nb) (fromBool scrollable)

-- @method notebookSetTabBorder@ Set the width of the borders of the tab
-- labels.
--
-- * Sets both vertical and horizontal widths.
--
notebookSetTabBorder :: NotebookClass nb => nb -> Int -> IO ()
notebookSetTabBorder nb width = {#call notebook_set_tab_border#}
  (toNotebook nb) (fromIntegral width)

-- @method notebookSetTabHBorder@ Set the width of the borders of the tab
-- labels.
--
-- * Sets horizontal widths.
--
notebookSetTabHBorder :: NotebookClass nb => nb -> Int -> IO ()
notebookSetTabHBorder nb width = {#call notebook_set_tab_hborder#}
  (toNotebook nb) (fromIntegral width)

-- @method notebookSetTabVBorder@ Set the width of the borders of the tab
-- labels.
--
-- * Sets vertical widths.
--
notebookSetTabVBorder :: NotebookClass nb => nb -> Int -> IO ()
notebookSetTabVBorder nb width = {#call notebook_set_tab_vborder#}
  (toNotebook nb) (fromIntegral width)

-- @method notebookSetPopup@ Enable or disable context menus with all tabs in
-- it.
--
notebookSetPopup :: NotebookClass nb => nb -> Bool -> IO ()
notebookSetPopup nb enable = (if enable 
  then {#call notebook_popup_enable#} else {#call notebook_popup_disable#})
  (toNotebook nb)

-- @method notebookGetCurrentPage@ Query the currently selected page.
--
-- * Returns -1 if notebook has no pages.
--
notebookGetCurrentPage :: NotebookClass nb => nb -> IO Int
notebookGetCurrentPage nb = liftM fromIntegral $
  {#call unsafe notebook_get_current_page#} (toNotebook nb)

-- @method notebookGetMenuLabel@ Extract the menu label from the given
-- @ref arg child@.
--
-- * Returns Nothing if @ref arg child@ was not found.
--
notebookGetMenuLabel :: (NotebookClass nb, WidgetClass w) => nb -> w ->
                        IO (Maybe Label)
notebookGetMenuLabel nb child = do
  wPtr <- {#call unsafe notebook_get_menu_label#} 
    (toNotebook nb) (toWidget child)
  if wPtr==nullPtr then return Nothing else liftM Just $
    makeNewObject mkLabel $ return $ castPtr wPtr

-- @method notebookGetNthPage@ Retrieve the child widget at positon
-- @ref arg pos@ (stating from 0).
--
-- * Returns Nothing if the index is out of bounds.
--
notebookGetNthPage :: NotebookClass nb => nb -> Int -> IO (Maybe Widget)
notebookGetNthPage nb pos = do
  wPtr <- {#call unsafe notebook_get_nth_page#} 
    (toNotebook nb) (fromIntegral pos)
  if wPtr==nullPtr then return Nothing else liftM Just $
    makeNewObject mkWidget $ return wPtr

-- @method notebookGetNPages@ Get the number of pages in a notebook.
--
notebookGetNPages :: NotebookClass nb => nb -> IO Int
notebookGetNPages nb = liftM fromIntegral $
  {#call unsafe notebook_get_n_pages#} (toNotebook nb)

-- @method notebookGetTabLabel@ Extract the tab label from the given
-- @ref arg child@.
--
notebookGetTabLabel :: (NotebookClass nb, WidgetClass w) => nb -> w ->
                       IO (Maybe Label)
notebookGetTabLabel nb child = do
  wPtr <- {#call unsafe notebook_get_tab_label#} 
    (toNotebook nb) (toWidget child)
  if wPtr==nullPtr then return Nothing else liftM Just $
    makeNewObject mkLabel $ return $ castPtr wPtr

-- @method notebookQueryTabLabelPacking@ Query the packing attributes of the
-- given @ref arg child@.
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

-- @method notebookSetTabLabelPacking@ Set the packing attributes of the given
-- @ref arg child@.
--
notebookSetTabLabelPacking :: (NotebookClass nb, WidgetClass w) => nb -> w ->
                              Packing -> PackType -> IO ()
notebookSetTabLabelPacking nb child pack pt = 
  {#call notebook_set_tab_label_packing#} (toNotebook nb) (toWidget child)
    (fromBool $ pack/=PackNatural) (fromBool $ pack==PackGrow) 
    ((fromIntegral.fromEnum) pt)

-- @method notebookSetHomogeneousTabs@ Sets whether the tabs must have all the
-- same size or not.
--
notebookSetHomogeneousTabs :: NotebookClass nb => nb -> Bool -> IO ()
notebookSetHomogeneousTabs nb hom = {#call notebook_set_homogeneous_tabs#}
  (toNotebook nb) (fromBool hom)


-- @method notebookSetTabLabel@ Set a new tab label for a given
-- @ref arg child@.
--
notebookSetTabLabel :: (NotebookClass nb, WidgetClass ch, WidgetClass tab) => 
                       nb -> ch -> tab -> IO ()
notebookSetTabLabel nb child tab = {#call notebook_set_tab_label#}
  (toNotebook nb) (toWidget child) (toWidget tab)

-- signals

-- @signal connectToSwitchPage@ This signal is emitted when a new page is
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


 
