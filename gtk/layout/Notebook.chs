-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: Widget Notebook
--
--  Author : Axel Simon
--          
--  Created: 15 May 2001
--
--  Version $Revision: 1.1.1.1 $ from $Date: 2002/03/24 21:56:20 $
--
--  Copyright (c) [1999.2001] Manuel Chakravarty, Axel Simon
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
--- DESCRIPTION ---------------------------------------------------------------
--
-- * This widget can display several pages of widgets. Each page can be 
--   selected by a tab at the top of the widget. It is useful in dialogs where
--   a lot of information has to be displayed.
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------
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
  notebookGetTabLabel,
  Packing(..), PackType(..),
  notebookQueryTabLabelPacking,
  notebookSetTabLabelPacking,
  notebookSetHomogeneousTabs,
  notebookSetTabLabel,
  connectToSwitchPage
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

-- Create a new notebook. (EXPORTED)
--
notebookNew :: IO Notebook
notebookNew = makeNewObject mkNotebook $ 
  liftM castPtr {#call unsafe notebook_new#}

-- Insert a new tab to the right of the existing tabs. (EXPORTED)
--
-- * The @tabName will be inserted as a Label widget. In case the context
--   menu is enabled, this name will appear in the menu. If you want to
--   specify something else to go in the tab, use @notebookAppendPageMenu
--   and specify some suitable widget for @menuLabel.
--
notebookAppendPage :: (NotebookClass nb, WidgetClass child) =>
  child -> String -> nb -> IO ()
notebookAppendPage child tabLabel nb = do
  tab <- labelNew (Just tabLabel)
  {#call notebook_append_page#} (toNotebook nb) (toWidget child) 
    (toWidget tab)

-- Insert a new tab to the right of the existing tabs. @menuLabel is the
-- label for the context menu (useful if @tabLabel is not a Label widget).
-- (EXPORTED)
--
notebookAppendPageMenu :: (NotebookClass nb, WidgetClass child, 
  WidgetClass tab) => child -> tab -> String -> nb -> IO ()
notebookAppendPageMenu child tabWidget menuLabel nb = do
  menu <- labelNew (Just menuLabel)
  {#call notebook_append_page_menu#} (toNotebook nb) (toWidget child)
    (toWidget tabWidget) (toWidget menu)

-- Insert a new tab to the left of the existing tabs. (EXPORTED)
--
-- * The @tabName will be inserted as a Label widget. In case the context
--   menu is enabled, this name will appear in the menu. If you want to
--   specify something else to go in the tab, use @notebookPrependPageMenu
--   and specify some suitable widget for @menuLabel.
--
notebookPrependPage :: (NotebookClass nb, WidgetClass child) =>
  child -> String -> nb -> IO ()
notebookPrependPage child tabLabel nb = do
  tab <- labelNew (Just tabLabel)
  {#call notebook_prepend_page#} (toNotebook nb) (toWidget child) (toWidget tab)

-- Insert a new tab to the left of the existing tabs. @menuLabel is the
-- label for the context menu (useful if @tabLabel is not a Label widget).
-- (EXPORTED)
--
notebookPrependPageMenu :: (NotebookClass nb, WidgetClass child, 
  WidgetClass tab) => child -> tab -> String -> nb -> IO ()
notebookPrependPageMenu child tabWidget menuLabel nb = do
  menu <- labelNew (Just menuLabel)
  {#call notebook_prepend_page_menu#} (toNotebook nb) (toWidget child)
    (toWidget tabWidget) (toWidget menu)


-- Insert a new tab at the specified position. (EXPORTED)
--
-- * The @tabName will be inserted as a Label widget. In case the context
--   menu is enabled, this name will appear in the menu. If you want to
--   specify something else to go in the tab, use @notebookInsertPageMenu
--   and specify some suitable widget for @menuLabel.
--
notebookInsertPage :: (NotebookClass nb, WidgetClass child) =>
  child -> String -> Int -> nb -> IO ()
notebookInsertPage child tabLabel pos nb = do
  lbl <- labelNew (Just tabLabel)
  {#call notebook_insert_page#} (toNotebook nb) (toWidget child) 
     (toWidget lbl) (fromIntegral pos)

-- Insert a new tab between the tab no. @pos and @pos+1. @menuLabel is the
-- label for the context menu (useful if @tabLabel is not a Label widget).
-- (EXPORTED)
--
notebookInsertPageMenu :: (NotebookClass nb, WidgetClass child, 
  WidgetClass tab) => child -> tab -> String -> Int -> nb -> IO ()
notebookInsertPageMenu child tabWidget menuLabel pos nb = do
  menu <- labelNew (Just menuLabel)
  {#call notebook_insert_page_menu#} (toNotebook nb) (toWidget child)
    (toWidget tabWidget) (toWidget menu) (fromIntegral pos)

-- Remove a specific page from the notebook, counting from 0. (EXPORTED)
--
notebookRemovePage :: NotebookClass nb => Int -> nb -> IO ()
notebookRemovePage pos nb = 
  {#call notebook_remove_page#} (toNotebook nb) (fromIntegral pos)

-- Query the page the @child widget is contained in. (EXPORTED)
--
-- * The function returns the page number if the child was found, Nothing 
--   otherwise.
--
notebookPageNum :: (NotebookClass nb, WidgetClass w) => 
  w -> nb -> IO (Maybe Int)
notebookPageNum child nb = 
  liftM (\page -> if page==(-1) then Nothing else Just (fromIntegral page)) $
  {#call unsafe notebook_page_num#} (toNotebook nb) (toWidget child)

-- Move to the specified page of the notebook. (EXPORTED)
--
-- * If @pos is out of range (e.g. negative) select the last page.
--
notebookSetCurrentPage :: NotebookClass nb => Int -> nb -> IO ()
notebookSetCurrentPage pos nb =
  {#call notebook_set_current_page#} (toNotebook nb) (fromIntegral pos)

-- Move to the right neighbour of the current page. (EXPORTED)
--
-- * Nothing happens if there is no such page.
--
notebookNextPage :: NotebookClass nb => nb -> IO ()
notebookNextPage nb = {#call notebook_next_page#} (toNotebook nb)

-- Move to the left neighbour of the current page. (EXPORTED)
--
-- * Nothing happens if there is no such page.
--
notebookPrevPage :: NotebookClass nb => nb -> IO ()
notebookPrevPage nb = {#call notebook_prev_page#} (toNotebook nb)

-- Move a page withing the notebook. (EXPORTED)
--
notebookReorderChild :: (NotebookClass nb, WidgetClass w) => 
  w -> Int -> nb -> IO ()
notebookReorderChild child pos nb = {#call notebook_reorder_child#}
  (toNotebook nb) (toWidget child) (fromIntegral pos)

-- Specify at which border the tabs should be drawn. (EXPORTED)
--
notebookSetTabPos :: NotebookClass nb => PositionType -> nb -> IO ()
notebookSetTabPos pt nb = {#call notebook_set_tab_pos#}
  (toNotebook nb) ((fromIntegral.fromEnum) pt)

-- Show or hide the tabs of a notebook. (EXPORTED)
--
notebookSetShowTabs :: NotebookClass nb => Bool -> nb -> IO ()
notebookSetShowTabs showTabs nb = {#call notebook_set_show_tabs#}
  (toNotebook nb) (fromBool showTabs)

-- In case the tabs are not shown, specify whether to draw a border around
-- the notebook. (EXPORTED)
--
notebookSetShowBorder :: NotebookClass nb => Bool -> nb -> IO ()
notebookSetShowBorder showBorder nb = {#call notebook_set_show_border#}
  (toNotebook nb) (fromBool showBorder)

-- Set whether scroll bars will be added in case the notebook has too many
-- tabs to fit the widget size. (EXPORTED)
--
notebookSetScrollable :: NotebookClass nb => Bool -> nb -> IO ()
notebookSetScrollable scrollable nb = {#call unsafe notebook_set_scrollable#}
  (toNotebook nb) (fromBool scrollable)

-- Set the width of the borders of the tab labels. (EXPORTED)
--
-- * Sets both vertical and horizontal widths.
--
notebookSetTabBorder :: NotebookClass nb => Int -> nb -> IO ()
notebookSetTabBorder width nb = {#call notebook_set_tab_border#}
  (toNotebook nb) (fromIntegral width)

-- Set the width of the borders of the tab labels. (EXPORTED)
--
-- * Sets horizontal widths.
--
notebookSetTabHBorder :: NotebookClass nb => Int -> nb -> IO ()
notebookSetTabHBorder width nb = {#call notebook_set_tab_hborder#}
  (toNotebook nb) (fromIntegral width)

-- Set the width of the borders of the tab labels. (EXPORTED)
--
-- * Sets vertical widths.
--
notebookSetTabVBorder :: NotebookClass nb => Int -> nb -> IO ()
notebookSetTabVBorder width nb = {#call notebook_set_tab_vborder#}
  (toNotebook nb) (fromIntegral width)

-- Enable or disable context menus with all tabs in it. (EXPORTED)
--
notebookSetPopup :: NotebookClass nb => Bool -> nb -> IO ()
notebookSetPopup enable nb = (if enable 
  then {#call notebook_popup_enable#} else {#call notebook_popup_disable#})
  (toNotebook nb)

-- Query the currently selected page. (EXPORTED)
--
-- * Returns -1 if notebook has no pages.
--
notebookGetCurrentPage :: NotebookClass nb => nb -> IO Int
notebookGetCurrentPage nb = liftM fromIntegral $
  {#call unsafe notebook_get_current_page#} (toNotebook nb)

-- Extract the menu label from the given @child. (EXPORTED)
--
-- * Returns Nothing if @child was not found.
--
notebookGetMenuLabel :: (NotebookClass nb, WidgetClass w) => 
  w -> nb -> IO (Maybe Label)
notebookGetMenuLabel child nb = do
  wPtr <- {#call unsafe notebook_get_menu_label#} 
    (toNotebook nb) (toWidget child)
  if wPtr==nullPtr then return Nothing else liftM Just $
    makeNewObject mkLabel $ return $ castPtr wPtr

-- Retrieve the child widget at positon @pos (stating from 0). (EXPORTED)
--
-- * Returns Nothing if the index is out of bounds.
--
notebookGetNthPage :: NotebookClass nb => Int -> nb -> IO (Maybe Widget)
notebookGetNthPage pos nb = do
  wPtr <- {#call unsafe notebook_get_nth_page#} 
    (toNotebook nb) (fromIntegral pos)
  if wPtr==nullPtr then return Nothing else liftM Just $
    makeNewObject mkWidget $ return wPtr

-- Extract the tab label from the given @child. (EXPORTED)
--
notebookGetTabLabel :: (NotebookClass nb, WidgetClass w) => 
  w -> nb -> IO (Maybe Label)
notebookGetTabLabel child nb = do
  wPtr <- {#call unsafe notebook_get_tab_label#} 
    (toNotebook nb) (toWidget child)
  if wPtr==nullPtr then return Nothing else liftM Just $
    makeNewObject mkLabel $ return $ castPtr wPtr

-- Query the packing attributes of the given @child. (EXPORTED)
--
notebookQueryTabLabelPacking :: (NotebookClass nb, WidgetClass w) =>
  w -> nb -> IO (Packing,PackType)
notebookQueryTabLabelPacking child nb = 
  alloca $ \expPtr -> alloca $ \fillPtr -> alloca $ \packPtr -> do
    {#call unsafe notebook_query_tab_label_packing#} (toNotebook nb)
      (toWidget child) expPtr fillPtr packPtr
    expand <- liftM toBool $ peek expPtr
    fill <- liftM toBool $ peek fillPtr
    pt <- liftM (toEnum.fromIntegral) $ peek packPtr
    return (if fill then PackFill else 
             (if expand then PackExpand else PackNatural),
	    pt)

-- Set the packing attributes of the given @child. (EXPORTED)
--
notebookSetTabLabelPacking :: (NotebookClass nb, WidgetClass w) =>
  w -> Packing -> PackType -> nb -> IO ()
notebookSetTabLabelPacking child pack pt nb = 
  {#call notebook_set_tab_label_packing#} (toNotebook nb) (toWidget child)
    (fromBool $ pack/=PackNatural) (fromBool $ pack==PackFill) 
    ((fromIntegral.fromEnum) pt)

-- Sets whether the tabs must have all the same size or not. (EXPORTED)
--
notebookSetHomogeneousTabs :: NotebookClass nb => Bool -> nb -> IO ()
notebookSetHomogeneousTabs hom nb = {#call notebook_set_homogeneous_tabs#}
  (toNotebook nb) (fromBool hom)


-- Set a new tab label for a given @child. (EXPORTED)
--
notebookSetTabLabel :: (NotebookClass nb, WidgetClass ch, WidgetClass tab) =>
  ch -> tab -> nb -> IO ()
notebookSetTabLabel child tab nb = {#call notebook_set_tab_label#}
  (toNotebook nb) (toWidget child) (toWidget tab)

-- signals

-- This signal is emitted when a new page is selected.
--
connectToSwitchPage :: NotebookClass nb =>
  (Int -> IO ()) -> ConnectAfter -> nb -> IO (ConnectId nb)
connectToSwitchPage fun = connect_BOXED_WORD__NONE "switch-page" 
  (const $ return ()) (\_ page -> fun (fromIntegral page))