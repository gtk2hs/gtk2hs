{-# OPTIONS -cpp #-}
-- -*-haskell-*-
--  The Monad GUI Library (Mogul): Retrieving a widget from the global store.
--
--  Author : Axel Simon
--
--  Created: 4 June 2001
--
--  Version $Revision: 1.3 $ from $Date: 2005/05/20 23:54:00 $
--
--  Copyright (C) 2001 Axel Simon
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
--
-- Retrieve a widget by name from the global store.
--
#include <gtk2hs-config.h>

module Graphics.UI.Gtk.Mogul.GetWidget (
  getMisc,
  getLabel,
  getAccelLabel,
#ifndef DISABLE_DEPRECATED
  getTipsQuery,
#endif
  getArrow,
  getImage,
  getContainer,
  getBin,
  getAlignment,
  getFrame,
  getAspectFrame,
  getButton,
  getToggleButton,
  getCheckButton,
  getRadioButton,
#ifndef DISABLE_DEPRECATED
  getOptionMenu,
#endif
  getItem,
  getMenuItem,
  getCheckMenuItem,
  getRadioMenuItem,
  getTearoffMenuItem,
#ifndef DISABLE_DEPRECATED
  getListItem,
#endif
  getWindow,
  getDialog,
  getColorSelectionDialog,
  getFileSelection,
  getFontSelectionDialog,
  getInputDialog,
  getMessageDialog,
  --getPlug,
  getEventBox,
  getHandleBox,
  getScrolledWindow,
  getViewport,
  getBox,
  getButtonBox,
  getHButtonBox,
  getVButtonBox,
  getVBox,
  getColorSelection,
  getFontSelection,
  getGammaCurve,
  getHBox,
#ifndef DISABLE_DEPRECATED
  getCombo,
#endif
  getStatusbar,
#ifndef DISABLE_DEPRECATED
  getCList,
  getCTree,
#endif
  getFixed,
  getPaned,
  getHPaned,
  getVPaned,
  getLayout,
#ifndef DISABLE_DEPRECATED
  getList,
#endif
  getMenuShell,
  getMenu,
  getMenuBar,
  getNotebook,
  --getSocket,
  getTable,
  getTextView,
  getToolbar,
  getTreeView,
  getCalendar,
  getDrawingArea,
  getCurve,
  getEntry,
  getSpinButton,
  getRuler,
  getHRuler,
  getVRuler,
  getRange,
  getScale,
  getHScale,
  getVScale,
  getScrollbar,
  getHScrollbar,
  getVScrollbar,
  getSeparator,
  getHSeparator,
  getVSeparator,
  getInvisible,
#ifndef DISABLE_DEPRECATED
  getPreview,
#endif
  getProgressBar
  ) where

import Graphics.UI.Gtk.Mogul.WidgetTable	(widgetLookup)
import Graphics.UI.Gtk.Types

-- The get<Name> functions lookup a widget by name in the global store. Only
-- widgets created by new<Name> can be retrieved this way. The function throws
-- an exception if the widget was not found or is the wrong type. (EXPORTED)
--

getMisc :: String -> IO Misc
getMisc name = 
  widgetLookup name "Misc" mkMisc

getLabel :: String -> IO Label
getLabel name = 
  widgetLookup name "Label" mkLabel

getAccelLabel :: String -> IO AccelLabel
getAccelLabel name = 
  widgetLookup name "AccelLabel" mkAccelLabel

#ifndef DISABLE_DEPRECATED
getTipsQuery :: String -> IO TipsQuery
getTipsQuery name = 
  widgetLookup name "TipsQuery" mkTipsQuery
#endif

getArrow :: String -> IO Arrow
getArrow name = 
  widgetLookup name "Arrow" mkArrow

getImage :: String -> IO Image
getImage name = 
  widgetLookup name "Image" mkImage

getContainer :: String -> IO Container
getContainer name = 
  widgetLookup name "Container" mkContainer

getBin :: String -> IO Bin
getBin name = 
  widgetLookup name "Bin" mkBin

getAlignment :: String -> IO Alignment
getAlignment name = 
  widgetLookup name "Alignment" mkAlignment

getFrame :: String -> IO Frame
getFrame name = 
  widgetLookup name "Frame" mkFrame

getAspectFrame :: String -> IO AspectFrame
getAspectFrame name = 
  widgetLookup name "AspectFrame" mkAspectFrame

getButton :: String -> IO Button
getButton name = 
  widgetLookup name "Button" mkButton

getToggleButton :: String -> IO ToggleButton
getToggleButton name = 
  widgetLookup name "ToggleButton" mkToggleButton

getCheckButton :: String -> IO CheckButton
getCheckButton name = 
  widgetLookup name "CheckButton" mkCheckButton

getRadioButton :: String -> IO RadioButton
getRadioButton name = 
  widgetLookup name "RadioButton" mkRadioButton

#ifndef DISABLE_DEPRECATED
getOptionMenu :: String -> IO OptionMenu
getOptionMenu name = 
  widgetLookup name "OptionMenu" mkOptionMenu
#endif

getItem :: String -> IO Item
getItem name = 
  widgetLookup name "Item" mkItem

getMenuItem :: String -> IO MenuItem
getMenuItem name = 
  widgetLookup name "MenuItem" mkMenuItem

getCheckMenuItem :: String -> IO CheckMenuItem
getCheckMenuItem name = 
  widgetLookup name "CheckMenuItem" mkCheckMenuItem

getRadioMenuItem :: String -> IO RadioMenuItem
getRadioMenuItem name = 
  widgetLookup name "RadioMenuItem" mkRadioMenuItem

getTearoffMenuItem :: String -> IO TearoffMenuItem
getTearoffMenuItem name = 
  widgetLookup name "TearoffMenuItem" mkTearoffMenuItem

#ifndef DISABLE_DEPRECATED
getListItem :: String -> IO ListItem
getListItem name = 
  widgetLookup name "ListItem" mkListItem
#endif

getWindow :: String -> IO Window
getWindow name = 
  widgetLookup name "Window" mkWindow

getDialog :: String -> IO Dialog
getDialog name = 
  widgetLookup name "Dialog" mkDialog

getColorSelectionDialog :: String -> IO ColorSelectionDialog
getColorSelectionDialog name = 
  widgetLookup name "ColorSelectionDialog" mkColorSelectionDialog

getFileSelection :: String -> IO FileSelection
getFileSelection name = 
  widgetLookup name "FileSelection" mkFileSelection

getFontSelectionDialog :: String -> IO FontSelectionDialog
getFontSelectionDialog name = 
  widgetLookup name "FontSelectionDialog" mkFontSelectionDialog

getInputDialog :: String -> IO InputDialog
getInputDialog name = 
  widgetLookup name "InputDialog" mkInputDialog

getMessageDialog :: String -> IO MessageDialog
getMessageDialog name = 
  widgetLookup name "MessageDialog" mkMessageDialog

--getPlug :: String -> IO Plug
--getPlug name = 
--  widgetLookup name "Plug" mkPlug

getEventBox :: String -> IO EventBox
getEventBox name = 
  widgetLookup name "EventBox" mkEventBox

getHandleBox :: String -> IO HandleBox
getHandleBox name = 
  widgetLookup name "HandleBox" mkHandleBox

getScrolledWindow :: String -> IO ScrolledWindow
getScrolledWindow name = 
  widgetLookup name "ScrolledWindow" mkScrolledWindow

getViewport :: String -> IO Viewport
getViewport name = 
  widgetLookup name "Viewport" mkViewport

getBox :: String -> IO Box
getBox name = 
  widgetLookup name "Box" mkBox

getButtonBox :: String -> IO ButtonBox
getButtonBox name = 
  widgetLookup name "ButtonBox" mkButtonBox

getHButtonBox :: String -> IO HButtonBox
getHButtonBox name = 
  widgetLookup name "HButtonBox" mkHButtonBox

getVButtonBox :: String -> IO VButtonBox
getVButtonBox name = 
  widgetLookup name "VButtonBox" mkVButtonBox

getVBox :: String -> IO VBox
getVBox name = 
  widgetLookup name "VBox" mkVBox

getColorSelection :: String -> IO ColorSelection
getColorSelection name = 
  widgetLookup name "ColorSelection" mkColorSelection

getFontSelection :: String -> IO FontSelection
getFontSelection name = 
  widgetLookup name "FontSelection" mkFontSelection

getGammaCurve :: String -> IO GammaCurve
getGammaCurve name = 
  widgetLookup name "GammaCurve" mkGammaCurve

getHBox :: String -> IO HBox
getHBox name = 
  widgetLookup name "HBox" mkHBox

#ifndef DISABLE_DEPRECATED
getCombo :: String -> IO Combo
getCombo name = 
  widgetLookup name "Combo" mkCombo
#endif

getStatusbar :: String -> IO Statusbar
getStatusbar name = 
  widgetLookup name "Statusbar" mkStatusbar

#ifndef DISABLE_DEPRECATED
getCList :: String -> IO CList
getCList name = 
  widgetLookup name "CList" mkCList

getCTree :: String -> IO CTree
getCTree name = 
  widgetLookup name "CTree" mkCTree
#endif

getFixed :: String -> IO Fixed
getFixed name = 
  widgetLookup name "Fixed" mkFixed

getPaned :: String -> IO Paned
getPaned name = 
  widgetLookup name "Paned" mkPaned

getHPaned :: String -> IO HPaned
getHPaned name = 
  widgetLookup name "HPaned" mkHPaned

getVPaned :: String -> IO VPaned
getVPaned name = 
  widgetLookup name "VPaned" mkVPaned

getLayout :: String -> IO Layout
getLayout name = 
  widgetLookup name "Layout" mkLayout

#ifndef DISABLE_DEPRECATED
getList :: String -> IO List
getList name = 
  widgetLookup name "List" mkList
#endif

getMenuShell :: String -> IO MenuShell
getMenuShell name = 
  widgetLookup name "MenuShell" mkMenuShell

getMenu :: String -> IO Menu
getMenu name = 
  widgetLookup name "Menu" mkMenu

getMenuBar :: String -> IO MenuBar
getMenuBar name = 
  widgetLookup name "MenuBar" mkMenuBar

getNotebook :: String -> IO Notebook
getNotebook name = 
  widgetLookup name "Notebook" mkNotebook

--getSocket :: String -> IO Socket
--getSocket name = 
--  widgetLookup name "Socket" mkSocket

getTable :: String -> IO Table
getTable name = 
  widgetLookup name "Table" mkTable

getTextView :: String -> IO TextView
getTextView name = 
  widgetLookup name "TextView" mkTextView

getToolbar :: String -> IO Toolbar
getToolbar name = 
  widgetLookup name "Toolbar" mkToolbar

getTreeView :: String -> IO TreeView
getTreeView name = 
  widgetLookup name "TreeView" mkTreeView

getCalendar :: String -> IO Calendar
getCalendar name = 
  widgetLookup name "Calendar" mkCalendar

getDrawingArea :: String -> IO DrawingArea
getDrawingArea name = 
  widgetLookup name "DrawingArea" mkDrawingArea

getCurve :: String -> IO Curve
getCurve name = 
  widgetLookup name "Curve" mkCurve

getEntry :: String -> IO Entry
getEntry name = 
  widgetLookup name "Entry" mkEntry

getSpinButton :: String -> IO SpinButton
getSpinButton name = 
  widgetLookup name "SpinButton" mkSpinButton

getRuler :: String -> IO Ruler
getRuler name = 
  widgetLookup name "Ruler" mkRuler

getHRuler :: String -> IO HRuler
getHRuler name = 
  widgetLookup name "HRuler" mkHRuler

getVRuler :: String -> IO VRuler
getVRuler name = 
  widgetLookup name "VRuler" mkVRuler

getRange :: String -> IO Range
getRange name = 
  widgetLookup name "Range" mkRange

getScale :: String -> IO Scale
getScale name = 
  widgetLookup name "Scale" mkScale

getHScale :: String -> IO HScale
getHScale name = 
  widgetLookup name "HScale" mkHScale

getVScale :: String -> IO VScale
getVScale name = 
  widgetLookup name "VScale" mkVScale

getScrollbar :: String -> IO Scrollbar
getScrollbar name = 
  widgetLookup name "Scrollbar" mkScrollbar

getHScrollbar :: String -> IO HScrollbar
getHScrollbar name = 
  widgetLookup name "HScrollbar" mkHScrollbar

getVScrollbar :: String -> IO VScrollbar
getVScrollbar name = 
  widgetLookup name "VScrollbar" mkVScrollbar

getSeparator :: String -> IO Separator
getSeparator name = 
  widgetLookup name "Separator" mkSeparator

getHSeparator :: String -> IO HSeparator
getHSeparator name = 
  widgetLookup name "HSeparator" mkHSeparator

getVSeparator :: String -> IO VSeparator
getVSeparator name = 
  widgetLookup name "VSeparator" mkVSeparator

getInvisible :: String -> IO Invisible
getInvisible name = 
  widgetLookup name "Invisible" mkInvisible

#ifndef DISABLE_DEPRECATED
getPreview :: String -> IO Preview
getPreview name = 
  widgetLookup name "Preview" mkPreview
#endif

getProgressBar :: String -> IO ProgressBar
getProgressBar name = 
  widgetLookup name "ProgressBar" mkProgressBar
