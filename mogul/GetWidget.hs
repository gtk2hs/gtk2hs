-- -*-haskell-*-
--  The Monad GUI Library (Mogul): Retrieving a widget from the global store.
--
--  Author : Axel Simon
--          
--  Created: 4 June 2001
--
--  Version $Revision: 1.4 $ from $Date: 2004/05/25 00:33:35 $
--
--  Copyright (c) 2001 Axel Simon
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
-- |
--
-- Retrieve a widget by name from the global store.
--

module GetWidget(
  getMisc,
  getLabel,
  getAccelLabel,
  getTipsQuery,
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
  getOptionMenu,
  getItem,
  getMenuItem,
  getCheckMenuItem,
  getRadioMenuItem,
  getTearoffMenuItem,
  getListItem,
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
  getCombo,
  getStatusbar,
  getCList,
  getCTree,
  getFixed,
  getPaned,
  getHPaned,
  getVPaned,
  getLayout,
  getList,
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
  getPreview,
  getProgressBar
  ) where

import WidgetTable	(widgetLookup)
import Hierarchy

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

getTipsQuery :: String -> IO TipsQuery
getTipsQuery name = 
  widgetLookup name "TipsQuery" mkTipsQuery

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

getOptionMenu :: String -> IO OptionMenu
getOptionMenu name = 
  widgetLookup name "OptionMenu" mkOptionMenu

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

getListItem :: String -> IO ListItem
getListItem name = 
  widgetLookup name "ListItem" mkListItem

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

getCombo :: String -> IO Combo
getCombo name = 
  widgetLookup name "Combo" mkCombo

getStatusbar :: String -> IO Statusbar
getStatusbar name = 
  widgetLookup name "Statusbar" mkStatusbar

getCList :: String -> IO CList
getCList name = 
  widgetLookup name "CList" mkCList

getCTree :: String -> IO CTree
getCTree name = 
  widgetLookup name "CTree" mkCTree

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

getList :: String -> IO List
getList name = 
  widgetLookup name "List" mkList

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

getPreview :: String -> IO Preview
getPreview name = 
  widgetLookup name "Preview" mkPreview

getProgressBar :: String -> IO ProgressBar
getProgressBar name = 
  widgetLookup name "ProgressBar" mkProgressBar
