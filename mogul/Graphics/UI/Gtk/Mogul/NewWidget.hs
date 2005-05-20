{-# OPTIONS -cpp #-}
--  The Monad GUI Library (Mogul): Creation of new widgets.
--
--  Author : Axel Simon
--
--  Created: 2 June 2001
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
-- These functions generate a new widget with a name.
--
-- * The widget can later be lookup up by its name from the global store. As
--   soon as the widget is destroyed it is deleted from the store. If a given
--   name is still available can be tested by the 'isValidName' function.
--
#include <gtk2hs-config.h>

module Graphics.UI.Gtk.Mogul.NewWidget (
  newTextBuffer,
--  newTextTag,
--  newTextTagTable,
  newLabel,
  newNamedLabel,
  newAccelLabel,
  newNamedAccelLabel,
  newArrow,
  newNamedArrow,
  newImageFromFile,
  newNamedImageFromFile,
  newAlignment,
  newNamedAlignment,
  newFrame,
  newNamedFrame,
  newAspectFrame,
  newNamedAspectFrame,
  newButton,
  newNamedButton,
  newButtonWithLabel,
  newNamedButtonWithLabel,
  newButtonWithMnemonic,
  newNamedButtonWithMnemonic,
  newButtonFromStock,
  newNamedButtonFromStock,
  newToggleButton,
  newNamedToggleButton,
  newToggleButtonWithLabel,
  newNamedToggleButtonWithLabel,
  newCheckButton,
  newNamedCheckButton,
  newCheckButtonWithLabel,
  newNamedCheckButtonWithLabel,
  newCheckButtonWithMnemonic,
  newNamedCheckButtonWithMnemonic,
  newRadioButton,
  newNamedRadioButton,
  newRadioButtonWithLabel,
  newNamedRadioButtonWithLabel,
  newRadioButtonJoinGroup,
  newNamedRadioButtonJoinGroup,
  newRadioButtonJoinGroupWithLabel,
  newNamedRadioButtonJoinGroupWithLabel,
#ifndef DISABLE_DEPRECATED
  newOptionMenu,
  newNamedOptionMenu,
#endif
  newMenuItem,
  newNamedMenuItem,
  newMenuItemWithLabel,
  newNamedMenuItemWithLabel,
  newCheckMenuItem,
  newNamedCheckMenuItem,
  newCheckMenuItemWithLabel,
  newNamedCheckMenuItemWithLabel,
  newRadioMenuItem,
  newNamedRadioMenuItem,
  newRadioMenuItemWithLabel,
  newNamedRadioMenuItemWithLabel,
  newRadioMenuItemJoinGroup,
  newNamedRadioMenuItemJoinGroup,
  newRadioMenuItemJoinGroupWithLabel,
  newNamedRadioMenuItemJoinGroupWithLabel,
  newTearoffMenuItem,
  newNamedTearoffMenuItem,
  newWindow,
  newNamedWindow,
  newDialog,
  newNamedDialog,
  --newColorSelectionDialog,
  --newFileSelection,
  --newFontSelectionDialog,
  --newPlug,
  --newNamedPlug,
  newEventBox,
  newNamedEventBox,
  newHandleBox,
  newNamedHandleBox,
  newScrolledWindow,
  newNamedScrolledWindow,
  newViewport,
  newNamedViewport,
  newVBox,
  newNamedVBox,
  --newColorSelection,
  --newFontSelection,
  newHBox,
  newNamedHBox,
#ifndef DISABLE_DEPRECATED
  newCombo,
  newNamedCombo,
#endif
  newStatusbar,
  newNamedStatusbar,
  newHPaned,
  newNamedHPaned,
  newVPaned,
  newNamedVPaned,
  newLayout,
  newNamedLayout,
  newMenu,
  newNamedMenu,
  newMenuBar,
  newNamedMenuBar,
  newNotebook,
  newNamedNotebook,
  --newSocket,
  --newNamedSocket,
  newTable,
  newNamedTable,
  newTextView,
  newNamedTextView,
  newToolbar,
  newNamedToolbar,
  newCalendar,
  newNamedCalendar,
  --newDrawingArea,
  newEntry,
  newNamedEntry,
  newSpinButton,
  newNamedSpinButton,
  newSpinButtonWithRange,
  newNamedSpinButtonWithRange,
  newHScale,
  newNamedHScale,
  newVScale,
  newNamedVScale,
  newHScrollbar,
  newNamedHScrollbar,
  newVScrollbar,
  newNamedVScrollbar,
  newHSeparator,
  newNamedHSeparator,
  newVSeparator,
  newNamedVSeparator,
  newProgressBar,
  newNamedProgressBar,
  newAdjustment,
  --newIMContext,
  --newIMMulticontext,
  --newItemFactory,
  newTooltips,
  newTreeView,
  newNamedTreeView,
  newTreeViewWithModel,
  newNamedTreeViewWithModel,
  newTreeViewColumn,
  newIconFactory,
  ) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Mogul.WidgetTable	(WidgetName, newNamedWidget)

-- | see 'textBufferNew'
--
newTextBuffer :: Maybe TextTagTable -> IO TextBuffer
newTextBuffer = textBufferNew

-- | see 'textTagNew'
--
--newTextTag :: IO TextTag
--newTextTag = textTagNew

-- | see 'textTagTableNew'
--
--newTextTagTable :: IO TextTagTable
--newTextTagTable = textTagTableNew

-- | see 'labelNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedLabel :: WidgetName -> Maybe String -> IO Label
newNamedLabel name str = newNamedWidget name $ labelNew str

-- | see 'labelNew'
--
newLabel :: Maybe String -> IO Label
newLabel = labelNew

-- | see 'accelLabelNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedAccelLabel :: WidgetName -> String -> IO AccelLabel
newNamedAccelLabel name str = newNamedWidget name $ accelLabelNew str

-- | see 'accelLabelNew'
--
newAccelLabel :: String -> IO AccelLabel
newAccelLabel = accelLabelNew

-- | see 'arrowNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedArrow :: WidgetName -> ArrowType -> ShadowType -> IO Arrow
newNamedArrow name at st = newNamedWidget name $ arrowNew at st 

-- | see 'arrowNew'
--
newArrow :: ArrowType -> ShadowType -> IO Arrow
newArrow = arrowNew

-- | see 'imageNewFromFile'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedImageFromFile :: WidgetName -> FilePath -> IO Image
newNamedImageFromFile name fname = newNamedWidget name $ imageNewFromFile fname

-- | see 'imageNewFromFile'
--
newImageFromFile :: FilePath -> IO Image
newImageFromFile fname = imageNewFromFile fname

-- | see 'alignmentNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedAlignment :: 
  WidgetName -> Float -> Float -> Float -> Float -> IO Alignment
newNamedAlignment name xalign yalign xscale yscale = 
  newNamedWidget name $ alignmentNew xalign yalign xscale yscale 

-- | see 'alignmentNew'
--
newAlignment :: Float -> Float -> Float -> Float -> IO Alignment
newAlignment = alignmentNew

-- | see 'frameNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedFrame :: WidgetName -> IO Frame
newNamedFrame name = newNamedWidget name $ frameNew

-- | see 'frameNew'
--
newFrame :: IO Frame
newFrame = frameNew

-- | see 'aspectFrameNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedAspectFrame :: 
  WidgetName -> Float -> Float -> Maybe Float -> IO AspectFrame
newNamedAspectFrame name xalign yalign ratio = newNamedWidget name $
  aspectFrameNew xalign yalign ratio

-- | see 'aspectFrameNew'
--
newAspectFrame :: Float -> Float -> Maybe Float -> IO AspectFrame
newAspectFrame = aspectFrameNew
 
-- | see 'buttonNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedButton :: WidgetName -> IO Button
newNamedButton name = newNamedWidget name $ buttonNew

-- | see 'buttonNew'
--
newButton :: IO Button
newButton = buttonNew

-- | see 'buttonNewWithLabel'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedButtonWithLabel :: WidgetName -> String -> IO Button
newNamedButtonWithLabel name lbl = newNamedWidget name $ buttonNewWithLabel lbl

-- | see 'buttonNewWithLabel'
--
newButtonWithLabel :: String -> IO Button
newButtonWithLabel lbl = buttonNewWithLabel lbl

-- | see 'buttonNewWithMnemonic'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedButtonWithMnemonic :: WidgetName -> String -> IO Button
newNamedButtonWithMnemonic name mn = newNamedWidget name $ buttonNewWithMnemonic mn

-- | see 'buttonNewWithMnemonic'
--
newButtonWithMnemonic :: String -> IO Button
newButtonWithMnemonic mn = buttonNewWithMnemonic mn

-- | see 'buttonNewFromStock'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedButtonFromStock :: WidgetName -> String -> IO Button
newNamedButtonFromStock name stock = newNamedWidget name $ buttonNewFromStock stock

-- | see 'buttonNewFromStock'
--
newButtonFromStock :: String -> IO Button
newButtonFromStock stock = buttonNewFromStock stock

-- | see 'toggleButtonNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedToggleButton :: WidgetName -> IO ToggleButton
newNamedToggleButton name = newNamedWidget name $ toggleButtonNew

-- | see 'toggleButtonNew'
--
newToggleButton :: IO ToggleButton
newToggleButton = toggleButtonNew

-- | see 'toggleButtonNewWithLabel'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedToggleButtonWithLabel :: WidgetName -> String -> IO ToggleButton
newNamedToggleButtonWithLabel name lbl = newNamedWidget name $ 
  toggleButtonNewWithLabel lbl

-- | see 'toggleButtonNewWithLabel'
--
newToggleButtonWithLabel :: String -> IO ToggleButton
newToggleButtonWithLabel lbl = 
  toggleButtonNewWithLabel lbl

-- | see 'checkButtonNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedCheckButton :: WidgetName -> IO CheckButton
newNamedCheckButton name = newNamedWidget name $ checkButtonNew

-- | see 'checkButtonNew'
--
newCheckButton :: IO CheckButton
newCheckButton = checkButtonNew

-- | see 'checkButtonNewWithLabel'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedCheckButtonWithLabel :: WidgetName -> String -> IO CheckButton
newNamedCheckButtonWithLabel name lbl = newNamedWidget name $ 
  checkButtonNewWithLabel lbl

-- | see 'checkButtonNewWithLabel'
--
newCheckButtonWithLabel :: String -> IO CheckButton
newCheckButtonWithLabel lbl = checkButtonNewWithLabel lbl

-- | see 'checkButtonNewWithMnemonic'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedCheckButtonWithMnemonic :: WidgetName -> String -> IO CheckButton
newNamedCheckButtonWithMnemonic name mn = newNamedWidget name $ 
  checkButtonNewWithMnemonic mn

-- | see 'checkButtonNewWithMnemonic'
--
newCheckButtonWithMnemonic :: String -> IO CheckButton
newCheckButtonWithMnemonic mn = checkButtonNewWithMnemonic mn

-- | see 'radioButtonNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedRadioButton :: WidgetName -> IO RadioButton
newNamedRadioButton name = newNamedWidget name $ radioButtonNew

-- | see 'radioButtonNew'
--
newRadioButton :: IO RadioButton
newRadioButton = radioButtonNew

-- | see 'radioButtonNewWithLabel'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedRadioButtonWithLabel :: WidgetName -> String -> IO RadioButton
newNamedRadioButtonWithLabel name lbl = newNamedWidget name $ 
  radioButtonNewWithLabel lbl

-- | see 'radioButtonNewWithLabel'
--
newRadioButtonWithLabel :: String -> IO RadioButton
newRadioButtonWithLabel lbl = radioButtonNewWithLabel lbl

-- | see 'radioButtonNewJoinGroup'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedRadioButtonJoinGroup :: WidgetName -> RadioButton -> IO RadioButton
newNamedRadioButtonJoinGroup name grp = newNamedWidget name $ 
  radioButtonNewJoinGroup grp

-- | see 'radioButtonNewJoinGroup'
--
newRadioButtonJoinGroup :: RadioButton -> IO RadioButton
newRadioButtonJoinGroup grp = 
  radioButtonNewJoinGroup grp

-- | see 'radioButtonNewJoinGroupWithLabel'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedRadioButtonJoinGroupWithLabel :: 
  WidgetName -> RadioButton -> String -> IO RadioButton
newNamedRadioButtonJoinGroupWithLabel name grp lbl = newNamedWidget name $ 
  radioButtonNewJoinGroupWithLabel grp lbl

-- | see 'radioButtonNewJoinGroupWithLabel'
--
newRadioButtonJoinGroupWithLabel :: 
  RadioButton -> String -> IO RadioButton
newRadioButtonJoinGroupWithLabel grp lbl = 
  radioButtonNewJoinGroupWithLabel grp lbl

#ifndef DISABLE_DEPRECATED
-- | see 'optionMenuNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedOptionMenu :: WidgetName -> IO OptionMenu
newNamedOptionMenu name = newNamedWidget name $ optionMenuNew

-- | see 'optionMenuNew'
--
newOptionMenu :: IO OptionMenu
newOptionMenu = optionMenuNew
#endif

-- | see 'menuItemNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedMenuItem :: WidgetName -> IO MenuItem
newNamedMenuItem name = newNamedWidget name $ menuItemNew

-- | see 'menuItemNew'
--
newMenuItem :: IO MenuItem
newMenuItem = menuItemNew

-- | see 'menuItemNewWithLabel'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedMenuItemWithLabel :: WidgetName -> String -> IO MenuItem
newNamedMenuItemWithLabel name lbl = newNamedWidget name $ menuItemNewWithLabel lbl

-- | see 'menuItemNewWithLabel'
--
newMenuItemWithLabel :: String -> IO MenuItem
newMenuItemWithLabel lbl = menuItemNewWithLabel lbl

-- | see 'checkMenuItemNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedCheckMenuItem :: WidgetName -> IO CheckMenuItem
newNamedCheckMenuItem name = newNamedWidget name $ checkMenuItemNew

-- | see 'checkMenuItemNew'
--
newCheckMenuItem :: IO CheckMenuItem
newCheckMenuItem = checkMenuItemNew

-- | see 'checkMenuItemNewWithLabel'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedCheckMenuItemWithLabel :: WidgetName -> String -> IO CheckMenuItem
newNamedCheckMenuItemWithLabel name lbl = newNamedWidget name $ 
  checkMenuItemNewWithLabel lbl

-- | see 'checkMenuItemNewWithLabel'
--
newCheckMenuItemWithLabel :: String -> IO CheckMenuItem
newCheckMenuItemWithLabel lbl = 
  checkMenuItemNewWithLabel lbl

-- | see 'radioMenuItemNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedRadioMenuItem :: WidgetName -> IO RadioMenuItem
newNamedRadioMenuItem name = newNamedWidget name $ radioMenuItemNew

-- | see 'radioMenuItemNew'
--
newRadioMenuItem :: IO RadioMenuItem
newRadioMenuItem = radioMenuItemNew

-- | see 'radioMenuItemNewWithLabel'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedRadioMenuItemWithLabel :: WidgetName -> String -> IO RadioMenuItem
newNamedRadioMenuItemWithLabel name lbl = newNamedWidget name $ 
  radioMenuItemNewWithLabel lbl

-- | see 'radioMenuItemNewWithLabel'
--
newRadioMenuItemWithLabel :: String -> IO RadioMenuItem
newRadioMenuItemWithLabel lbl = radioMenuItemNewWithLabel lbl

-- | see 'radioMenuNewItemJoinGroup'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedRadioMenuItemJoinGroup :: WidgetName -> RadioMenuItem -> IO RadioMenuItem
newNamedRadioMenuItemJoinGroup name grp = newNamedWidget name $ 
  radioMenuItemNewJoinGroup grp

-- | see 'radioMenuNewItemJoinGroup'
--
newRadioMenuItemJoinGroup :: RadioMenuItem -> IO RadioMenuItem
newRadioMenuItemJoinGroup grp = radioMenuItemNewJoinGroup grp

-- | see 'radioMenuItemNewJoinGroupWithLabel'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedRadioMenuItemJoinGroupWithLabel :: 
  WidgetName -> RadioMenuItem -> String -> IO RadioMenuItem
newNamedRadioMenuItemJoinGroupWithLabel name lbl grp = newNamedWidget name $ 
  radioMenuItemNewJoinGroupWithLabel lbl grp

-- | see 'radioMenuItemNewJoinGroupWithLabel'
--
newRadioMenuItemJoinGroupWithLabel :: 
  RadioMenuItem -> String -> IO RadioMenuItem
newRadioMenuItemJoinGroupWithLabel lbl grp = 
  radioMenuItemNewJoinGroupWithLabel lbl grp

-- | see 'tearoffMenuItemNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedTearoffMenuItem :: WidgetName -> IO TearoffMenuItem
newNamedTearoffMenuItem name = newNamedWidget name $ tearoffMenuItemNew

-- | see 'tearoffMenuItemNew'
--
newTearoffMenuItem :: IO TearoffMenuItem
newTearoffMenuItem = tearoffMenuItemNew

-- | see 'windowNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedWindow :: WidgetName -> IO Window
newNamedWindow name = newNamedWidget name $ windowNew

-- | see 'windowNew'
--
newWindow :: IO Window
newWindow = windowNew

-- | see 'dialogNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedDialog :: WidgetName -> IO Dialog
newNamedDialog name = newNamedWidget name $ dialogNew

-- | see 'dialogNew'
--
newDialog :: IO Dialog
newDialog = dialogNew

-- | see 'colorSelectionDialogNew'
--
--newColorSelectionDialog :: WidgetName -> IO ColorSelectionDialog
--newColorSelectionDialog name = newNamedWidget name $ colorSelectionDialogNew

-- | see 'fileSelectionNew'
--
--newFileSelection :: WidgetName -> IO FileSelection
--newFileSelection name = newNamedWidget name $ fileSelectionNew

-- | see 'fontSelectionDialogNew'
--
--newFontSelectionDialog :: WidgetName -> IO FontSelectionDialog
--newFontSelectionDialog name = newNamedWidget name $ fontSelectionDialogNew

-- | see 'plugNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
--newNamedPlug :: WidgetName -> XID -> IO Plug
--newNamedPlug name wn = newNamedWidget name $ plugNew wn

-- | see 'plugNew'
--
--newPlug :: XID -> IO Plug
--newPlug = plugNew 

-- | see 'eventBoxNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedEventBox :: WidgetName -> IO EventBox
newNamedEventBox name = newNamedWidget name $ eventBoxNew

-- | see 'eventBoxNew'
--
newEventBox :: IO EventBox
newEventBox = eventBoxNew

-- | see 'handleBoxNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedHandleBox :: WidgetName -> IO HandleBox
newNamedHandleBox name = newNamedWidget name $ handleBoxNew

-- | see 'handleBoxNew'
--
newHandleBox :: IO HandleBox
newHandleBox = handleBoxNew

-- | see 'scrolledWindowNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedScrolledWindow :: 
  WidgetName -> Maybe Adjustment -> Maybe Adjustment -> IO ScrolledWindow
newNamedScrolledWindow name hAdj vAdj = newNamedWidget name $ 
  scrolledWindowNew hAdj vAdj 

-- | see 'scrolledWindowNew'
--
newScrolledWindow :: Maybe Adjustment -> Maybe Adjustment -> IO ScrolledWindow
newScrolledWindow hAdj vAdj = scrolledWindowNew hAdj vAdj 

-- | see 'viewportNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedViewport :: WidgetName -> Adjustment -> Adjustment -> IO Viewport
newNamedViewport name hAdj vAdj = newNamedWidget name $ viewportNew hAdj vAdj 

-- | see 'viewportNew'
--
newViewport :: Adjustment -> Adjustment -> IO Viewport
newViewport hAdj vAdj = viewportNew hAdj vAdj 

-- | see 'vBoxNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedVBox :: WidgetName -> Bool -> Int -> IO VBox
newNamedVBox name homogeneous spacing = newNamedWidget name $ 
  vBoxNew homogeneous spacing

-- | see 'vBoxNew'
--
newVBox :: Bool -> Int -> IO VBox
newVBox homogeneous spacing = vBoxNew homogeneous spacing

-- | see 'colorSelectionNew'
--
--newColorSelection :: WidgetName -> IO colorSelection
--newColorSelection name = newNamedWidget name $ colorSelectionNew

-- | see 'fontSelectionNew'
--
--newFontSelection :: WidgetName -> IO FontSelection
--newFontSelection name = newNamedWidget name $ fontSelectionNew

-- | see 'hBoxNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedHBox :: WidgetName -> Bool -> Int -> IO HBox
newNamedHBox name homogeneous spacing = newNamedWidget name $ 
  hBoxNew homogeneous spacing

-- | see 'hBoxNew'
--
newHBox :: Bool -> Int -> IO HBox
newHBox homogeneous spacing = hBoxNew homogeneous spacing

#ifndef DISABLE_DEPRECATED
-- | see 'comboNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedCombo :: WidgetName -> IO Combo
newNamedCombo name = newNamedWidget name $ comboNew

-- | see 'comboNew'
--
newCombo :: IO Combo
newCombo = comboNew
#endif

-- | see 'statusbarNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedStatusbar :: WidgetName -> IO Statusbar
newNamedStatusbar name = newNamedWidget name $ statusbarNew

-- | see 'statusbarNew'
--
newStatusbar :: IO Statusbar
newStatusbar = statusbarNew

-- | see 'hPanedNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedHPaned :: WidgetName -> IO HPaned
newNamedHPaned name = newNamedWidget name $ hPanedNew

-- | see 'hPanedNew'
--
newHPaned :: IO HPaned
newHPaned = hPanedNew

-- | see 'vPanedNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedVPaned :: WidgetName -> IO VPaned
newNamedVPaned name = newNamedWidget name $ vPanedNew

-- | see 'vPanedNew'
--
newVPaned :: IO VPaned
newVPaned = vPanedNew

-- | see 'layoutNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedLayout :: WidgetName -> Maybe Adjustment -> Maybe Adjustment -> IO Layout
newNamedLayout name hadjustment vadjustment = newNamedWidget name $ 
  layoutNew hadjustment vadjustment

-- | see 'layoutNew'
--
newLayout :: Maybe Adjustment -> Maybe Adjustment -> IO Layout
newLayout hadjustment vadjustment = layoutNew hadjustment vadjustment

-- | see 'menuNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedMenu :: WidgetName -> IO Menu
newNamedMenu name = newNamedWidget name $ menuNew

-- | see 'menuNew'
--
newMenu :: IO Menu
newMenu = menuNew

-- | see 'menuBarNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedMenuBar :: WidgetName -> IO MenuBar
newNamedMenuBar name = newNamedWidget name $ menuBarNew

-- | see 'menuBarNew'
--
newMenuBar :: IO MenuBar
newMenuBar = menuBarNew

-- | see 'notebookNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedNotebook :: WidgetName -> IO Notebook
newNamedNotebook name = newNamedWidget name $ notebookNew

-- | see 'notebookNew'
--
newNotebook :: IO Notebook
newNotebook = notebookNew

-- | see 'socketNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
--newNamedSocket :: WidgetName -> IO Socket
--newNamedSocket name = newNamedWidget name $ socketNew

-- | see 'socketNew'
--
--newSocket :: IO Socket
--newSocket = socketNew

-- | see 'tableNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedTable :: WidgetName -> Int -> Int -> Bool -> IO Table
newNamedTable name rows columns homogeneous = newNamedWidget name $ 
  tableNew rows columns homogeneous

-- | see 'tableNew'
--
newTable :: Int -> Int -> Bool -> IO Table
newTable rows columns homogeneous = 
  tableNew rows columns homogeneous

-- | see 'textViewNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedTextView :: WidgetName -> IO TextView
newNamedTextView name = newNamedWidget name $ textViewNew

-- | see 'textViewNew'
--
newTextView :: IO TextView
newTextView = textViewNew

-- | see 'toolbarNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedToolbar :: WidgetName -> IO Toolbar
newNamedToolbar name = newNamedWidget name toolbarNew 

-- | see 'toolbarNew'
--
newToolbar :: IO Toolbar
newToolbar = toolbarNew

-- Create a new 'TreeView' that displays that data of a 'TreeModel' (which
-- may either be a 'TreeStore' or a 'ListStore'.
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedTreeView :: TreeModelClass tm => WidgetName -> tm -> IO TreeView
newNamedTreeView name tm = newNamedWidget name $ treeViewNewWithModel tm

-- Create a new 'TreeView' that displays that data of a 'TreeModel' (which
-- may either be a 'TreeStore' or a 'ListStore'.
--
newTreeView :: TreeModelClass tm => tm -> IO TreeView
newTreeView tm = treeViewNewWithModel tm

-- | see 'textViewNewWithModel'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedTreeViewWithModel :: TreeModelClass tm => WidgetName -> tm -> 
						  IO TreeView
newNamedTreeViewWithModel name tm = 
  newNamedWidget name $ treeViewNewWithModel tm

-- | see 'textViewNewWithModel'
--
newTreeViewWithModel :: TreeModelClass tm => tm -> IO TreeView
newTreeViewWithModel = treeViewNewWithModel

-- | see 'treeViewColumnNew'
--
newTreeViewColumn :: IO TreeViewColumn
newTreeViewColumn = treeViewColumnNew

-- | see 'calendarNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedCalendar :: WidgetName -> IO Calendar
newNamedCalendar name = newNamedWidget name $ calendarNew

-- | see 'calendarNew'
--
newCalendar :: IO Calendar
newCalendar = calendarNew

-- | see 'drawingAreaNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
--newNamedDrawingArea :: WidgetName -> IO DrawingArea
--newNamedDrawingArea name = newNamedWidget name $ drawingAreaNew

-- | see 'drawingAreaNew'
--
--newDrawingArea :: IO DrawingArea
--newDrawingArea = drawingAreaNew

-- | see 'entryNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedEntry :: WidgetName -> IO Entry
newNamedEntry name = newNamedWidget name $ entryNew

-- | see 'entryNew'
--
newEntry :: IO Entry
newEntry = entryNew

-- | see 'spinButtonNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedSpinButton :: String -> Adjustment -> Double -> Int -> IO SpinButton
newNamedSpinButton name adj climbRate digits = newNamedWidget name $ 
 spinButtonNew adj climbRate digits

-- | see 'spinButtonNew'
--
newSpinButton :: Adjustment -> Double -> Int -> IO SpinButton
newSpinButton adj climbRate digits = spinButtonNew adj climbRate digits

-- | see 'spinButtonNewWithRange'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedSpinButtonWithRange :: 
  WidgetName -> Double -> Double -> Double -> IO SpinButton
newNamedSpinButtonWithRange name min max step = newNamedWidget name $
  spinButtonNewWithRange min max step

-- | see 'spinButtonNewWithRange'
--
newSpinButtonWithRange :: Double -> Double -> Double -> IO SpinButton
newSpinButtonWithRange min max step = spinButtonNewWithRange min max step

-- | see 'hScaleNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedHScale :: WidgetName -> Adjustment -> IO HScale
newNamedHScale name adj = newNamedWidget name $ hScaleNew adj

-- | see 'hScaleNew'
--
newHScale :: Adjustment -> IO HScale
newHScale adj = hScaleNew adj

-- | see 'vScaleNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedVScale :: WidgetName -> Adjustment -> IO VScale
newNamedVScale name adj = newNamedWidget name $ vScaleNew adj

-- | see 'vScaleNew'
--
newVScale :: Adjustment -> IO VScale
newVScale adj = vScaleNew adj

-- | see 'hScrollbarNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedHScrollbar :: WidgetName -> Adjustment -> IO HScrollbar
newNamedHScrollbar name adj = newNamedWidget name $ hScrollbarNew adj

-- | see 'hScrollbarNew'
--
newHScrollbar :: Adjustment -> IO HScrollbar
newHScrollbar adj = hScrollbarNew adj

-- | see 'vScrollbarNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedVScrollbar :: WidgetName -> Adjustment -> IO VScrollbar
newNamedVScrollbar name adj = newNamedWidget name $ vScrollbarNew adj

-- | see 'vScrollbarNew'
--
newVScrollbar :: Adjustment -> IO VScrollbar
newVScrollbar adj = vScrollbarNew adj

-- | see 'hSeparatorNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedHSeparator :: WidgetName -> IO HSeparator
newNamedHSeparator name = newNamedWidget name $ hSeparatorNew

-- | see 'hSeparatorNew'
--
newHSeparator :: IO HSeparator
newHSeparator = hSeparatorNew

-- | see 'vSeparatorNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedVSeparator :: WidgetName -> IO VSeparator
newNamedVSeparator name = newNamedWidget name $ vSeparatorNew

-- | see 'vSeparatorNew'
--
newVSeparator :: IO VSeparator
newVSeparator = vSeparatorNew

-- | see 'progressBarNew'
--
-- * The supplied name can later be used to lookup the widget in the global
--   store.
--
newNamedProgressBar :: WidgetName -> IO ProgressBar
newNamedProgressBar name = newNamedWidget name $ progressBarNew

-- | see 'progressBarNew'
--
newProgressBar :: IO ProgressBar
newProgressBar = progressBarNew

-- | see 'adjustmentNew'
--
newAdjustment :: Double -> Double -> Double -> Double -> Double -> Double -> 
		 IO Adjustment
newAdjustment value lower upper stepIncrement pageIncrement pageSize = 
  adjustmentNew value lower upper stepIncrement pageIncrement pageSize

-- | see 'iMContextNew'
--
--newIMContext :: IO iMContext
--newIMContext = iMContextNew

-- | see 'iMMulticontextNew'
--
--newIMMulticontext :: IO iMMulticontext
--newIMMulticontext = iMMulticontextNew

-- | see 'itemFactoryNew'
--
--newItemFactory :: IO ItemFactory
--newItemFactory = itemFactoryNew

-- | see 'tooltipsNew'
--
newTooltips :: IO Tooltips
newTooltips = tooltipsNew


-- | see 'name'
--
newIconFactory :: IO IconFactory
newIconFactory = iconFactoryNew

