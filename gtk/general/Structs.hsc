-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: Marshaling of structures
--
--  Author : Axel Simon
--          
--  Created: 2 May 2001
--
--  Version $Revision: 1.2 $ from $Date: 2002/05/04 14:02:30 $
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
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------

module Structs(
  Rectangle(..),	-- data type providing a rectangle
  Allocation,
  Requisition(..),
  treeIterSize,
  textIterSize,
  inputError,
  dialogGetUpper,
  dialogGetActionArea,
  ResponseId,
  responseNone,
  responseReject,
  responseAccept,
  responseDeleteEvent,
  responseOk,
  responseCancel,
  responseClose,
  responseYes,
  responseNo,
  responseApply,
  responseHelp,
  XID,
--  socketGetXID,
  socketHasPlug,
  toolbarGetSize',
  toolbarChildButton,
  toolbarChildToggleButton,
  toolbarChildRadioButton,
  IconSize,
  iconSizeInvalid,
  iconSizeMenu,
  iconSizeSmallToolbar,
  iconSizeLargeToolbar,
  iconSizeButton,
  iconSizeDialog,
  checkMenuItemGetActive,
  comboGetList,
  priorityLow,
  priorityDefault,
  priorityHigh,
  nullForeignPtr
  ) where

import Monad		(liftM)
import Foreign
import UTFCForeign
import IOExts		(unsafePerformIO)	-- for nullForeignPtr
import Object		(makeNewObject)
import Hierarchy
import Bits		(testBit)

#include <gtk/gtk.h>

-- Rectangle (EXPORTED)
--
-- * for Events
--
-- * Specifies x, y, width and height
--
data Rectangle = Rectangle Int Int Int Int

instance Storable Rectangle where
  sizeOf _ = #{const sizeof(GdkRectangle)}
  alignment _ = alignment (undefined:: #type gint)
  peek ptr = do
    (x_	     ::#type gint)	<- #{peek GdkRectangle, x} ptr
    (y_	     ::#type gint)	<- #{peek GdkRectangle, y} ptr
    (width_  ::#type gint)	<- #{peek GdkRectangle, width} ptr
    (height_ ::#type gint)	<- #{peek GdkRectangle, height} ptr
    return $ Rectangle (fromIntegral x_) (fromIntegral y_) 
		       (fromIntegral width_) (fromIntegral height_)
  poke ptr (Rectangle x y width height) = do
    #{poke GdkRectangle, x} ptr ((fromIntegral x)::#type gint)
    #{poke GdkRectangle, y} ptr ((fromIntegral y)::#type gint)
    #{poke GdkRectangle, width} ptr ((fromIntegral width)::#type gint)
    #{poke GdkRectangle, height} ptr ((fromIntegral height)::#type gint)
  destruct = free

-- Allocation (EXPORTED)
-- * for Widget's size_allocate signal
type Allocation = Rectangle


-- Requisition (EXPORTED)
-- * for Widget's size_request
data Requisition = Requisition Int Int

instance Storable Requisition where
  sizeOf _ = #{const sizeof(GtkRequisition)}
  alignment _ = alignment (undefined::#type gint)
  peek ptr = do
    (width_  ::#type gint)	<- #{peek GtkRequisition, width} ptr
    (height_ ::#type gint)	<- #{peek GtkRequisition, width} ptr
    return $ Requisition (fromIntegral width_) (fromIntegral height_)
  poke ptr (Requisition width height) = do
    #{poke GtkRequisition, width} ptr ((fromIntegral width)::#type gint)
    #{poke GtkRequisition, height} ptr ((fromIntegral height)::#type gint)
  destruct = free

-- If an invalid input has been put into a SpinButton the input function may
-- reject this value by returning this value.
inputError :: #{type gint}
inputError = #{const GTK_INPUT_ERROR}

-- The @TreeIter struct is not used by itself. But we have to allocate space
-- for it in module TreeModel.
treeIterSize :: Int
treeIterSize = #{const sizeof(GtkTreeIter)}


-- The @TextIter struct can be a local variable in a C program. We have to
-- store it on the heap.
--
textIterSize :: Int
textIterSize = #{const sizeof(GtkTextIter)}

-- Get the upper part of a dialog where interaction widget should be added.
-- (EXPORTED).
--
dialogGetUpper :: DialogClass dc => dc -> IO VBox
dialogGetUpper dc = makeNewObject mkVBox $ liftM castPtr $
  withForeignPtr ((unDialog.toDialog) dc) #{peek GtkDialog, vbox}

-- Extract the action area of a dialog box. This is useful to add some special
-- widgets that cannot be added with dialogAddActionWidget. (EXPORTED)
--
dialogGetActionArea :: DialogClass dc => dc -> IO HBox
dialogGetActionArea dc = makeNewObject mkHBox $ liftM castPtr $
  withForeignPtr ((unDialog.toDialog) dc) #{peek GtkDialog, action_area} 

-- Here are some constants that can be used as response numbers for dialogs.
-- (EXPORTED)
--
type ResponseId = Int

-- GTK returns this if a response widget has no response_id, or if the dialog
-- gets programmatically hidden or destroyed. (EXPORTED)
responseNone :: ResponseId
responseNone = -1

-- GTK won't return these unless you pass them in as the response for an 
-- action widget. They are for your convenience. (EXPORTED)
--
responseReject :: ResponseId
responseReject = -2

responseAccept :: ResponseId
responseAccept = -3

-- If the dialog is deleted. (EXPORTED)
responseDeleteEvent :: ResponseId
responseDeleteEvent = -4

-- These are returned from GTK dialogs, and you can also use them yourself if
-- you like. (EXPORTED)
responseOk :: ResponseId
responseOk = -5

responseCancel :: ResponseId
responseCancel = -6

responseClose :: ResponseId
responseClose = -7 

responseYes :: ResponseId
responseYes = -8

responseNo :: ResponseId
responseNo = -9

responseApply :: ResponseId
responseApply = -10

responseHelp :: ResponseId
responseHelp = -11

#include<gdk/gdkx.h>

type XID = CUInt	-- unfortunately hsc and c2hs do not agree on the type
			-- of GdkNativeWindow (Word32 vs. CUInt)

-- Query the XID field of the socket widget. This value needs to be sent
-- to the Plug widget of the other application. (EXPORTED)
--socketGetXID :: Socket -> IO XID
--socketGetXID socket = do
--  winPtr <- throwIfNull "socketGetXID: the socket widget is not realized" $
--    withForeignPtr (unSocket socket) #{peek GtkWidget, window}
--  implPtr <- throwIfNull "socketGetXID: no GdkDrawable defined" $
--    #{peek GdkWindowObject, impl} winPtr
--  #{peek GdkDrawableImplX11, xid} implPtr

-- Test if a Plug is connected to the socket.
-- 
socketHasPlug :: Socket -> IO Bool
socketHasPlug socket = do
  plugPtr <- withForeignPtr (unSocket socket) #{peek GtkSocket, plug_window}
  return (plugPtr/=nullPtr)

-- Get the current size of the @Button@s in a @Toolbar. The value is not
-- mangled.
--
toolbarGetSize' :: Toolbar -> IO IconSize
toolbarGetSize' tb = withForeignPtr (unToolbar tb) #peek GtkToolbar, icon_size

-- Static values for different @Toolbar widgets.
--
-- * c2hs and hsc should agree on types!
--
toolbarChildButton, toolbarChildToggleButton, toolbarChildRadioButton ::
  CInt -- #type GtkToolbarChildType
toolbarChildButton       = #const GTK_TOOLBAR_CHILD_BUTTON
toolbarChildToggleButton = #const GTK_TOOLBAR_CHILD_TOGGLEBUTTON
toolbarChildRadioButton  = #const GTK_TOOLBAR_CHILD_RADIOBUTTON

-- IconSize is an enumeration in Gtk that can be extended by the user by adding
-- new names for sizes.
type IconSize = Int

iconSizeInvalid      :: IconSize
iconSizeInvalid      = #const GTK_ICON_SIZE_INVALID
iconSizeMenu	     :: IconSize
iconSizeMenu	     = #const GTK_ICON_SIZE_MENU
iconSizeSmallToolbar :: IconSize
iconSizeSmallToolbar = #const GTK_ICON_SIZE_SMALL_TOOLBAR
iconSizeLargeToolbar :: IconSize
iconSizeLargeToolbar = #const GTK_ICON_SIZE_LARGE_TOOLBAR
iconSizeButton	     :: IconSize
iconSizeButton	     = #const GTK_ICON_SIZE_BUTTON
iconSizeDialog	     :: IconSize
iconSizeDialog	     = #const GTK_ICON_SIZE_DIALOG

-- Return the current state of the check of the @CheckMenuItem. (EXPORTED)
--
checkMenuItemGetActive :: CheckMenuItemClass mi => mi -> IO Bool
checkMenuItemGetActive mi = 
  withForeignPtr ((unCheckMenuItem.toCheckMenuItem) mi) $ \miPtr -> do
    let actPtr = miPtr `plusPtr` #const sizeof(GtkMenuItem)
    act <- peek (actPtr::Ptr #type guint)
    return $ testBit act 1

-- Extract the List container from a @Combo box. (EXPORTED)
--
comboGetList :: Combo -> IO List
comboGetList c = withForeignPtr (unCombo c) $ \cPtr ->
  makeNewObject mkList $ #{peek GtkCombo, list} cPtr

-- For installing idle callbacks: Priorities. (EXPORTED)
--
priorityHigh :: Int
priorityHigh    = #const G_PRIORITY_HIGH_IDLE

priorityDefault :: Int
priorityDefault = #const G_PRIORITY_DEFAULT_IDLE

priorityLow :: Int
priorityLow	= #const G_PRIORITY_LOW


-- helper function: nullForeignPtr
-- this must be a performance hit
nullForeignPtr :: ForeignPtr a
nullForeignPtr = unsafePerformIO $ newForeignPtr nullPtr (return ())









