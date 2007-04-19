{-# OPTIONS -cpp #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Structures
--
--  Author : Axel Simon
--
--  Created: 2 May 2001
--
--  Version $Revision: 1.16 $ from $Date: 2005/11/07 11:28:53 $
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
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
module Graphics.UI.Gtk.General.Structs (
  Point,
  Rectangle(..),
  Color(..),
  GCValues(..),
  pokeGCValues,
  newGCValues,
  widgetGetState,
  widgetGetSavedState,
  Allocation,
  Requisition(..),
  treeIterSize,
  textIterSize,
  inputError,
  dialogGetUpper,
  dialogGetActionArea,
  fileSelectionGetButtons,
  ResponseId(..),
  fromResponse,
  toResponse,
  --XID,
  --socketGetXID,
  --socketHasPlug,
#ifndef DISABLE_DEPRECATED
  toolbarChildButton,
  toolbarChildToggleButton,
  toolbarChildRadioButton,
#endif
  IconSize,
  iconSizeInvalid,
  iconSizeMenu,
  iconSizeSmallToolbar,
  iconSizeLargeToolbar,
  iconSizeButton,
  iconSizeDialog,
#ifndef DISABLE_DEPRECATED
  comboGetList,
#endif
  widgetGetDrawWindow,
  widgetGetSize,
  layoutGetDrawWindow,
  pangoScale,
  PangoDirection(..),
  pangodirToLevel,
  setAttrPos,
  pangoItemRawGetFont,
  pangoItemRawGetLanguage,
  pangoItemRawAnalysis,
  pangoItemRawGetLevel,
  styleGetForeground,
  styleGetBackground,
  styleGetLight,
  styleGetMiddle,
  styleGetDark,
  styleGetText,
  styleGetBase,
  styleGetAntiAliasing,
  colorSelectionDialogGetColor,
  colorSelectionDialogGetOkButton,
  colorSelectionDialogGetCancelButton,
  colorSelectionDialogGetHelpButton,
  dragContextGetActions,
  dragContextSetActions,
  dragContextGetSuggestedAction,
  dragContextSetSuggestedAction,
  dragContextGetAction,
  dragContextSetAction
  ) where

import Monad		(liftM)
import Data.IORef
import Control.Exception
import Data.Bits        (testBit)

import System.Glib.FFI
import System.Glib.UTFString ( UTFCorrection, ofsToUTF )
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
import System.Glib.GObject		(makeNewGObject)
import Graphics.UI.Gtk.Types
import Graphics.UI.Gtk.Gdk.Enums	(Function, Fill, SubwindowMode,
					 LineStyle, CapStyle, JoinStyle)
import Graphics.UI.Gtk.General.Enums	(StateType)

-- | Represents the x and y coordinate of a point.
--
type Point = (Int, Int)

-- | Rectangle
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

-- | Color
--
-- * Specifies a color with three integer values for red, green and blue.
--   All values range from 0 (least intense) to 65535 (highest intensity).
--
data Color = Color (#type guint16) (#type guint16) (#type guint16)

instance Storable Color where
  sizeOf _ = #{const sizeof(GdkColor)}
  alignment _ = alignment (undefined::#type guint32)
  peek ptr = do
    red	   <- #{peek GdkColor, red} ptr
    green  <- #{peek GdkColor, green} ptr
    blue   <- #{peek GdkColor, blue} ptr
    return $ Color red green blue
  poke ptr (Color red green blue) = do
    #{poke GdkColor, pixel} ptr (0::#{type gint32})
    #{poke GdkColor, red}   ptr red
    #{poke GdkColor, green} ptr green
    #{poke GdkColor, blue}  ptr blue
    cPtr <- gdkColormapGetSystem
    gdkColormapAllocColor cPtr ptr 0 1
    return ()

type ColorMap = ()

foreign import ccall unsafe "gdk_colormap_get_system"
  gdkColormapGetSystem :: IO (Ptr ColorMap)

foreign import ccall unsafe "gdk_colormap_alloc_color"
  gdkColormapAllocColor :: Ptr ColorMap -> Ptr Color -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "gdk_colormap_query_color"
  gdkColormapQueryColor :: Ptr ColorMap -> CULong -> Ptr Color -> IO ()

-- entry GC

-- | Intermediate data structure for 'GC's.
--
-- * If @graphicsExposure@ is set then copying portions into a
--   drawable will generate an @\"exposure\"@ event, even if the
--   destination area is not currently visible.
--
data GCValues = GCValues {
  foreground :: Color,
  background :: Color,
  function   :: Function,
  fill       :: Fill,
  tile       :: Maybe Pixmap,
  stipple    :: Maybe Pixmap,
  clipMask   :: Maybe Pixmap,
  subwindowMode :: SubwindowMode,
  tsXOrigin  :: Int,
  tsYOrigin  :: Int,
  clipXOrigin:: Int,
  clipYOrigin:: Int,
  graphicsExposure :: Bool,
  lineWidth  :: Int,
  lineStyle  :: LineStyle,
  capStyle   :: CapStyle,
  joinStyle  :: JoinStyle
  }

instance Storable GCValues where
  sizeOf _ = #{const sizeof(GdkGCValues)}
  alignment _ = alignment (undefined::Color)
  peek ptr = do
    -- gdk_gc_get_values does not fill in the r,g,b members of the foreground
    -- and background colours (it only fills in the allocated pixel value),
    -- so we have to fill them in here:
    let foregroundPtr, backgroundPtr :: Ptr Color
        foregroundPtr = #{ptr GdkGCValues, foreground} ptr
        backgroundPtr = #{ptr GdkGCValues, background} ptr
    (foregroundPixelPtr :: CULong) <- #{peek GdkColor, pixel} foregroundPtr
    (backgroundPixelPtr :: CULong) <- #{peek GdkColor, pixel} backgroundPtr
    colormapPtr <- gdkColormapGetSystem    
    gdkColormapQueryColor colormapPtr foregroundPixelPtr foregroundPtr
    gdkColormapQueryColor colormapPtr backgroundPixelPtr backgroundPtr

    foreground_ <- peek (#{ptr GdkGCValues, foreground} ptr)
    background_ <- peek (#{ptr GdkGCValues, background} ptr)
    (function_	:: #{type GdkFunction}) <- #{peek GdkGCValues, function} ptr
    (fill_	:: #{type GdkFill}) <- #{peek GdkGCValues, fill} ptr
    tile_	<- do
		     pPtr <- #{peek GdkGCValues, tile} ptr
		     if (pPtr==nullPtr) then return Nothing else
		       liftM Just $ makeNewGObject mkPixmap $ return pPtr
    stipple_	<- do
		     pPtr <- #{peek GdkGCValues, stipple} ptr
		     if (pPtr==nullPtr) then return Nothing else
		       liftM Just $ makeNewGObject mkPixmap $ return pPtr
    clipMask_	<- do
		     pPtr <- #{peek GdkGCValues, clip_mask} ptr
		     if (pPtr==nullPtr) then return Nothing else
		       liftM Just $ makeNewGObject mkPixmap $ return pPtr
    (subwindow_	:: #{type GdkSubwindowMode}) 
		<- #{peek GdkGCValues, subwindow_mode} ptr
    (tsXOrigin_	:: #{type gint}) 
		<- #{peek GdkGCValues, ts_x_origin} ptr
    (tsYOrigin_	:: #{type gint}) 
		<- #{peek GdkGCValues, ts_y_origin} ptr
    (clipXOrigin_:: #{type gint}) 
		<- #{peek GdkGCValues, clip_x_origin} ptr
    (clipYOrigin_:: #{type gint}) 
		<- #{peek GdkGCValues, clip_y_origin} ptr
    (graphics_	:: #{type gint})
		<- #{peek GdkGCValues, graphics_exposures} ptr
    (lineWidth_	:: #{type gint})
		<- #{peek GdkGCValues, line_width} ptr
    (lineStyle_	:: #{type GdkLineStyle}) 
		<- #{peek GdkGCValues, line_style} ptr
    (capStyle_	:: #{type GdkCapStyle}) 
		<- #{peek GdkGCValues, cap_style} ptr
    (joinStyle_	:: #{type GdkJoinStyle}) 
		<- #{peek GdkGCValues, join_style} ptr
    return $ GCValues {
      foreground = foreground_,
      background = background_,
      function   = (toEnum.fromIntegral) function_,
      fill       = (toEnum.fromIntegral) fill_,
      tile       = tile_,
      stipple    = stipple_,
      clipMask   = clipMask_,
      subwindowMode = (toEnum.fromIntegral) subwindow_,
      tsXOrigin  = fromIntegral tsXOrigin_,
      tsYOrigin  = fromIntegral tsYOrigin_,
      clipXOrigin= fromIntegral clipXOrigin_,
      clipYOrigin= fromIntegral clipYOrigin_,
      graphicsExposure = toBool graphics_,
      lineWidth  = fromIntegral lineWidth_,
      lineStyle  = (toEnum.fromIntegral) lineStyle_,
      capStyle   = (toEnum.fromIntegral) capStyle_,
      joinStyle  = (toEnum.fromIntegral) joinStyle_
    }

pokeGCValues :: Ptr GCValues -> GCValues -> IO CInt
pokeGCValues ptr (GCValues {
    foreground = foreground_,
    background = background_,
    function   = function_,
    fill       = fill_,
    tile       = tile_,
    stipple    = stipple_,
    clipMask   = clipMask_,
    subwindowMode = subwindow_,
    tsXOrigin  = tsXOrigin_,
    tsYOrigin  = tsYOrigin_,
    clipXOrigin= clipXOrigin_,
    clipYOrigin= clipYOrigin_,
    graphicsExposure = graphics_,
    lineWidth  = lineWidth_,
    lineStyle  = lineStyle_,
    capStyle   = capStyle_,
    joinStyle  = joinStyle_
  }) = do
    r <- newIORef 0
    add r #{const GDK_GC_FOREGROUND } $ 
      poke (#{ptr GdkGCValues, foreground} ptr) foreground_
    add r #{const GDK_GC_BACKGROUND } $ 
      poke (#{ptr GdkGCValues, background} ptr) background_
    add r #{const GDK_GC_FUNCTION } $ 
      #{poke GdkGCValues, function} ptr 
      (fromIntegral (fromEnum function_):: #{type GdkFunction})
    add r #{const GDK_GC_FILL } $
      #{poke GdkGCValues, fill} ptr 
      (fromIntegral (fromEnum fill_):: #{type GdkFill})
    case tile_ of 
      Nothing -> return ()
      Just tile_ -> add r #{const GDK_GC_TILE} $
                    withForeignPtr (unPixmap tile_) $
                    #{poke GdkGCValues, tile} ptr
    case stipple_ of
      Nothing -> return ()
      Just stipple_ -> add r #{const GDK_GC_STIPPLE} $
                       withForeignPtr (unPixmap stipple_) $
                       #{poke GdkGCValues, stipple} ptr
    case clipMask_ of
      Nothing -> return ()
      Just clipMask_ -> add r #{const GDK_GC_CLIP_MASK } $
                        withForeignPtr (unPixmap clipMask_) $
                        #{poke GdkGCValues, clip_mask} ptr
    add r #{const GDK_GC_SUBWINDOW } $
      #{poke GdkGCValues, subwindow_mode} ptr
      (fromIntegral (fromEnum subwindow_):: #{type GdkSubwindowMode})
    add r #{const GDK_GC_TS_X_ORIGIN } $
      #{poke GdkGCValues, ts_x_origin } ptr
      (fromIntegral tsXOrigin_:: #{type gint})
    add r #{const GDK_GC_TS_Y_ORIGIN } $
      #{poke GdkGCValues, ts_y_origin } ptr
      (fromIntegral tsYOrigin_:: #{type gint})
    add r #{const GDK_GC_CLIP_X_ORIGIN } $ 
      #{poke GdkGCValues, clip_x_origin } ptr
      (fromIntegral clipXOrigin_:: #{type gint})
    add r #{const GDK_GC_CLIP_Y_ORIGIN } $
      #{poke GdkGCValues, clip_y_origin } ptr
      (fromIntegral clipYOrigin_:: #{type gint})
    add r #{const GDK_GC_EXPOSURES } $
      #{poke GdkGCValues, graphics_exposures } ptr
      (fromBool graphics_:: #{type gint})
    add r #{const GDK_GC_LINE_WIDTH } $
      #{poke GdkGCValues, line_width } ptr
      (fromIntegral lineWidth_:: #{type gint})
    add r #{const GDK_GC_LINE_STYLE } $
      #{poke GdkGCValues, line_style } ptr
      (fromIntegral (fromEnum lineStyle_):: #{type GdkLineStyle})
    add r #{const GDK_GC_CAP_STYLE } $ 
      #{poke GdkGCValues, cap_style } ptr
      (fromIntegral (fromEnum capStyle_):: #{type GdkCapStyle})
    add r #{const GDK_GC_JOIN_STYLE } $ 
      #{poke GdkGCValues, join_style } ptr
      (fromIntegral (fromEnum joinStyle_):: #{type GdkJoinStyle})
    readIORef r
  where
    add :: IORef CInt -> CInt -> IO () -> IO ()
    add r mVal act = handle (const $ return ()) $ do
      act
      modifyIORef r (\val -> val+mVal)

-- constant newGCValues An empty record of 'GCValues'.
--
-- * Use this value instead of the constructor to avoid compiler wanings
--   about uninitialized fields.
--
newGCValues :: GCValues
newGCValues = GCValues {
    foreground = undefined,
    background = undefined,
    function   = undefined,
    fill       = undefined,
    tile       = Nothing,
    stipple    = Nothing,
    clipMask   = Nothing,
    subwindowMode = undefined,
    tsXOrigin  = undefined,
    tsYOrigin  = undefined,
    clipXOrigin= undefined,
    clipYOrigin= undefined,
    graphicsExposure = undefined,
    lineWidth  = undefined,
    lineStyle  = undefined,
    capStyle   = undefined,
    joinStyle  = undefined
  }

-- Widget related methods

-- | Retrieve the current state of the widget.
--
-- * The state refers to different modes of user interaction, see
--   'StateType' for more information.
--
widgetGetState :: WidgetClass w => w -> IO StateType
widgetGetState w = liftM toEnum $ withForeignPtr ((unWidget.toWidget) w) $
  \ptr -> #{peek GtkWidget,state} ptr

-- | Retrieve the current state of the widget.
--
-- * If a widget is turned insensitive, the previous state is stored in
--   a specific location. This function retrieves this previous state.
--
widgetGetSavedState :: WidgetClass w => w -> IO StateType
widgetGetSavedState w = liftM toEnum $ withForeignPtr ((unWidget.toWidget) w) $
  \ptr -> #{peek GtkWidget,saved_state} ptr


-- | Allocation
--
-- * for Widget's size_allocate signal
--
type Allocation = Rectangle


-- | Requisition
--
-- * for Widget's size_request
--
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


-- SpinButton related mothods

-- If an invalid input has been put into a SpinButton the input function may
-- reject this value by returning this value.
inputError :: #{type gint}
inputError = #{const GTK_INPUT_ERROR}


-- The TreeIter struct is not used by itself. But we have to allocate space
-- for it in module TreeModel.
treeIterSize :: Int
treeIterSize = #{const sizeof(GtkTreeIter)}


-- The TextIter struct can be a local variable in a C program. We have to
-- store it on the heap.
--
textIterSize :: Int
textIterSize = #{const sizeof(GtkTextIter)}

-- Dialog related methods

-- | Get the upper part of a dialog.
--
-- * The upper part of a dialog window consists of a 'VBox'.
--   Add the required widgets into this box.
--
dialogGetUpper :: DialogClass dc => dc -> IO VBox
dialogGetUpper dc = makeNewObject mkVBox $ liftM castPtr $
  withForeignPtr ((unDialog.toDialog) dc) #{peek GtkDialog, vbox}

-- | Extract the action area of a dialog box.
--
-- * This
-- is useful to add some special widgets that cannot be added with
-- dialogAddActionWidget.
--
dialogGetActionArea :: DialogClass dc => dc -> IO HBox
dialogGetActionArea dc = makeNewObject mkHBox $ liftM castPtr $
  withForeignPtr ((unDialog.toDialog) dc) #{peek GtkDialog, action_area} 

-- | Some constructors that can be used as response
-- numbers for dialogs.
--
data ResponseId

  -- | GTK returns this if a response widget has no @response_id@,
  --   or if the dialog gets programmatically hidden or destroyed.
  = ResponseNone

  -- | GTK won't return these unless you pass them in as
  --   the response for an action widget. They are for your convenience.
  | ResponseReject
  | ResponseAccept -- ^ (as above)

  -- | If the dialog is deleted.
  | ResponseDeleteEvent

  -- | \"Ok\" was pressed.
  --
  -- * This value is returned from the \"Ok\" stock dialog button.
  | ResponseOk

  -- | \"Cancel\" was pressed.
  --
  -- * These value is returned from the \"Cancel\" stock dialog button.
  | ResponseCancel

  -- | \"Close\" was pressed.
  --
  -- * This value is returned from the \"Close\" stock dialog button.
	| ResponseClose

  -- | \"Yes\" was pressed.
  --
  -- * This value is returned from the \"Yes\" stock dialog button.
  | ResponseYes

  -- | \"No\" was pressed.
  --
  -- * This value is returned from the \"No\" stock dialog button.
  | ResponseNo

  -- | \"Apply\" was pressed.
  --
  -- * This value is returned from the \"Apply\" stock dialog button.
	| ResponseApply

  -- |  \"Help\" was pressed.
  --
  -- * This value is returned from the \"Help\" stock dialog button.
  | ResponseHelp

  -- | A user-defined response
  --
  -- * This value is returned from a user defined button
  | ResponseUser Int
  deriving (Show, Eq)

fromResponse :: Integral a => ResponseId -> a
fromResponse ResponseNone = -1
fromResponse ResponseReject = -2
fromResponse ResponseAccept = -3
fromResponse ResponseDeleteEvent = -4
fromResponse ResponseOk = -5
fromResponse ResponseCancel = -6
fromResponse ResponseClose = -7
fromResponse ResponseYes = -8
fromResponse ResponseNo = -9
fromResponse ResponseApply = -10
fromResponse ResponseHelp = -11
fromResponse (ResponseUser i) | i > 0 = fromIntegral i

toResponse :: Integral a => a -> ResponseId
toResponse (-1) = ResponseNone
toResponse (-2) = ResponseReject
toResponse (-3) = ResponseAccept
toResponse (-4) = ResponseDeleteEvent
toResponse (-5) = ResponseOk
toResponse (-6) = ResponseCancel
toResponse (-7) = ResponseClose
toResponse (-8) = ResponseYes
toResponse (-9) = ResponseNo
toResponse (-10) = ResponseApply
toResponse (-11) = ResponseHelp
toResponse i | i > 0  = ResponseUser $ fromIntegral i

-- include<gdk/gdkx.h>

type XID = CUInt	-- unfortunately hsc and c2hs do not agree on the type
			-- of NativeWindow (Word32 vs. CUInt)

-- Query the XID field of the socket widget. This value needs to be
-- sent to the Plug widget of the other application.
--
--socketGetXID :: Socket -> IO XID
--socketGetXID socket = do
--  winPtr <- throwIfNull "socketGetXID: the socket widget is not realized" $
--    withForeignPtr (unSocket socket) #{peek GtkWidget, window}
--  implPtr <- throwIfNull "socketGetXID: no Drawable defined" $
--    #{peek GdkWindowObject, impl} winPtr
--  #{peek GdkDrawableImplX11, xid} implPtr


-- Test if a Plug is connected to the socket.
-- 
--socketHasPlug :: Socket -> IO Bool
--socketHasPlug socket = do
--  plugPtr <- withForeignPtr (unSocket socket) #{peek GtkSocket, plug_window}
--  return (plugPtr/=nullPtr)

#ifndef DISABLE_DEPRECATED
-- Static values for different Toolbar widgets.
--
-- * c2hs and hsc should agree on types!
--
toolbarChildButton, toolbarChildToggleButton, toolbarChildRadioButton ::
  CInt -- \#type GtkToolbarChildType
toolbarChildButton       = #const GTK_TOOLBAR_CHILD_BUTTON
toolbarChildToggleButton = #const GTK_TOOLBAR_CHILD_TOGGLEBUTTON
toolbarChildRadioButton  = #const GTK_TOOLBAR_CHILD_RADIOBUTTON
#endif

-- | The size of an icon in pixels.
--
type IconSize = Int

-- | Don't scale but use any of the available sizes.
iconSizeInvalid      :: IconSize
iconSizeInvalid      = #const GTK_ICON_SIZE_INVALID

-- | Icon size to use in next to menu items in drop-down menus.
iconSizeMenu	     :: IconSize
iconSizeMenu	     = #const GTK_ICON_SIZE_MENU

-- | Icon size for small toolbars.
iconSizeSmallToolbar :: IconSize
iconSizeSmallToolbar = #const GTK_ICON_SIZE_SMALL_TOOLBAR

-- | Icon size for larger toolbars.
iconSizeLargeToolbar :: IconSize
iconSizeLargeToolbar = #const GTK_ICON_SIZE_LARGE_TOOLBAR

-- | Icon size for icons in buttons, next to the label.
iconSizeButton	     :: IconSize
iconSizeButton	     = #const GTK_ICON_SIZE_BUTTON

-- | Icon size for icons next to dialog text.
iconSizeDialog	     :: IconSize
iconSizeDialog	     = #const GTK_ICON_SIZE_DIALOG

-- entry Widget Combo

#ifndef DISABLE_DEPRECATED
-- | Extract the List container from a 'Combo' box.
--
comboGetList :: Combo -> IO List
comboGetList c = withForeignPtr (unCombo c) $ \cPtr ->
  makeNewObject mkList $ #{peek GtkCombo, list} cPtr
#endif

-- FileSelection related methods

-- | Extract the buttons of a fileselection.
--
fileSelectionGetButtons :: FileSelectionClass fsel => fsel -> 
			   IO (Button, Button)
fileSelectionGetButtons fsel =
    do
    ok <- butPtrToButton #{peek GtkFileSelection, ok_button}
    cancel <- butPtrToButton #{peek GtkFileSelection, cancel_button}
    return (ok,cancel)
  where
  butPtrToButton bp = makeNewObject mkButton $ liftM castPtr $
      withForeignPtr ((unFileSelection . toFileSelection) fsel) bp

-- DrawingArea related methods

-- | Retrieves the 'DrawWindow' that the widget draws onto.
--
-- This may be @Nothing@ if the widget has not yet been realized, since a
-- widget does not allocate its window resources until just before it is
-- displayed on the screen. You can use the
-- 'Graphics.UI.Gtk.Abstract.Widget.onRealize' signal to give you the
-- opportunity to use a widget's 'DrawWindow' as soon as it has been created
-- but before the widget is displayed.
--
widgetGetDrawWindow :: WidgetClass widget => widget -> IO DrawWindow
widgetGetDrawWindow da =
  withForeignPtr (unWidget.toWidget $ da) $ \da' -> do
  drawWindowPtr <- #{peek GtkWidget, window} da'
  if drawWindowPtr == nullPtr
    then fail "widgetGetDrawWindow: no DrawWindow available (the widget is probably not realized)"
    else makeNewGObject mkDrawWindow (return $ castPtr drawWindowPtr)

-- | Returns the current size.
--
-- * This information may be out of date if the user is resizing the window.
--
widgetGetSize :: WidgetClass widget => widget -> IO (Int, Int)
widgetGetSize da = withForeignPtr (unWidget.toWidget $ da) $ \wPtr -> do
    (width :: #{type gint}) <- #{peek GtkAllocation, width} 
			       (#{ptr GtkWidget, allocation} wPtr)
    (height :: #{type gint}) <- #{peek GtkAllocation, height}
				(#{ptr GtkWidget, allocation} wPtr)
    return (fromIntegral width, fromIntegral height)

-- Layout related methods

-- | Retrieves the 'Drawable' part.
--
layoutGetDrawWindow :: Layout -> IO DrawWindow
layoutGetDrawWindow lay = makeNewGObject mkDrawWindow $
  withForeignPtr (unLayout lay) $
  \lay' -> liftM castPtr $ #{peek GtkLayout, bin_window} lay'

-- PangoLayout related constant

-- Internal unit of measuring sizes.
--
-- * This constant represents the scale between
--   dimensions used for distances in text rendering and Pango device units.
--   The
--   definition of device unit is dependent on the output device; it will
--   typically be pixels for a screen, and points for a printer.  When
--   setting font sizes, device units are always considered to be points
--   (as in \"12 point font\"), rather than pixels.
--
pangoScale :: Double
pangoScale = #const PANGO_SCALE

-- | The 'PangoDirection' type represents a direction in the Unicode
-- bidirectional algorithm.
--
-- * The \"weak\" values denote a left-to-right or right-to-left direction
--   only if there is no character with a strong direction in a paragraph.
--   An example is a sequence of special, graphical characters which are
--   neutral with respect to their rendering direction. A fresh
--   'Graphics.UI.Gtk.Pango.Rendering.PangoContext' is by default weakly
--   left-to-right.
--
-- * Not every value in this enumeration makes sense for every usage
--   of 'PangoDirection'; for example, the return value of
--   'unicharDirection' and 'findBaseDir' cannot be 'PangoDirectionWeakLtr'
--   or 'PangoDirectionWeakRtl', since every character is either neutral or
--   has a strong direction; on the other hand 'PangoDirectionNeutral'
--   doesn't make sense to pass to 'log2visGetEmbeddingLevels'.
--
data PangoDirection = PangoDirectionLtr
                    | PangoDirectionRtl
#if PANGO_CHECK_VERSION(1,4,0)
                    | PangoDirectionWeakLtr
                    | PangoDirectionWeakRtl
                    | PangoDirectionNeutral
#endif
                    deriving (Eq,Ord)

instance Enum PangoDirection where
  fromEnum PangoDirectionLtr        = #{const PANGO_DIRECTION_LTR }
  fromEnum PangoDirectionRtl        = #{const PANGO_DIRECTION_RTL }
#if PANGO_CHECK_VERSION(1,4,0)
  fromEnum PangoDirectionWeakLtr    = #{const PANGO_DIRECTION_WEAK_LTR }
  fromEnum PangoDirectionWeakRtl    = #{const PANGO_DIRECTION_WEAK_RTL }
  fromEnum PangoDirectionNeutral    = #{const PANGO_DIRECTION_NEUTRAL }
#endif
  toEnum #{const PANGO_DIRECTION_LTR } = PangoDirectionLtr
  toEnum #{const PANGO_DIRECTION_RTL } = PangoDirectionRtl
  toEnum #{const PANGO_DIRECTION_TTB_LTR } = PangoDirectionLtr
  toEnum #{const PANGO_DIRECTION_TTB_RTL } = PangoDirectionRtl
#if PANGO_CHECK_VERSION(1,4,0)
  toEnum #{const PANGO_DIRECTION_WEAK_LTR } = PangoDirectionWeakLtr
  toEnum #{const PANGO_DIRECTION_WEAK_RTL } = PangoDirectionWeakRtl
  toEnum #{const PANGO_DIRECTION_NEUTRAL } = PangoDirectionNeutral
#endif

-- This is a copy of the local function direction_simple in pango-layout.c
pangodirToLevel :: PangoDirection -> Int
pangodirToLevel PangoDirectionLtr = 1
pangodirToLevel PangoDirectionRtl = -1
#if PANGO_CHECK_VERSION(1,4,0)
pangodirToLevel PangoDirectionWeakLtr = 1
pangodirToLevel PangoDirectionWeakRtl = -1
pangodirToLevel PangoDirectionNeutral = 0
#endif

-- Get the font of a PangoAnalysis within a PangoItem.
pangoItemRawGetFont :: Ptr pangoItem -> IO Font
pangoItemRawGetFont ptr =
  makeNewGObject mkFont (#{peek PangoItem, analysis.font} ptr)

-- Get the font of a PangoAnalysis within a PangoItem.
pangoItemRawGetLanguage :: Ptr pangoItem -> IO (Ptr CChar)
pangoItemRawGetLanguage ptr =
  #{peek PangoItem, analysis.language} ptr

-- Get the PangoAnalysis within a PangoItem
pangoItemRawAnalysis :: Ptr pangoItem -> Ptr pangoAnalysis
pangoItemRawAnalysis = #{ptr PangoItem, analysis}

-- Get the text direction of this PangoItem.
pangoItemRawGetLevel :: Ptr pangoItem -> IO Bool
pangoItemRawGetLevel ptr = do
  level <- #{peek PangoItem, analysis.level} ptr
  return (toBool (level :: #{type guint8}))

-- Set the start and end position of an attribute
setAttrPos :: UTFCorrection -> Int -> Int -> IO (Ptr ()) -> IO (Ptr ())
setAttrPos correct start end act = do
  atPtr <- act
  #{poke PangoAttribute, start_index} atPtr
    (fromIntegral (ofsToUTF start correct) :: #{type guint})
  #{poke PangoAttribute, end_index} atPtr
    (fromIntegral (ofsToUTF end correct) :: #{type guint})
  return atPtr

-- Styles related methods

-- | Retrieve the the foreground color.
--
-- * The parameter @state@ determines for which widget
--   state (one of 'StateType') the 'Color' should be retrieved.
--   Use 'widgetGetState' to determine the current state of the
--   widget.
--
styleGetForeground :: Style -> StateType -> IO Color
styleGetForeground st ty =
  withForeignPtr (unStyle st) $ \stPtr -> do
    peekElemOff (#{ptr GtkStyle, fg} stPtr) (fromEnum ty)

-- | Retrieve the background color.
--
-- * The parameter @state@ determines for which widget
--   state (one of 'StateType') the 'Color' should be retrieved.
--   Use 'widgetGetState' to determine the current state of the
--   widget.
--
styleGetBackground :: Style -> StateType -> IO Color
styleGetBackground st ty =
  withForeignPtr (unStyle st) $ \stPtr ->
    peekElemOff (#{ptr GtkStyle, bg} stPtr) (fromEnum ty)

-- | Retrieve a light color.
--
-- * The parameter @state@ determines for which widget
--   state (one of 'StateType') the 'Color' should be retrieved.
--   Use 'widgetGetState' to determine the current state of the
--   widget.
--
styleGetLight :: Style -> StateType -> IO Color
styleGetLight st ty =
  withForeignPtr (unStyle st) $ \stPtr ->
    peekElemOff (#{ptr GtkStyle, light} stPtr) (fromEnum ty)

-- | Retrieve a middle color.
--
-- * The parameter @state@ determines for which widget
--   state (one of 'StateType') the 'Color' should be retrieved.
--   Use 'widgetGetState' to determine the current state of the
--   widget.
--
styleGetMiddle :: Style -> StateType -> IO Color
styleGetMiddle st ty =
  withForeignPtr (unStyle st) $ \stPtr ->
    peekElemOff (#{ptr GtkStyle, mid} stPtr) (fromEnum ty)

-- | Retrieve a dark color.
--
-- * The parameter @state@ determines for which widget
--   state (one of 'StateType') the 'Color' should be retrieved.
--   Use 'widgetGetState' to determine the current state of the
--   widget.
--
styleGetDark :: Style -> StateType -> IO Color
styleGetDark st ty =
  withForeignPtr (unStyle st) $ \stPtr ->
    peekElemOff (#{ptr GtkStyle, dark} stPtr) (fromEnum ty)

-- | Retrieve the text color.
--
-- * The parameter @state@ determines for which widget
--   state (one of 'StateType') the 'Color' should be retrieved.
--   Use 'widgetGetState' to determine the current state of the
--   widget.
--
styleGetText :: Style -> StateType -> IO Color
styleGetText st ty =
  withForeignPtr (unStyle st) $ \stPtr ->
    peekElemOff (#{ptr GtkStyle, text} stPtr) (fromEnum ty)

-- | Retrieve the base color.
--
-- * The base color is the standard text background of a widget.
--
-- * The parameter @state@ determines for which widget
--   state (one of 'StateType') the 'Color' should be retrieved.
--   Use 'widgetGetState' to determine the current state of the
--   widget.
--
styleGetBase :: Style -> StateType -> IO Color
styleGetBase st ty =
  withForeignPtr (unStyle st) $ \stPtr ->
    peekElemOff (#{ptr GtkStyle, base} stPtr) (fromEnum ty)

-- | Retrieve the color for drawing anti-aliased text.
--
-- * The anti-aliasing color is the color which is used when the rendering
--   of a character does not make it clear if a certain pixel shoud be set
--   or not. This color is between the text and the base color.
--
-- * The parameter @state@ determines for which widget
--   state (one of 'StateType') the 'Color' should be retrieved.
--   Use 'widgetGetState' to determine the current state of the
--   widget.
--
styleGetAntiAliasing :: Style -> StateType -> IO Color
styleGetAntiAliasing st ty =
  withForeignPtr (unStyle st) $ \stPtr ->
    peekElemOff (#{ptr GtkStyle, text_aa} stPtr) (fromEnum ty)

-- | Retrieve the ColorSelection object contained within the dialog.
colorSelectionDialogGetColor :: ColorSelectionDialog -> IO ColorSelection
colorSelectionDialogGetColor cd =
  makeNewObject mkColorSelection $ liftM castPtr $
    withForeignPtr (unColorSelectionDialog cd)
      #{peek GtkColorSelectionDialog, colorsel}

-- | Retrieve the OK button widget contained within the dialog.
colorSelectionDialogGetOkButton :: ColorSelectionDialog -> IO Button
colorSelectionDialogGetOkButton cd =
  makeNewObject mkButton $ liftM castPtr $
    withForeignPtr (unColorSelectionDialog cd)
      #{peek GtkColorSelectionDialog, ok_button}

-- | Retrieve the Cancel button widget contained within the dialog.
colorSelectionDialogGetCancelButton :: ColorSelectionDialog -> IO Button
colorSelectionDialogGetCancelButton cd =
  makeNewObject mkButton $ liftM castPtr $
    withForeignPtr (unColorSelectionDialog cd)
      #{peek GtkColorSelectionDialog, cancel_button}

-- | Retrieve the Help button widget contained within the dialog.
colorSelectionDialogGetHelpButton :: ColorSelectionDialog -> IO Button
colorSelectionDialogGetHelpButton cd =
  makeNewObject mkButton $ liftM castPtr $
    withForeignPtr (unColorSelectionDialog cd)
      #{peek GtkColorSelectionDialog, help_button}

dragContextGetActions :: DragContext -> IO Int
dragContextGetActions dc = liftM (fromIntegral :: #{type int} -> Int) $
  withForeignPtr (unDragContext dc) #{peek GdkDragContext, actions}

dragContextSetActions :: DragContext -> Int -> IO ()
dragContextSetActions dc val = withForeignPtr (unDragContext dc) $ \ptr ->
  #{poke GdkDragContext, actions} ptr (fromIntegral val :: #{type int})

dragContextGetAction :: DragContext -> IO Int
dragContextGetAction dc = liftM (fromIntegral :: #{type int} -> Int) $
  withForeignPtr (unDragContext dc) #{peek GdkDragContext, action}

dragContextSetAction :: DragContext -> Int -> IO ()
dragContextSetAction dc val = withForeignPtr (unDragContext dc) $ \ptr ->
  #{poke GdkDragContext, action} ptr (fromIntegral val :: #{type int})

dragContextGetSuggestedAction :: DragContext -> IO Int
dragContextGetSuggestedAction dc = liftM (fromIntegral :: #{type int} -> Int) $
  withForeignPtr (unDragContext dc) #{peek GdkDragContext, suggested_action}

dragContextSetSuggestedAction :: DragContext -> Int -> IO ()
dragContextSetSuggestedAction dc val = withForeignPtr (unDragContext dc) $ \ptr ->
  #{poke GdkDragContext, suggested_action} ptr (fromIntegral val :: #{type int})
