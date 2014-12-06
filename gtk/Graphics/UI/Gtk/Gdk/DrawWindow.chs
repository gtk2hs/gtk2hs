{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) DrawWindow
--
--  Author : Axel Simon
--
--  Created: 5 November 2002
--
--  Copyright (C) 2002-2005 Axel Simon
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
-- A 'DrawWindow' is a rectangular region on the screen.
--
module Graphics.UI.Gtk.Gdk.DrawWindow (
-- A 'DrawWindow' is used to implement high-level objects such as 'Widget' and
-- 'Window' on the Gtk+ level.
--
-- Most widgets draws its content into a 'DrawWindow', in particular
-- 'DrawingArea' is nothing but a widget that contains a 'DrawWindow'.
-- This object derives from 'Drawable' which defines the basic drawing
-- primitives.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Drawable'
-- |         +----DrawWindow
-- @
--

-- * Types
  DrawWindow,
  DrawWindowClass,
  castToDrawWindow, gTypeDrawWindow,
  WindowState(..),
  NativeWindowId,
  toNativeWindowId,
  fromNativeWindowId,
-- * Methods
  drawWindowGetState,
  drawWindowScroll,
#if GTK_MAJOR_VERSION < 3
  drawWindowClear,
  drawWindowClearArea,
  drawWindowClearAreaExpose,
#endif
  drawWindowRaise,
  drawWindowLower,
  drawWindowRegisterDnd,
  drawWindowBeginPaintRect,
#if GTK_MAJOR_VERSION < 3
  drawWindowBeginPaintRegion,
#endif
  drawWindowEndPaint,
  drawWindowInvalidateRect,
#if GTK_MAJOR_VERSION < 3
  drawWindowInvalidateRegion,
  drawWindowGetUpdateArea,
#endif
  drawWindowFreezeUpdates,
  drawWindowThawUpdates,
  drawWindowProcessUpdates,
#if GTK_CHECK_VERSION(2,4,0)
  drawWindowSetAcceptFocus,
#endif
#if GTK_MAJOR_VERSION < 3
  drawWindowShapeCombineMask,
  drawWindowShapeCombineRegion,
#endif
  drawWindowSetChildShapes,
  drawWindowMergeChildShapes,
  drawWindowGetPointer,
  drawWindowGetPointerPos,
  drawWindowGetOrigin,
  drawWindowSetCursor,
#if GTK_MAJOR_VERSION < 3 && !defined(HAVE_QUARTZ_GTK) && !defined(GDK_WINDOWING_QUARTZ)
  drawWindowForeignNew,
#endif
  drawWindowGetDefaultRootWindow,
#if GTK_CHECK_VERSION(2,24,0)
  drawWindowGetWidth,
  drawWindowGetHeight,
#endif
  ) where

import Control.Monad    (liftM)
import Data.Maybe       (fromMaybe)

import System.Glib.FFI
import System.Glib.Flags                (toFlags)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Gdk.Enums#}
#if GTK_MAJOR_VERSION < 3
{#import Graphics.UI.Gtk.Gdk.Region#}
#endif
{#import Graphics.UI.Gtk.Gdk.Cursor#}
import Graphics.UI.Gtk.General.Structs

{# context lib="gdk" prefix="gdk" #}

-- | Gets the bitwise OR of the currently active drawWindow state flags, from
-- the 'WindowState' enumeration.
--
drawWindowGetState :: DrawWindowClass self => self
 -> IO [WindowState] -- ^ returns @DrawWindow@ flags
drawWindowGetState self =
  liftM (toFlags . fromIntegral) $
  {# call gdk_window_get_state #}
     (toDrawWindow self)

-- | Scroll the contents of @DrawWindow@.
--
-- * Scroll both, pixels and children, by the given amount.
--   @DrawWindow@ itself does not move. Portions of the window that the
-- scroll operation brings inm from offscreen areas are invalidated. The
-- invalidated region may be bigger than what would strictly be necessary. (For
-- X11, a minimum area will be invalidated if the window has no subwindows, or
-- if the edges of the window's parent do not extend beyond the edges of the
-- drawWindow. In other cases, a multi-step process is used to scroll the window
-- which may produce temporary visual artifacts and unnecessary invalidations.)
--
drawWindowScroll :: DrawWindowClass self => self
 -> Int   -- ^ @dx@ - Amount to scroll in the X direction
 -> Int   -- ^ @dy@ - Amount to scroll in the Y direction
 -> IO ()
drawWindowScroll self dx dy =
  {# call gdk_window_scroll #}
     (toDrawWindow self)
     (fromIntegral dx)
     (fromIntegral dy)

#if GTK_MAJOR_VERSION < 3
-- | Clears an entire @DrawWindow@ to the background color or background pixmap.
--
-- Removed in Gtk3.
drawWindowClear :: DrawWindowClass self => self -> IO ()
drawWindowClear self =
  {# call gdk_window_clear #}
     (toDrawWindow self)

-- | Clears an area of @DrawWindow@ to the background color or background pixmap.
--
-- Removed in Gtk3.
drawWindowClearArea :: DrawWindowClass self => self
 -> Int   -- ^ @x@ - x coordinate of rectangle to clear
 -> Int   -- ^ @y@ - y coordinate of rectangle to clear
 -> Int   -- ^ @width@ - width of rectangle to clear
 -> Int   -- ^ @height@ - height of rectangle to clear
 -> IO ()
drawWindowClearArea self x y width height =
  {# call gdk_window_clear_area #}
     (toDrawWindow self)
     (fromIntegral x)
     (fromIntegral y)
     (fromIntegral width)
     (fromIntegral height)

-- | Like 'drawWindowClearArea', but also generates an expose event for the
-- cleared area.
--
-- Removed in Gtk3.
drawWindowClearAreaExpose :: DrawWindowClass self => self
 -> Int   -- ^ @x@ - x coordinate of rectangle to clear
 -> Int   -- ^ @y@ - y coordinate of rectangle to clear
 -> Int   -- ^ @width@ - width of rectangle to clear
 -> Int   -- ^ @height@ - height of rectangle to clear
 -> IO ()
drawWindowClearAreaExpose self x y width height =
  {# call gdk_window_clear_area_e #}
     (toDrawWindow self)
     (fromIntegral x)
     (fromIntegral y)
     (fromIntegral width)
     (fromIntegral height)
#endif

-- | Raises @DrawWindow@ to the top of the Z-order (stacking order), so that other
-- drawWindows with the same parent drawWindow appear below @DrawWindow@. This is true
-- whether or not the drawWindows are visible.
--
-- If @DrawWindow@ is a toplevel, the window manager may choose to deny the
-- request to move the drawWindow in the Z-order, 'drawWindowRaise' only requests the
-- restack, does not guarantee it.
--
drawWindowRaise :: DrawWindowClass self => self -> IO ()
drawWindowRaise self =
  {# call gdk_window_raise #}
     (toDrawWindow self)

-- | Lowers @DrawWindow@ to the bottom of the Z-order (stacking order), so that
-- other windows with the same parent window appear above @DrawWindow@. This is
-- true whether or not the other windows are visible.
--
-- If @DrawWindow@ is a toplevel, the window manager may choose to deny the
-- request to move the drawWindow in the Z-order, 'drawWindowLower' only
-- requests the restack, does not guarantee it.
--
-- Note that a widget is raised automatically when it is mapped, thus you
-- need to call 'drawWindowLower' after
        -- 'Graphics.UI.Gtk.Abstract.Widget.widgetShow' if the window should
-- not appear above other windows.
--
drawWindowLower :: DrawWindowClass self => self -> IO ()
drawWindowLower self =
  {# call gdk_window_lower #}
     (toDrawWindow self)

-- | Registers a drawWindow as a potential drop destination.
--
drawWindowRegisterDnd :: DrawWindowClass self => self -> IO ()
drawWindowRegisterDnd self =
  {# call gdk_window_register_dnd #}
     (toDrawWindow self)

-- | A convenience wrapper around 'drawWindowBeginPaintRegion' which creates a
-- rectangular region for you.
--
-- * See 'drawWindowBeginPaintRegion' for details.
--
drawWindowBeginPaintRect :: DrawWindowClass self => self
 -> Rectangle -- ^ @rectangle@ - rectangle you intend to draw to
 -> IO ()
drawWindowBeginPaintRect self rectangle = with rectangle $ \rectPtr ->
  {#call gdk_window_begin_paint_rect#} (toDrawWindow self) (castPtr rectPtr)

#if GTK_MAJOR_VERSION < 3
-- | Indicate that you are beginning the process of redrawing @region@.
--
-- * A
-- backing store (offscreen buffer) large enough to contain @region@ will be
-- created. The backing store will be initialized with the background color or
-- background pixmap for @DrawWindow@. Then, all drawing operations performed on
-- @DrawWindow@ will be diverted to the backing store. When you call
-- 'drawWindowEndPaint', the backing store will be copied to @DrawWindow@, making it
-- visible onscreen. Only the part of @DrawWindow@ contained in @region@ will be
-- modified; that is, drawing operations are clipped to @region@.
--
-- The net result of all this is to remove flicker, because the user sees
-- the finished product appear all at once when you call 'drawWindowEndPaint'. If
-- you draw to @DrawWindow@ directly without calling 'drawWindowBeginPaintRegion', the
-- user may see flicker as individual drawing operations are performed in
-- sequence. The clipping and background-initializing features of
-- 'drawWindowBeginPaintRegion' are conveniences for the programmer, so you can
-- avoid doing that work yourself.
--
-- When using GTK+, the widget system automatically places calls to
-- 'drawWindowBeginPaintRegion' and 'drawWindowEndPaint' around emissions of the
-- @expose_event@ signal. That is, if you\'re writing an expose event handler,
-- you can assume that the exposed area in 'eventRegion' has already been
-- cleared to the window background, is already set as the clip region, and
-- already has a backing store. Therefore in most cases, application code need
-- not call 'drawWindowBeginPaintRegion'. (You can disable the automatic calls
-- around expose events on a widget-by-widget basis by calling
-- 'widgetSetDoubleBuffered'.)
--
-- If you call this function multiple times before calling the matching
-- 'drawWindowEndPaint', the backing stores are pushed onto a stack.
-- 'drawWindowEndPaint' copies the topmost backing store onscreen, subtracts the
-- topmost region from all other regions in the stack, and pops the stack. All
-- drawing operations affect only the topmost backing store in the stack. One
-- matching call to 'drawWindowEndPaint' is required for each call to
-- 'drawWindowBeginPaintRegion'.
--
-- Removed in Gtk3.
drawWindowBeginPaintRegion :: DrawWindowClass self => self
 -> Region -- ^ @region@ - region you intend to draw to
 -> IO ()
drawWindowBeginPaintRegion self region =
  {# call gdk_window_begin_paint_region #}
     (toDrawWindow self)
     region
#endif

-- | Signal that drawing has finished.
--
-- * Indicates that the backing store created by the most recent call to
-- 'drawWindowBeginPaintRegion' should be copied onscreen and deleted, leaving the
-- next-most-recent backing store or no backing store at all as the active
-- paint region. See 'drawWindowBeginPaintRegion' for full details. It is an error
-- to call this function without a matching 'drawWindowBeginPaintRegion' first.
--
drawWindowEndPaint :: DrawWindowClass self => self -> IO ()
drawWindowEndPaint self =
  {# call gdk_window_end_paint #}
     (toDrawWindow self)

-- | A convenience wrapper around 'drawWindowInvalidateRegion' which invalidates a
-- rectangular region. See 'drawWindowInvalidateRegion' for details.
--
drawWindowInvalidateRect :: DrawWindowClass self => self
 -> Rectangle -- ^ @rect@ - rectangle to invalidate
 -> Bool              -- ^ @invalidateChildren@ - whether to also invalidate
                      -- child drawWindows
 -> IO ()
drawWindowInvalidateRect self rect invalidateChildren =
  with rect $ \rectPtr ->
  {# call gdk_window_invalidate_rect #}
     (toDrawWindow self)
     (castPtr rectPtr)
     (fromBool invalidateChildren)

#if GTK_MAJOR_VERSION < 3
-- | Adds @region@ to the update area for @DrawWindow@. The update area is the
-- region that needs to be redrawn, or \"dirty region.\". During the
-- next idle period of the main look, an expose even for this region
-- will be created. An application would normally redraw
-- the contents of @DrawWindow@ in response to those expose events.
--
-- The @invalidateChildren@ parameter controls whether the region of each
-- child drawWindow that intersects @region@ will also be invalidated. If @False@,
-- then the update area for child drawWindows will remain unaffected.
--
drawWindowInvalidateRegion :: DrawWindowClass self => self
 -> Region -- ^ @region@ - a "Region"
 -> Bool           -- ^ @invalidateChildren@ - @True@ to also invalidate child
                   -- drawWindows
 -> IO ()
drawWindowInvalidateRegion self region invalidateChildren =
  {# call gdk_window_invalidate_region #}
     (toDrawWindow self)
     region
     (fromBool invalidateChildren)
#endif

#if GTK_MAJOR_VERSION < 3
-- | Ask for the dirty region of this window.
--
-- * Transfers ownership of the update area from @DrawWindow@ to the caller of the
-- function. That is, after calling this function, @DrawWindow@ will no longer have
-- an invalid\/dirty region; the update area is removed from @DrawWindow@ and
-- handed to you. If this window has no update area, 'drawWindowGetUpdateArea' returns 'Nothing'.
--
-- Removed in Gtk3.
drawWindowGetUpdateArea :: DrawWindowClass self => self
 -> IO (Maybe Region) -- ^ returns the update area for @DrawWindow@
drawWindowGetUpdateArea self = do
  reg <- {# call gdk_window_get_update_area #} (toDrawWindow self)
  if reg==nullPtr then return Nothing else liftM Just (makeNewRegion reg)
#endif

-- | Temporarily freezes a drawWindow such that it won\'t receive expose events.
--  * The drawWindow will begin receiving expose events again when
--  'drawWindowThawUpdates'
-- is called. If 'drawWindowFreezeUpdates' has been called more than once,
-- 'drawWindowThawUpdates' must be called an equal number of times to begin
-- processing exposes.
--
drawWindowFreezeUpdates :: DrawWindowClass self => self -> IO ()
drawWindowFreezeUpdates self =
  {# call gdk_window_freeze_updates #}
     (toDrawWindow self)

-- | Thaws a drawWindow frozen with 'drawWindowFreezeUpdates'.
--
drawWindowThawUpdates :: DrawWindowClass self => self -> IO ()
drawWindowThawUpdates self =
  {# call gdk_window_thaw_updates #}
     (toDrawWindow self)

-- | Sends one or more expose events to @DrawWindow@.
--
-- * The areas in each expose
-- event will cover the entire update area for the window (see
-- 'drawWindowInvalidateRegion' for details). Normally Gtk calls
-- 'drawWindowProcessUpdates' on your behalf, so there's no need to call this
-- function unless you want to force expose events to be delivered immediately
-- and synchronously (vs. the usual case, where Gtk delivers them in an idle
-- handler). Occasionally this is useful to produce nicer scrolling behavior,
-- for example.
--
drawWindowProcessUpdates :: DrawWindowClass self => self
 -> Bool  -- ^ @updateChildren@ - whether to also process updates for child
          -- drawWindows
 -> IO ()
drawWindowProcessUpdates self updateChildren =
  {# call gdk_window_process_updates #}
     (toDrawWindow self)
     (fromBool updateChildren)

#if GTK_CHECK_VERSION(2,4,0)
-- | Setting @acceptFocus@ to @False@ hints the desktop environment that the
-- window doesn\'t want to receive input focus.
--
-- On X, it is the responsibility of the drawWindow manager to interpret this
-- hint. ICCCM-compliant drawWindow manager usually respect it.
--
-- * Available since Gdk version 2.4
--
drawWindowSetAcceptFocus :: DrawWindowClass self => self
 -> Bool  -- ^ @acceptFocus@ - @True@ if the drawWindow should receive input focus
 -> IO ()
drawWindowSetAcceptFocus self acceptFocus =
  {# call gdk_window_set_accept_focus #}
     (toDrawWindow self)
     (fromBool acceptFocus)
#endif

#if GTK_MAJOR_VERSION < 3
-- | Applies a shape mask to window. Pixels in window corresponding to set
--   bits in the mask will be visible; pixels in window corresponding to
--   unset bits in the mask will be transparent. This gives a non-rectangular
--   window.
--
-- * If @mask@ is @Nothing@, the shape mask will be unset, and the x\/y parameters
--   are not used. The @mask@ must be a bitmap, that is, a 'Pixmap' of depth
--   one.
--
-- * On the X11 platform, this uses an X server extension which is widely
--   available on most common platforms, but not available on very old
--   X servers, and occasionally the implementation will be buggy.
--   On servers without the shape extension, this function will do nothing.
--   On the Win32 platform the functionality is always present.
--
-- * This function works on both toplevel and child windows.
--
drawWindowShapeCombineMask :: DrawWindowClass self => self
 -> Maybe Pixmap -- ^ @mask@ - region of drawWindow to be non-transparent
 -> Int            -- ^ @offsetX@ - X position of @shapeRegion@ in @DrawWindow@
                   -- coordinates
 -> Int            -- ^ @offsetY@ - Y position of @shapeRegion@ in @DrawWindow@
                   -- coordinates
 -> IO ()
drawWindowShapeCombineMask self (Just (Pixmap mask)) offsetX offsetY =
  withForeignPtr mask $ \maskPtr ->
  {# call gdk_window_shape_combine_mask #}
     (toDrawWindow self)
     (castPtr maskPtr)
     (fromIntegral offsetX)
     (fromIntegral offsetY)
drawWindowShapeCombineMask self Nothing offsetX offsetY =
  {# call gdk_window_shape_combine_mask #}
     (toDrawWindow self)
     nullPtr
     (fromIntegral offsetX)
     (fromIntegral offsetY)
#endif

#if GTK_MAJOR_VERSION < 3
-- | Makes pixels in @DrawWindow@ outside @shapeRegion@ transparent.
--
-- * Makes pixels in @DrawWindow@ outside @shapeRegion@ transparent, so that
-- the window may be nonrectangular.
--
-- If @shapeRegion@ is 'Nothing', the shape will be unset, so the whole
-- 'DrawWindow' will be opaque again. The parameters @offsetX@ and @offsetY@
-- are ignored if @shapeRegion@ is 'Nothing'.
--
-- On the X11 platform, this uses an X server extension which is widely
-- available on most common platforms, but not available on very old X servers,
-- and occasionally the implementation will be buggy. On servers without the
-- shape extension, this function will do nothing.
--
-- This function works on both toplevel and child drawWindows.
--
drawWindowShapeCombineRegion :: DrawWindowClass self => self
 -> Maybe Region -- ^ @shapeRegion@ - region of drawWindow to be non-transparent
 -> Int            -- ^ @offsetX@ - X position of @shapeRegion@ in @DrawWindow@
                   -- coordinates
 -> Int            -- ^ @offsetY@ - Y position of @shapeRegion@ in @DrawWindow@
                   -- coordinates
 -> IO ()
drawWindowShapeCombineRegion self (Just reg) offsetX offsetY =
  {# call gdk_window_shape_combine_region #}
     (toDrawWindow self)
     reg
     (fromIntegral offsetX)
     (fromIntegral offsetY)
drawWindowShapeCombineRegion self Nothing offsetX offsetY =
  {# call gdk_window_shape_combine_region #}
     (toDrawWindow self)
     (Region nullForeignPtr)
     (fromIntegral offsetX)
     (fromIntegral offsetY)
#endif

-- | Sets the shape mask of @DrawWindow@ to the union of shape masks for all
-- children of @DrawWindow@, ignoring the shape mask of @DrawWindow@ itself. Contrast
-- with 'drawWindowMergeChildShapes' which includes the shape mask of @DrawWindow@ in
-- the masks to be merged.
--
drawWindowSetChildShapes :: DrawWindowClass self => self -> IO ()
drawWindowSetChildShapes self =
  {# call gdk_window_set_child_shapes #}
     (toDrawWindow self)

-- | Merges the shape masks for any child drawWindows into the shape mask for
-- @DrawWindow@. i.e. the union of all masks for @DrawWindow@ and its children will
-- become the new mask for @DrawWindow@. See 'drawWindowShapeCombineMask'.
--
-- This function is distinct from 'drawWindowSetChildShapes' because it includes
-- @DrawWindow@'s shape mask in the set of shapes to be merged.
--
drawWindowMergeChildShapes :: DrawWindowClass self => self -> IO ()
drawWindowMergeChildShapes self =
  {# call gdk_window_merge_child_shapes #}
     (toDrawWindow self)

-- Superseded by 'drawWindowGetPointerPos', won't be removed.
-- Obtains the current pointer position and modifier state.
--
-- * The position is
-- given in coordinates relative to the given window.
--
-- * The return value is @Just (same, x, y, mod)@ where @same@ is @True@
--   if the passed in window is the window over which the mouse currently
--   resides.
--
-- * The return value is @Nothing@ if the mouse cursor is over a different
--   application.
--
drawWindowGetPointer :: DrawWindowClass self => self
 -> IO (Maybe (Bool, Int, Int, [Modifier]))
drawWindowGetPointer self =
  alloca $ \xPtr -> alloca $ \yPtr -> alloca $ \mPtr -> do
  winPtr <- {# call gdk_window_get_pointer #} (toDrawWindow self)
     xPtr yPtr mPtr
  if winPtr==nullPtr then return Nothing else do
  same <- withForeignPtr (unDrawWindow (toDrawWindow self)) $ \dPtr ->
          return (winPtr==dPtr)
  x <- peek xPtr
  y <- peek yPtr
  m <- peek mPtr
  return (Just (same, fromIntegral x, fromIntegral y,
                toFlags (fromIntegral m)))

-- | Obtains the current pointer position and modifier state.
--
-- * The position is
-- given in coordinates relative to the given window.
--
-- * The return value is @(Just win, x, y, mod)@ where @win@ is the
--   window over which the mouse currently resides and @mod@ denotes
--   the keyboard modifiers currently being depressed.
--
-- * The return value is @Nothing@ for the window if the mouse cursor is
--   not over a known window.
--
drawWindowGetPointerPos :: DrawWindowClass self => self
 -> IO (Maybe DrawWindow, Int, Int, [Modifier])
drawWindowGetPointerPos self =
  alloca $ \xPtr -> alloca $ \yPtr -> alloca $ \mPtr -> do
  winPtr <- {# call gdk_window_get_pointer #} (toDrawWindow self)
     xPtr yPtr mPtr
  x <- peek xPtr
  y <- peek yPtr
  m <- peek mPtr
  mWin <- if winPtr==nullPtr then return Nothing else liftM Just $
    makeNewGObject mkDrawWindow (return winPtr)
  return (mWin, fromIntegral x, fromIntegral y, toFlags (fromIntegral m))


-- | Obtains the position of a window in screen coordinates.
--
-- You can use this to help convert a position between screen coordinates and
-- local 'DrawWindow' relative coordinates.
--
drawWindowGetOrigin :: DrawWindow
 -> IO (Int, Int) -- ^ @(x, y)@
drawWindowGetOrigin self =
  alloca $ \xPtr ->
  alloca $ \yPtr -> do
  {# call gdk_window_get_origin #}
    (toDrawWindow self)
    xPtr
    yPtr
  x <- peek xPtr
  y <- peek yPtr
  return (fromIntegral x, fromIntegral y)

-- | Sets the mouse pointer for a 'DrawWindow'.
--
-- Use 'cursorNewForDisplay' or 'cursorNewFromPixmap' to create the cursor.
-- To make the cursor invisible, use 'BlankCursor'. Passing @Nothing@ means
-- that the @DrawWindow@ will use the cursor of its parent @DrawWindow@.
-- Most @DrawWindow@ should use this default.
--
drawWindowSetCursor :: DrawWindow -> Maybe Cursor -> IO ()
drawWindowSetCursor self cursor =
  {# call gdk_window_set_cursor #}
    self
    (fromMaybe (Cursor nullForeignPtr) cursor)

#if GTK_MAJOR_VERSION < 3 && !defined(HAVE_QUARTZ_GTK) && !defined(GDK_WINDOWING_QUARTZ)
-- | Get the handle to an exising window of the windowing system. The
-- passed-in handle is a reference to a native window, that is, an Xlib XID
-- for X windows and a HWND for Win32.
--
-- Removed in Gtk3.
drawWindowForeignNew :: NativeWindowId -> IO (Maybe DrawWindow)
drawWindowForeignNew anid = maybeNull (wrapNewGObject mkDrawWindow) $
  liftM castPtr $ {#call gdk_window_foreign_new#} (fromNativeWindowId anid)
#endif

-- | Obtains the root window (parent all other windows are inside) for the default display and screen.
drawWindowGetDefaultRootWindow ::
  IO DrawWindow -- ^ returns the default root window
drawWindowGetDefaultRootWindow =
  makeNewGObject mkDrawWindow $
  {#call gdk_get_default_root_window #}

#if GTK_CHECK_VERSION(2,24,0)
-- | Returns the width of the window.
--
-- On the X11 platform the returned size is the size reported in the
-- most-recently-processed configure event, rather than the current
-- size on the X server.
--
drawWindowGetWidth :: DrawWindow -> IO Int
drawWindowGetWidth self =
  liftM fromIntegral $ {# call gdk_window_get_width #} (toDrawWindow self)

-- | Returns the height of the window.
--
-- On the X11 platform the returned size is the size reported in the
-- most-recently-processed configure event, rather than the current
-- size on the X server.
--
drawWindowGetHeight :: DrawWindow -> IO Int
drawWindowGetHeight self =
  liftM fromIntegral $ {# call gdk_window_get_height #} (toDrawWindow self)
#endif
