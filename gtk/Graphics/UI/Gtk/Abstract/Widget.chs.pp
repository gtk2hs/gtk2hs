-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Widget
--
--  Author : Axel Simon
--
--  Created: 27 April 2001
--
--  Version $Revision: 1.2 $ from $Date: 2005/04/07 00:13:59 $
--
--  Copyright (C) 2001-2005 Axel Simon
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
-- unimplemented methods that seem to be useful in user programs:
--      widgetSizeRequest, widgetAddAccelerator, widgetRemoveAccelerator,
--	widgetAcceleratorSignal, widgetGrabDefault,
--	widgetPango*, widgetSetAdjustments
--
-- implement the following methods in GtkWindow object:
--      widget_set_uposition, widget_set_usize
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Base class for all widgets
--
module Graphics.UI.Gtk.Abstract.Widget (
-- * Detail
-- 
-- | 'Widget' introduces style properties - these are basically object
-- properties that are stored not on the object, but in the style object
-- associated to the widget. Style properties are set in resource files. This
-- mechanism is used for configuring such things as the location of the
-- scrollbar arrows through the theme, giving theme authors more control over
-- the look of applications without the need to write a theme engine in C.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----Widget
-- |               +----'Container'
-- |               +----'Misc'
-- |               +----'Calendar'
-- |               +----'CellView'
-- |               +----'DrawingArea'
-- |               +----'Entry'
-- |               +----'Ruler'
-- |               +----'Range'
-- |               +----'Separator'
-- |               +----'Invisible'
-- |               +----'OldEditable'
-- |               +----'Preview'
-- |               +----'Progress'
-- @

-- * Types
  Widget,
  WidgetClass,
  castToWidget,
  Allocation,
  Requisition(..),
  Rectangle(..),

-- * Methods
  widgetGetState,
  widgetGetSavedState,
  widgetShow,			-- Showing and hiding a widget.
  widgetShowNow,
  widgetHide,
  widgetShowAll,
  widgetHideAll,
  widgetDestroy,
  widgetCreateLayout,		-- Drawing text.
  widgetQueueDraw,		-- Functions to be used with DrawingArea.
  widgetHasIntersection,
  widgetIntersect,
  widgetRegionIntersect,
  widgetActivate,		-- Manipulate widget state.
  widgetSetSensitivity,
  widgetSetSizeRequest,
  widgetIsFocus,
  widgetGrabFocus,
  widgetSetAppPaintable,
  widgetSetName,		-- Naming, Themes
  widgetGetName,
  widgetGetToplevel,		-- Widget browsing.
  widgetIsAncestor,
  widgetReparent,
  TextDirection(..),
  widgetSetDirection,		-- General Setup.
  widgetGetDirection,
  widgetQueueDrawArea,
  widgetSetDoubleBuffered,
  widgetSetRedrawOnAllocate,
  widgetGetPointer,
  widgetPath,
  widgetClassPath,
  widgetGetCompositeName,
  widgetSetCompositeName,
  widgetModifyStyle,
  widgetGetModifierStyle,
  widgetModifyFg,
  widgetModifyBg,
  widgetModifyText,
  widgetModifyBase,
  widgetModifyFont,
  widgetGetParentWindow,
  widgetSetExtensionEvents,
  widgetGetExtensionEvents,
  widgetGetEvents,
  widgetTranslateCoordinates,
  widgetSetDefaultDirection,
  widgetGetDefaultDirection,
  widgetCreatePangoContext,
  widgetGetPangoContext,
  widgetRenderIcon,
  widgetGetParent,
  widgetGetSizeRequest,

-- * Properties
  widgetExtensionEvents,
  widgetDirection,

-- * Signals
  Event(..),
  onButtonPress,
  afterButtonPress,
  onButtonRelease,
  afterButtonRelease,
  onClient,
  afterClient,
  onConfigure,
  afterConfigure,
  onDelete,
  afterDelete,
  onDestroyEvent,		-- you probably want onDestroy
  afterDestroyEvent,
  onDirectionChanged,
  afterDirectionChanged,
  onEnterNotify,
  afterEnterNotify,
  onLeaveNotify,
  afterLeaveNotify,
  onExpose,
  afterExpose,
  onFocusIn,
  afterFocusIn,
  onFocusOut,
  afterFocusOut,
  onGrabFocus,
  afterGrabFocus,
  onDestroy,
  afterDestroy,
  onHide,
  afterHide,
  onHierarchyChanged,
  afterHierarchyChanged,
  onKeyPress,
  afterKeyPress,
  onKeyRelease,
  afterKeyRelease,
  onMnemonicActivate,
  afterMnemonicActivate,
  onMotionNotify,
  afterMotionNotify,
  onParentSet,
  afterParentSet,
  onPopupMenu,
  afterPopupMenu,
  onProximityIn,
  afterProximityIn,
  onProximityOut,
  afterProximityOut,
  onRealize,
  afterRealize,
  onScroll,
  afterScroll,
  onShow,
  afterShow,
  onSizeAllocate,
  afterSizeAllocate,
  onSizeRequest,
  afterSizeRequest,
  StateType(..),
  onStateChanged,
  afterStateChanged,
  onUnmap,
  afterUnmap,
  onUnrealize,
  afterUnrealize,
  onVisibilityNotify,
  afterVisibilityNotify,
  onWindowState,
  afterWindowState
  ) where

import Monad	(liftM, unless)
import Maybe	(fromMaybe)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes		(Attr(..))
import System.Glib.GObject		(makeNewGObject)
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.Gdk.Enums
{#import Graphics.UI.Gtk.Gdk.Region#}	(Region(..), makeNewRegion)
import Graphics.UI.Gtk.General.Structs	(Allocation, Rectangle(..)
					,Requisition(..), Color, IconSize,
					,widgetGetState, widgetGetSavedState)
import Graphics.UI.Gtk.Gdk.Events	(Event(..), marshalEvent)
import Graphics.UI.Gtk.General.Enums	(StateType(..), TextDirection(..))
{#import Graphics.UI.Gtk.Pango.Types#}	(FontDescription(FontDescription))

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Methods

-- | Flags a widget to be displayed. Any widget that isn't shown will not
-- appear on the screen. If you want to show all the widgets in a container,
-- it's easier to call 'widgetShowAll' on the container, instead of
-- individually showing the widgets.
--
-- Remember that you have to show the containers containing a widget, in
-- addition to the widget itself, before it will appear onscreen.
--
-- When a toplevel container is shown, it is immediately realized and
-- mapped; other shown widgets are realized and mapped when their toplevel
-- container is realized and mapped.
--
widgetShow :: WidgetClass self => self -> IO ()
widgetShow self =
  {# call widget_show #}
    (toWidget self)

-- | Shows a widget. If the widget is an unmapped toplevel widget (i.e. a
-- 'Window' that has not yet been shown), enter the main loop and wait for the
-- window to actually be mapped. Be careful; because the main loop is running,
-- anything can happen during this function.
--
widgetShowNow :: WidgetClass self => self -> IO ()
widgetShowNow self =
  {# call widget_show_now #}
    (toWidget self)

-- | Reverses the effects of 'widgetShow', causing the widget to be hidden
-- (invisible to the user).
--
widgetHide :: WidgetClass self => self -> IO ()
widgetHide self =
  {# call widget_hide #}
    (toWidget self)

-- | Recursively shows a widget, and any child widgets (if the widget is a
-- container).
--
widgetShowAll :: WidgetClass self => self -> IO ()
widgetShowAll self =
  {# call widget_show_all #}
    (toWidget self)

-- | Recursively hides a widget and any child widgets.
--
widgetHideAll :: WidgetClass self => self -> IO ()
widgetHideAll self =
  {# call widget_hide_all #}
    (toWidget self)

-- | Destroys a widget. Equivalent to 'objectDestroy'.
--
-- When a widget is destroyed it will be removed from the screen and
-- unrealized. When a widget is destroyed, it will break any references it
-- holds to other objects.If the widget is inside a container, the widget will
-- be removed from the container. The widget will be garbage collected
-- (finalized) time after your last reference to the widget dissapears.
--
-- In most cases, only toplevel widgets (windows) require explicit
-- destruction, because when you destroy a toplevel its children will be
-- destroyed as well.
--
widgetDestroy :: WidgetClass self => self -> IO ()
widgetDestroy self =
  {# call widget_destroy #}
    (toWidget self)

-- Functions to be used with DrawingArea.

-- | Prepare text for display.
--
-- The 'PangoLayout' represents the rendered text. It can be shown on screen
-- by calling 'drawLayout'.
--
-- The returned 'Layout' shares the same font information ('Context') as this
-- widget. If this information changes, the 'Layout' should change. The
-- following code ensures that the displayed text always reflects the widget's
-- settings:
--
-- > l <- widgetCreateLayout w "My Text."
-- > let update = do
-- >                layoutContextChanged l
-- > 		    -- update the Drawables which show this layout
-- > w `onDirectionChanged` update
-- > w `onStyleChanged` update
--
widgetCreateLayout :: WidgetClass self => self
 -> String    -- ^ @text@ - text to set on the layout
 -> IO PangoLayout
widgetCreateLayout self text =
  makeNewGObject mkPangoLayout $
  withUTFString text $ \textPtr ->
  {# call unsafe widget_create_pango_layout #}
    (toWidget self)
    textPtr

-- | Send a redraw request to a widget. Equivalent to calling
-- 'widgetQueueDrawArea' for the entire area of a widget.
--
widgetQueueDraw :: WidgetClass self => self -> IO ()
widgetQueueDraw self =
  {# call widget_queue_draw #}
    (toWidget self)

-- | Check if the widget intersects with a given area.
--
widgetHasIntersection :: WidgetClass self => self
 -> Rectangle -- ^ @area@ - a rectangle
 -> IO Bool   -- ^ returns @True@ if there was an intersection
widgetHasIntersection self area = 
  liftM toBool $
  with area $ \areaPtr ->
  {# call unsafe widget_intersect #}
    (toWidget self)
    (castPtr areaPtr)
    (castPtr nullPtr)

-- | Computes the intersection of a widget's area and @area@, returning the
-- intersection, and returns @Nothing@ if there was no intersection.
--
widgetIntersect :: WidgetClass self => self
 -> Rectangle -- ^ @area@ - a rectangle
 -> IO (Maybe Rectangle) -- ^ returns the intersection or @Nothing@
widgetIntersect self area =
  with area $ \areaPtr ->
  alloca $ \intersectionPtr -> do
  hasIntersection <- {# call unsafe widget_intersect #}
    (toWidget self)
    (castPtr areaPtr)
    (castPtr intersectionPtr)
  if (toBool hasIntersection)
    then liftM Just $ peek intersectionPtr
    else return Nothing

-- | Computes the intersection of a widget's area and @region@, returning
-- the intersection. The result may be empty, use 'regionEmpty' to check.
--
widgetRegionIntersect :: WidgetClass self => self
 -> Region    -- ^ @region@ - a 'Region' in the same coordinate system as the
              -- widget's allocation. That is, relative to the widget's
              -- 'DrawWindow' for 'NoWindow' widgets; relative to the parent
              -- 'DrawWindow' of the widget's 'DrawWindow' for widgets with
              -- their own 'DrawWindow'.
 -> IO Region -- ^ returns A region holding the intersection of the widget and
              --  @region@. The coordinates of the return value are relative to
              -- the widget's 'DrawWindow' for 'NoWindow' widgets, and relative
              --  to the parent 'DrawWindow' of the widget's 'DrawWindow' for
              -- widgets with their own 'DrawWindow'.
widgetRegionIntersect self region = do
  intersectionPtr <- {# call gtk_widget_region_intersect #}
    (toWidget self)
    region
  makeNewRegion intersectionPtr

-- Manipulate widget state.

-- | For widgets that can be \"activated\" (buttons, menu items, etc.) this
-- function activates them. Activation is what happens when you press Enter on
-- a widget during key navigation. If @widget@ isn't activatable, the function
-- returns @False@.
--
widgetActivate :: WidgetClass self => self
 -> IO Bool -- ^ returns @True@ if the widget was activatable
widgetActivate self =
  liftM toBool $
  {# call widget_activate #}
    (toWidget self)

-- | Sets the sensitivity of a widget. A widget is sensitive if the user can
-- interact with it. Insensitive widgets are \"grayed out\" and the user can't
-- interact with them. Insensitive widgets are known as \"inactive\",
-- \"disabled\", or \"ghosted\" in some other toolkits.
--
widgetSetSensitivity :: WidgetClass self => self
 -> Bool  -- ^ @sensitive@ - @True@ to make the widget sensitive
 -> IO ()
widgetSetSensitivity self sensitive =
  {# call widget_set_sensitive #}
    (toWidget self)
    (fromBool sensitive)

-- | Sets the minimum size of a widget; that is, the widget's size request
-- will be @width@ by @height@. You can use this function to force a widget to
-- be either larger or smaller than it normally would be.
--
-- In most cases, 'windowSetDefaultSize' is a better choice for toplevel
-- windows than this function; setting the default size will still allow users
-- to shrink the window. Setting the size request will force them to leave the
-- window at least as large as the size request. When dealing with window
-- sizes, 'windowSetGeometryHints' can be a useful function as well.
--
-- Note the inherent danger of setting any fixed size - themes, translations
-- into other languages, different fonts, and user action can all change the
-- appropriate size for a given widget. So, it's basically impossible to
-- hardcode a size that will always be correct.
--
-- The size request of a widget is the smallest size a widget can accept
-- while still functioning well and drawing itself correctly. However in some
-- strange cases a widget may be allocated less than its requested size, and in
-- many cases a widget may be allocated more space than it requested.
--
-- If the size request in a given direction is -1 (unset), then the
-- \"natural\" size request of the widget will be used instead.
--
-- Widgets can't actually be allocated a size less than 1 by 1, but you can
-- pass 0,0 to this function to mean \"as small as possible.\"
--
widgetSetSizeRequest :: WidgetClass self => self
 -> Int   -- ^ @width@ - width @widget@ should request, or -1 to unset
 -> Int   -- ^ @height@ - height @widget@ should request, or -1 to unset
 -> IO ()
widgetSetSizeRequest self width height =
  {# call widget_set_size_request #}
    (toWidget self)
    (fromIntegral width)
    (fromIntegral height)

-- | Gets the size request that was explicitly set for the widget using
-- 'widgetSetSizeRequest'. A value of -1 for @width@ or @height@
-- indicates that that dimension has not been set explicitly and the natural
-- requisition of the widget will be used intead. See 'widgetSetSizeRequest'.
-- To get the size a widget will actually use, call 'widgetSizeRequest' instead
-- of this function.
--
widgetGetSizeRequest :: WidgetClass self => self
 -> IO (Int, Int) -- ^ @(width, height)@
widgetGetSizeRequest self =
  alloca $ \widthPtr ->
  alloca $ \heightPtr -> do
  {# call gtk_widget_get_size_request #}
    (toWidget self)
    widthPtr
    heightPtr
  width <- peek widthPtr
  height <- peek heightPtr
  return (fromIntegral width, fromIntegral height)

-- | Determines if the widget is the focus widget within its toplevel.
--
widgetIsFocus :: WidgetClass self => self
 -> IO Bool -- ^ returns @True@ if the widget is the focus widget.
widgetIsFocus self =
  liftM toBool $
  {# call unsafe widget_is_focus #}
    (toWidget self)

-- | Causes the widget to have the keyboard focus for the 'Window' it's inside.
-- The widget must be a focusable widget, such as a 'Entry'; something like
-- 'Frame' won't work. (More precisely, it must have the 'CanFocus' flag set.)
--
widgetGrabFocus :: WidgetClass self => self -> IO ()
widgetGrabFocus self =
  {# call widget_grab_focus #}
    (toWidget self)

-- | Sets some weired flag in the widget.
--
widgetSetAppPaintable :: WidgetClass self => self
 -> Bool  -- ^ @appPaintable@ -
 -> IO ()
widgetSetAppPaintable self appPaintable =
  {# call widget_set_app_paintable #}
    (toWidget self)
    (fromBool appPaintable)

-- | Widgets can be named, which allows you to refer to them from a gtkrc
-- file. You can apply a style to widgets with a particular name in the gtkrc
-- file. See the documentation for gtkrc files.
--
-- Note that widget names are separated by periods in paths (see
-- 'widgetPath'), so names with embedded periods may cause confusion.
--
widgetSetName :: WidgetClass self => self
 -> String -- ^ @name@ - name for the widget
 -> IO ()
widgetSetName self name =
  withUTFString name $ \namePtr ->
  {# call widget_set_name #}
    (toWidget self)
    namePtr

-- | Retrieves the name of a widget. See 'widgetSetName' for the significance
-- of widget names.
--
widgetGetName :: WidgetClass self => self -> IO String
widgetGetName self =
  {# call unsafe widget_get_name #}
    (toWidget self)
  >>= peekUTFString

-- | Enable event signals.
--
widgetAddEvents :: WidgetClass self => self -> [EventMask] -> IO ()
widgetAddEvents self events =
  {# call widget_add_events #}
    (toWidget self)
    (fromIntegral $ fromFlags events)

-- | Get enabled event signals. These are the events that the widget will
-- receive.
--
widgetGetEvents :: WidgetClass self => self -> IO [EventMask]
widgetGetEvents self =
  liftM (toFlags . fromIntegral) $
  {# call unsafe widget_get_events #}
    (toWidget self)

-- | Sets the extension events.
--
widgetSetExtensionEvents :: WidgetClass self => self
 -> [ExtensionMode]
 -> IO ()
widgetSetExtensionEvents self mode =
  {# call widget_set_extension_events #}
    (toWidget self)
    ((fromIntegral . fromFlags) mode)

-- | Retrieves the extension events the widget will receive; see
-- 'inputSetExtensionEvents'.
--
widgetGetExtensionEvents :: WidgetClass self => self
 -> IO [ExtensionMode]
widgetGetExtensionEvents self =
  liftM (toFlags . fromIntegral) $
  {# call widget_get_extension_events #}
    (toWidget self)

-- Widget browsing.

-- | This function returns the topmost widget in the container hierarchy
-- @widget@ is a part of. If @widget@ has no parent widgets, it will be
-- returned as the topmost widget.
--
widgetGetToplevel :: WidgetClass self => 
    self      -- ^ @widget@ - the widget in question
 -> IO Widget -- ^ returns the topmost ancestor of @widget@, or @widget@
              -- itself if there's no ancestor.
widgetGetToplevel self =
  makeNewObject mkWidget $
  {# call unsafe widget_get_toplevel #}
    (toWidget self)

-- | Determines whether @widget@ is somewhere inside @ancestor@, possibly with
-- intermediate containers.
--
widgetIsAncestor :: (WidgetClass self, WidgetClass ancestor) =>
    self     -- ^ @widget@ - the widget in question
 -> ancestor -- ^ @ancestor@ - another 'Widget'
 -> IO Bool  -- ^ returns @True@ if @ancestor@ contains @widget@ as a child,
             -- grandchild, great grandchild, etc.
widgetIsAncestor self ancestor =
  liftM toBool $
  {# call unsafe widget_is_ancestor #}
    (toWidget self)
    (toWidget ancestor)

-- | Moves a widget from one 'Container' to another.
--
widgetReparent :: (WidgetClass self, WidgetClass newParent) => self
 -> newParent -- ^ @newParent@ - a 'Container' to move the widget into
 -> IO ()
widgetReparent self newParent =
  {# call widget_reparent #}
    (toWidget self)
    (toWidget newParent)

-- | Sets the reading direction on a particular widget. This direction
-- controls the primary direction for widgets containing text, and also the
-- direction in which the children of a container are packed. The ability to
-- set the direction is present in order so that correct localization into
-- languages with right-to-left reading directions can be done. Generally,
-- applications will let the default reading direction present, except for
-- containers where the containers are arranged in an order that is explicitely
-- visual rather than logical (such as buttons for text justification).
--
-- If the direction is set to 'TextDirNone', then the value set by
-- 'widgetSetDefaultDirection' will be used.
--
widgetSetDirection :: WidgetClass self => self -> TextDirection -> IO ()
widgetSetDirection self dir =
  {# call widget_set_direction #}
    (toWidget self)
    ((fromIntegral . fromEnum) dir)

-- | Gets the reading direction for a particular widget. See
-- 'widgetSetDirection'.
--
widgetGetDirection :: WidgetClass self => self -> IO TextDirection
widgetGetDirection self =
  liftM (toEnum . fromIntegral) $
  {# call widget_get_direction #}
    (toWidget self)

-- | Invalidates the rectangular area of @widget@ defined by @x@, @y@, @width@
-- and @height@ by calling 'windowInvalidateRect' on the widget's window and
-- all its child windows. Once the main loop becomes idle (after the current
-- batch of events has been processed, roughly), the window will receive expose
-- events for the union of all regions that have been invalidated.
--
-- Normally you would only use this function in widget implementations. You
-- might also use it, or 'windowInvalidateRect' directly, to schedule a redraw
-- of a 'DrawingArea' or some portion thereof.
--
-- Frequently you can just call 'windowInvalidateRect' or
-- 'windowInvalidateRegion' instead of this function. Those functions will
-- invalidate only a single window, instead of the widget and all its children.
--
-- The advantage of adding to the invalidated region compared to simply
-- drawing immediately is efficiency; using an invalid region ensures that you
-- only have to redraw one time.
--
widgetQueueDrawArea :: WidgetClass self => self
 -> Int   -- ^ @x@ - x coordinate of upper-left corner of rectangle to redraw
 -> Int   -- ^ @y@ - y coordinate of upper-left corner of rectangle to redraw
 -> Int   -- ^ @width@ - width of region to draw
 -> Int   -- ^ @height@ - height of region to draw
 -> IO ()
widgetQueueDrawArea self x y width height =
  {# call gtk_widget_queue_draw_area #}
    (toWidget self)
    (fromIntegral x)
    (fromIntegral y)
    (fromIntegral width)
    (fromIntegral height)

-- | Widgets are double buffered by default; you can use this function to turn
-- off the buffering. \"Double buffered\" simply means that
-- 'windowBeginPaintRegion' and 'windowEndPaint' are called automatically
-- around expose events sent to the widget. 'windowBeginPaint' diverts all
-- drawing to a widget's window to an offscreen buffer, and 'windowEndPaint'
-- draws the buffer to the screen. The result is that users see the window
-- update in one smooth step, and don't see individual graphics primitives
-- being rendered.
--
-- In very simple terms, double buffered widgets don't flicker, so you would
-- only use this function to turn off double buffering if you had special needs
-- and really knew what you were doing.
--
widgetSetDoubleBuffered :: WidgetClass self => self
 -> Bool  -- ^ @doubleBuffered@ - @True@ to double-buffer a widget
 -> IO ()
widgetSetDoubleBuffered self doubleBuffered =
  {# call gtk_widget_set_double_buffered #}
    (toWidget self)
    (fromBool doubleBuffered)

-- | Sets whether when a widgets size allocation changes, the entire widget
-- is queued for drawing. By default, this setting is @True@ and the entire
-- widget is redrawn on every size change. If your widget leaves the upper left
-- unchanged when made bigger, turning this setting on will improve
-- performance.
--
-- Note that for \"no window\" widgets setting this flag to @False@ turns off
-- all allocation on resizing: the widget will not even redraw if its position
-- changes; this is to allow containers that don't draw anything to avoid
-- excess invalidations. If you set this flag on a \"no window\" widget that
-- /does/ draw its window, you are responsible for invalidating both
-- the old and new allocation of the widget when the widget is moved and
-- responsible for invalidating regions newly when the widget increases size.
--
widgetSetRedrawOnAllocate :: WidgetClass self => self
 -> Bool  -- ^ @redrawOnAllocate@ - if @True@, the entire widget will be
          -- redrawn when it is allocated to a new size. Otherwise, only the
          -- new portion of the widget will be redrawn.
 -> IO ()
widgetSetRedrawOnAllocate self redrawOnAllocate =
  {# call gtk_widget_set_redraw_on_allocate #}
    (toWidget self)
    (fromBool redrawOnAllocate)

-- | Gets the widget's parent window.
--
widgetGetParentWindow :: WidgetClass self => self -> IO DrawWindow
widgetGetParentWindow self =
  makeNewGObject mkDrawWindow $
  {# call gtk_widget_get_parent_window #}
    (toWidget self)

-- | Obtains the location of the mouse pointer in widget coordinates. Widget
-- coordinates are a bit odd; for historical reasons, they are defined as
-- 'widgetGetParentWindow' coordinates for widgets that are not 'NoWindow' widgets,
-- and are relative to the widget's allocation's (x,y) for
-- widgets that are 'NoWindow' widgets.
--
widgetGetPointer :: WidgetClass self => self
 -> IO (Int, Int) -- ^ @(x, y)@ - X Y coordinate
widgetGetPointer self =
  alloca $ \xPtr ->
  alloca $ \yPtr ->
  {# call gtk_widget_get_pointer #}
    (toWidget self)
    xPtr
    yPtr
  >>
  peek xPtr >>= \x ->
  peek yPtr >>= \y ->
  return (fromIntegral x, fromIntegral y)

-- | Translate coordinates relative to @srcWidget@'s allocation to coordinates
-- relative to @destWidget@'s allocations. In order to perform this operation,
-- both widgets must be realized, and must share a common toplevel.
--
widgetTranslateCoordinates :: (WidgetClass self, WidgetClass destWidget) =>
    self                -- ^ @srcWidget@ - a 'Widget'
 -> destWidget          -- ^ @destWidget@ - a 'Widget'
 -> Int                 -- ^ @srcX@ - X position relative to @srcWidget@
 -> Int                 -- ^ @srcY@ - Y position relative to @srcWidget@
 -> IO (Maybe (Int, Int)) -- ^ @Just (destX, destY)@ - X and Y position
                        -- relative to @destWidget@. Returns @Nothing@ if
                        -- either widget was not realized, or there was no
                        -- common ancestor.
widgetTranslateCoordinates self destWidget srcX srcY =
  alloca $ \destXPtr ->
  alloca $ \destYPtr -> do
  worked <- {# call gtk_widget_translate_coordinates #}
    (toWidget self)
    (toWidget destWidget)
    (fromIntegral srcX)
    (fromIntegral srcY)
    destXPtr
    destYPtr
  if (toBool worked)
    then do destX <- peek destXPtr
            destY <- peek destYPtr
            return (Just (fromIntegral destX, fromIntegral destY))
    else return Nothing

-- | Obtains the full path to the widget. The path is simply the name of a
-- widget and all its parents in the container hierarchy, separated by periods.
-- The name of a widget comes from 'widgetGetName'. Paths are used to apply
-- styles to a widget in gtkrc configuration files. Widget names are the type
-- of the widget by default (e.g. \"GtkButton\") or can be set to an
-- application-specific value with 'widgetSetName'. By setting the name of a
-- widget, you allow users or theme authors to apply styles to that specific
-- widget in their gtkrc file. Also returns the path in reverse
-- order, i.e. starting with the widget's name instead of starting with the
-- name of the widget's outermost ancestor.
--
widgetPath :: WidgetClass self => self
 -> IO (Int, String, String) -- ^ @(pathLength, path, pathReversed)@ - length
                             -- of the path, path string and reverse path
                             -- string
widgetPath self =
  alloca $ \pathLengthPtr ->
  alloca $ \pathPtr ->
  alloca $ \pathReversedPtr ->
  {# call gtk_widget_path #}
    (toWidget self)
    pathLengthPtr
    pathPtr
    pathReversedPtr
  >>
  peek pathLengthPtr >>= \pathLength ->
  peek pathPtr >>= readUTFString >>= \path ->
  peek pathReversedPtr >>= readUTFString >>= \pathReversed ->
  return (fromIntegral pathLength, path, pathReversed)

-- | Same as 'widgetPath', but always uses the name of a widget's type, never
-- uses a custom name set with 'widgetSetName'.
--
widgetClassPath :: WidgetClass self => self
 -> IO (Int, String, String) -- ^ @(pathLength, path, pathReversed)@ - length
                             -- of the path, path string and reverse path
                             -- string
widgetClassPath self =
  alloca $ \pathLengthPtr ->
  alloca $ \pathPtr ->
  alloca $ \pathReversedPtr ->
  {# call gtk_widget_class_path #}
    (toWidget self)
    pathLengthPtr
    pathPtr
    pathReversedPtr
  >>
  peek pathLengthPtr >>= \pathLength ->
  peek pathPtr >>= readUTFString >>= \path ->
  peek pathReversedPtr >>= readUTFString >>= \pathReversed ->
  return (fromIntegral pathLength, path, pathReversed)

-- | Obtains the composite name of a widget.
--
widgetGetCompositeName :: WidgetClass self => self
 -> IO (Maybe String) -- ^ returns the composite name of @widget@, or
                      -- @Nothing@ if @widget@ is not a composite child.
widgetGetCompositeName self =
  {# call gtk_widget_get_composite_name #}
    (toWidget self)
  >>= maybePeek peekUTFString

-- | Sets a widgets composite name. The widget must be a composite child of
-- its parent; see 'widgetPushCompositeChild'.
--
widgetSetCompositeName :: WidgetClass self => self
 -> String -- ^ @name@ - the name to set.
 -> IO ()
widgetSetCompositeName self name =
  withUTFString name $ \namePtr ->
  {# call gtk_widget_set_composite_name #}
    (toWidget self)
    namePtr

-- | Returns the parent container of @widget@.
--
widgetGetParent :: WidgetClass self => self
 -> IO Widget -- ^ returns the parent container of @widget@, or {@NULL@,
              -- FIXME: this should probably be converted to a Maybe data type}
widgetGetParent self =
  makeNewObject mkWidget $
  {# call gtk_widget_get_parent #}
    (toWidget self)

-- | Sets the default reading direction for widgets where the direction has
-- not been explicitly set by 'widgetSetDirection'.
--
widgetSetDefaultDirection :: 
    TextDirection -- ^ @dir@ - the new default direction. This cannot be
                  -- 'TextDirNone'.
 -> IO ()
widgetSetDefaultDirection dir =
  {# call gtk_widget_set_default_direction #}
    ((fromIntegral . fromEnum) dir)

-- | Obtains the current default reading direction. See
-- 'widgetSetDefaultDirection'.
--
widgetGetDefaultDirection :: IO TextDirection
widgetGetDefaultDirection =
  liftM (toEnum . fromIntegral) $
  {# call gtk_widget_get_default_direction #}

-- | Modifies style values on the widget. Modifications made using this
-- technique take precedence over style values set via an RC file, however,
-- they will be overriden if a style is explicitely set on the widget using
-- 'widgetSetStyle'. The 'RcStyle' structure is designed so each field can
-- either be set or unset, so it is possible, using this function, to modify
-- some style values and leave the others unchanged.
--
-- Note that modifications made with this function are not cumulative with
-- previous calls to 'widgetModifyStyle' or with such functions as
-- 'widgetModifyFg'. If you wish to retain previous values, you must first call
-- 'widgetGetModifierStyle', make your modifications to the returned style,
-- then call 'widgetModifyStyle' with that style. On the other hand, if you
-- first call 'widgetModifyStyle', subsequent calls to such functions
-- 'widgetModifyFg' will have a cumulative effect with the initial
-- modifications.
--
widgetModifyStyle :: (WidgetClass self, RcStyleClass style) => self
 -> style -- ^ @style@ - the 'RcStyle' holding the style modifications
 -> IO ()
widgetModifyStyle self style =
  {# call gtk_widget_modify_style #}
    (toWidget self)
    (toRcStyle style)

-- | Returns the current modifier style for the widget. (As set by
-- 'widgetModifyStyle'.) If no style has previously set, a new 'RcStyle' will
-- be created with all values unset, and set as the modifier style for the
-- widget. If you make changes to this rc style, you must call
-- 'widgetModifyStyle', passing in the returned rc style, to make sure that
-- your changes take effect.
--
-- Caution: passing the style back to 'widgetModifyStyle' will normally end
-- up destroying it, because 'widgetModifyStyle' copies the passed-in style and
-- sets the copy as the new modifier style, thus dropping any reference to the
-- old modifier style. Add a reference to the modifier style if you want to
-- keep it alive.
--
widgetGetModifierStyle :: WidgetClass self => self -> IO RcStyle
widgetGetModifierStyle self =
  makeNewGObject mkRcStyle $
  {# call gtk_widget_get_modifier_style #}
    (toWidget self)

-- | Sets the foreground color for a widget in a particular state. All other
-- style values are left untouched. See also 'widgetModifyStyle'.
--
widgetModifyFg :: WidgetClass self => self
 -> StateType -- ^ @state@ - the state for which to set the foreground color.
 -> Color     -- ^ @color@ - the color to assign (does not need to be
              -- allocated), or @Nothing@ to undo the effect of previous calls
              -- to of 'widgetModifyFg'.
 -> IO ()
widgetModifyFg self state color =
  with color $ \colorPtr ->
  {# call gtk_widget_modify_fg #}
    (toWidget self)
    ((fromIntegral . fromEnum) state)
    (castPtr colorPtr)

-- | Sets the background color for a widget in a particular state. All other
-- style values are left untouched. See also 'widgetModifyStyle'.
--
widgetModifyBg :: WidgetClass self => self
 -> StateType -- ^ @state@ - the state for which to set the background color.
 -> Color     -- ^ @color@ - the color to assign (does not need to be
              -- allocated), or @Nothing@ to undo the effect of previous calls
              -- to of 'widgetModifyBg'.
 -> IO ()
widgetModifyBg self state color =
  with color $ \colorPtr ->
  {# call gtk_widget_modify_bg #}
    (toWidget self)
    ((fromIntegral . fromEnum) state)
    (castPtr colorPtr)

-- | Sets the text color for a widget in a particular state. All other style
-- values are left untouched. The text color is the foreground color used along
-- with the base color (see 'widgetModifyBase') for widgets such as 'Entry' and
-- 'TextView'. See also 'widgetModifyStyle'.
--
widgetModifyText :: WidgetClass self => self
 -> StateType -- ^ @state@ - the state for which to set the text color.
 -> Color     -- ^ @color@ - the color to assign (does not need to be
              -- allocated), or @Nothing@ to undo the effect of previous calls
              -- to of 'widgetModifyText'.
 -> IO ()
widgetModifyText self state color =
  with color $ \colorPtr ->
  {# call gtk_widget_modify_text #}
    (toWidget self)
    ((fromIntegral . fromEnum) state)
    (castPtr colorPtr)

-- | Sets the base color for a widget in a particular state. All other style
-- values are left untouched. The base color is the background color used along
-- with the text color (see 'widgetModifyText') for widgets such as 'Entry' and
-- 'TextView'. See also 'widgetModifyStyle'.
--
widgetModifyBase :: WidgetClass self => self
 -> StateType -- ^ @state@ - the state for which to set the base color.
 -> Color     -- ^ @color@ - the color to assign (does not need to be
              -- allocated), or @Nothing@ to undo the effect of previous calls
              -- to of 'widgetModifyBase'.
 -> IO ()
widgetModifyBase self state color =
  with color $ \colorPtr ->
  {# call gtk_widget_modify_base #}
    (toWidget self)
    ((fromIntegral . fromEnum) state)
    (castPtr colorPtr)

-- | Sets the font to use for a widget. All other style values are left
-- untouched. See also 'widgetModifyStyle'.
--
widgetModifyFont :: WidgetClass self => self
 -> Maybe FontDescription -- ^ @fontDesc@ - the font description to use, or
                          -- @Nothing@ to undo the effect of previous calls to
                          -- 'widgetModifyFont'.
 -> IO ()
widgetModifyFont self fontDesc =
  {# call gtk_widget_modify_font #}
    (toWidget self)
    (fromMaybe (FontDescription nullForeignPtr) fontDesc)

-- | Creates a new 'Context' with the appropriate colormap, font description,
-- and base direction for drawing text for this widget. See also
-- 'widgetGetPangoContext'.
--
widgetCreatePangoContext :: WidgetClass self => self
 -> IO PangoContext -- ^ returns the new 'PangoContext'
widgetCreatePangoContext self =
  makeNewGObject mkPangoContext $
  {# call gtk_widget_create_pango_context #}
    (toWidget self)

-- | Gets a 'Context' with the appropriate colormap, font description and base
-- direction for this widget. Unlike the context returned by
-- 'widgetCreatePangoContext', this context is owned by the widget (it can be
-- used until the screen for the widget changes or the widget is removed from
-- its toplevel), and will be updated to match any changes to the widget's
-- attributes.
--
-- If you create and keep a 'PangoLayout' using this context, you must deal
-- with changes to the context by calling 'layoutContextChanged' on the layout
-- in response to the ::style-set and ::direction-changed signals for the
-- widget.
--
widgetGetPangoContext :: WidgetClass self => self
 -> IO PangoContext -- ^ returns the 'PangoContext' for the widget.
widgetGetPangoContext self =
  makeNewGObject mkPangoContext $
  {# call gtk_widget_get_pango_context #}
    (toWidget self)

-- | A convenience function that uses the theme engine and RC file settings
-- for @widget@ to look up @stockId@ and render it to a pixbuf. @stockId@
-- should be a stock icon ID such as {GTK_STOCK_OPEN, FIXME: unknown
-- type/value} or {GTK_STOCK_OK, FIXME: unknown type/value}. @size@ should be a
-- size such as 'IconSizeMenu'. @detail@ should be a string that identifies the
-- widget or code doing the rendering, so that theme engines can special-case
-- rendering for that widget or code.
--
-- The pixels in the returned 'Pixbuf' are shared with the rest of the
-- application and should not be modified. The pixbuf should be freed after use
-- with 'gObjectUnref'.
--
widgetRenderIcon :: WidgetClass self => self
 -> String    -- ^ @stockId@ - a stock ID
 -> IconSize  -- ^ @size@ - a stock size. A size of (GtkIconSize)-1 means
              -- render at the size of the source and don't scale (if there are
              -- multiple source sizes, Gtk+ picks one of the available sizes).
 -> String    -- ^ @detail@ - render detail to pass to theme engine
 -> IO Pixbuf -- ^ returns a new pixbuf, or {@NULL@, FIXME: this should
              -- probably be converted to a Maybe data type} if the stock ID
              -- wasn't known
widgetRenderIcon self stockId size detail =
  makeNewGObject mkPixbuf $
  withUTFString detail $ \detailPtr ->
  withUTFString stockId $ \stockIdPtr ->
  {# call gtk_widget_render_icon #}
    (toWidget self)
    stockIdPtr
    ((fromIntegral . fromEnum) size)
    detailPtr

--------------------
-- Properties

-- | The mask that decides what kind of extension events this widget gets.
--
-- Default value: 'ExtensionEventsNone'
--
widgetExtensionEvents :: Attr Widget [ExtensionMode]
widgetExtensionEvents = Attr 
  widgetGetExtensionEvents
  widgetSetExtensionEvents

-- | \'direction\' property. See 'widgetGetDirection' and 'widgetSetDirection'
--
widgetDirection :: Attr Widget TextDirection
widgetDirection = Attr 
  widgetGetDirection
  widgetSetDirection

--------------------
-- Signals

-- Because there are so many similar signals (those that take an Event and
-- return a Bool) we will abstract out the skeleton. As some of these events
-- are emitted at a high rate often a bit has to be set to enable emission.
event :: WidgetClass w => SignalName -> [EventMask] ->
  ConnectAfter -> w -> (Event -> IO Bool) -> IO (ConnectId w)
event name eMask after obj fun = do
  id <- connect_BOXED__BOOL name marshalEvent after obj fun
  widgetAddEvents obj eMask
  return id

-- | A Button was pressed.
--
-- * This widget is part of a button which was just pressed. The event passed
--   to the user function is a 'Button' event.
--
onButtonPress, afterButtonPress :: WidgetClass w => w -> (Event -> IO Bool) ->
                                   IO (ConnectId w)
onButtonPress = event "button_press_event" [ButtonPressMask] False
afterButtonPress = event "button_press_event" [ButtonPressMask] True

-- | A Button was released.
--
onButtonRelease, afterButtonRelease :: WidgetClass w => w ->
                                       (Event -> IO Bool) -> IO (ConnectId w)
onButtonRelease = event "button_release_event" [ButtonReleaseMask] False
afterButtonRelease = event "button_release_event" [ButtonReleaseMask] True

-- | 
--
onClient, afterClient :: WidgetClass w => w -> (Event -> IO Bool) ->
                         IO (ConnectId w)
onClient = event "client_event" [] False
afterClient = event "client_event" [] True

-- | The widget's status has changed.
--
onConfigure, afterConfigure :: WidgetClass w => w -> (Event -> IO Bool) ->
                               IO (ConnectId w)
onConfigure = event "configure_event" []  False
afterConfigure = event "configure_event" []  True

-- | This signal is emitted when the close icon on the
-- surrounding window is pressed. The default action is to emit the
-- @\"destroy\"@ signal.
--
onDelete, afterDelete :: WidgetClass w => w -> (Event -> IO Bool) ->
                         IO (ConnectId w)
onDelete = event "delete_event" [] False
afterDelete = event "delete_event" [] True

-- | The widget will be destroyed.
--
-- * The widget received a destroy event from the window manager.
--
onDestroyEvent, afterDestroyEvent :: WidgetClass w => 
				     w -> (Event -> IO Bool) ->
				     IO (ConnectId w)
onDestroyEvent = event "destroy_event" [] False
afterDestroyEvent = event "destroy_event" [] True

-- | The default text direction was changed.
--
onDirectionChanged, afterDirectionChanged :: WidgetClass w => w ->
                                             (Event -> IO Bool) ->
                                             IO (ConnectId w)
onDirectionChanged = event "direction_changed" [] False
afterDirectionChanged = event "direction_changed" [] True

-- | Mouse cursor entered widget.
--
onEnterNotify, afterEnterNotify :: WidgetClass w => w -> (Event -> IO Bool) ->
                                   IO (ConnectId w)
onEnterNotify = event "enter_notify_event" [EnterNotifyMask] False
afterEnterNotify = event "enter_notify_event" [EnterNotifyMask] True

-- | Mouse cursor leaves widget.
--
onLeaveNotify, afterLeaveNotify :: WidgetClass w => w -> (Event -> IO Bool) ->
                                   IO (ConnectId w)
onLeaveNotify = event "leave_notify_event" [LeaveNotifyMask] False
afterLeaveNotify = event "leave_notify_event" [LeaveNotifyMask] True

-- | Instructs the widget to redraw.
--
onExpose, afterExpose :: WidgetClass w => w -> (Event -> IO Bool) ->
                         IO (ConnectId w)
onExpose = event "expose_event" [] False
afterExpose = event "expose_event" [] True

-- | Widget gains input focus.
--
onFocusIn, afterFocusIn :: WidgetClass w => w -> (Event -> IO Bool) ->
                           IO (ConnectId w)
onFocusIn = event "focus_in_event" [FocusChangeMask] False
afterFocusIn = event "focus_in_event" [FocusChangeMask] True

-- | Widget looses input focus.
--
onFocusOut, afterFocusOut :: WidgetClass w => w -> (Event -> IO Bool) ->
                             IO (ConnectId w)
onFocusOut = event "focus_out_event" [FocusChangeMask] False
afterFocusOut = event "focus_out_event" [FocusChangeMask] True

-- | The widget is about to receive all events.
--
-- * It is possible to redirect all input events to one widget to force the
--   user to use only this widget. Such a situation is initiated by
--   'addGrab'.
--
onGrabFocus, afterGrabFocus :: WidgetClass w => w -> IO () ->
                               IO (ConnectId w)
onGrabFocus = connect_NONE__NONE  "grab_focus" False
afterGrabFocus = connect_NONE__NONE "grab_focus" True

-- | The widget will be destroyed.
--
-- * This is the last signal this widget will receive.
--
onDestroy, afterDestroy :: WidgetClass w => w -> (IO ()) ->
                           IO (ConnectId w)
onDestroy = connect_NONE__NONE "destroy" False
afterDestroy = connect_NONE__NONE "destroy" True

-- | The widget was asked to hide itself.
--
-- * This signal is emitted each time 'widgetHide' is called. Use
--   'connectToUnmap' when your application needs to be informed
--   when the widget is actually removed from screen.
--
onHide, afterHide :: WidgetClass w => w -> IO () -> IO (ConnectId w)
onHide = connect_NONE__NONE "hide" False
afterHide = connect_NONE__NONE "hide" True

-- | The toplevel window changed.
--
-- * When a subtree of widgets is removed or added from a tree with a toplevel
--   window this signal is emitted. It is emitted on each widget in the
--   detached or attached subtree.
--
onHierarchyChanged, afterHierarchyChanged :: WidgetClass w => w -> IO () ->
                                             IO (ConnectId w)
onHierarchyChanged = connect_NONE__NONE "hierarchy_changed" False
afterHierarchyChanged = connect_NONE__NONE "hierarchy_changed" True

-- | A key was pressed.
--
onKeyPress, afterKeyPress :: WidgetClass w => w -> (Event -> IO Bool) ->
                             IO (ConnectId w)
onKeyPress = event "key_press_event" [KeyPressMask] False
afterKeyPress = event "key_press_event" [KeyPressMask] True

-- | A key was released.
--
onKeyRelease, afterKeyRelease :: WidgetClass w => w -> (Event -> IO Bool) ->
                                 IO (ConnectId w)
onKeyRelease = event "key_release_event" [KeyReleaseMask] False
afterKeyRelease = event "key_release_event" [KeyReleaseMask] True

-- | 
--
onMnemonicActivate, afterMnemonicActivate :: WidgetClass w => w ->
                                             (Bool -> IO Bool) ->
                                             IO (ConnectId w)
onMnemonicActivate = connect_BOOL__BOOL "mnemonic_activate" False
afterMnemonicActivate = connect_BOOL__BOOL "mnemonic_activate" True

-- | Track mouse movements.
--
-- * If @hint@ is False, a callback for every movement of the mouse is
--   generated. To avoid a backlog of mouse messages, it is usually sufficient
--   to sent @hint@ to True, generating only one event. The
--   application now has to state that it is ready for the next message by
--   calling 'drawWindowGetPointer'.
--
onMotionNotify, afterMotionNotify :: WidgetClass w => w -> Bool ->
                                     (Event -> IO Bool) -> 
                                     IO (ConnectId w)
onMotionNotify w hint = event "motion_notify_event" 
  (if hint then [PointerMotionHintMask] else [PointerMotionMask]) False w
afterMotionNotify w hint = event "motion_notify_event" 
  (if hint then [PointerMotionHintMask] else [PointerMotionMask]) True w

-- | 
--
onParentSet, afterParentSet :: (WidgetClass w, WidgetClass old) => w ->
                               (old -> IO ()) -> IO (ConnectId w)
onParentSet = connect_OBJECT__NONE "parent_set"  False
afterParentSet = connect_OBJECT__NONE "parent_set"  True

-- | 
--
onPopupMenu, afterPopupMenu :: WidgetClass w => w -> IO () -> IO (ConnectId w)
onPopupMenu = connect_NONE__NONE "popup_menu" False
afterPopupMenu = connect_NONE__NONE "popup_menu" True

-- | The input device became active.
--
-- * This event indicates that a pen of a graphics tablet or similar device is
--   now touching the tablet.
--
onProximityIn, afterProximityIn :: WidgetClass w => w -> (Event -> IO Bool) ->
                                   IO (ConnectId w)
onProximityIn = event "proximity_in_event" [ProximityInMask] False
afterProximityIn = event "proximity_in_event" [ProximityInMask] True

-- | The input device became inactive.
--
-- * The pen was removed from the graphics tablet's surface.
--
onProximityOut, afterProximityOut :: WidgetClass w => w ->
                                     (Event -> IO Bool) -> IO (ConnectId w)
onProximityOut = event "proximity_out_event" [ProximityOutMask] False
afterProximityOut = event "proximity_out_event" [ProximityOutMask] True

-- | This widget's drawing area is about to be
-- destroyed.
--
onRealize, afterRealize :: WidgetClass w => w -> IO () -> IO (ConnectId w)
onRealize = connect_NONE__NONE "realize" False
afterRealize = connect_NONE__NONE "realize" True

-- | The mouse wheel has turned.
--
-- * The 'Event' is always 'Scroll'.
--
onScroll, afterScroll :: WidgetClass w => w -> (Event -> IO Bool) ->
                         IO (ConnectId w)
onScroll = event "scroll_event" [ScrollMask] False
afterScroll = event "scroll_event" [ScrollMask] True

-- | The widget was asked to show itself.
--
-- * This signal is emitted each time 'widgetShow' is called. Use
--   'connectToMap' when your application needs to be informed when
--   the widget is actually shown.
--
onShow, afterShow :: WidgetClass w => w -> IO () -> IO (ConnectId w)
onShow = connect_NONE__NONE "show" False
afterShow = connect_NONE__NONE "show" True

-- | Inform widget about the size it has.
--
-- * After querying a widget for the size it wants to have (through emitting
--   the @\"sizeRequest\"@ signal) a container will emit this signal to
--   inform the widget about the real size it should occupy.
--
onSizeAllocate, afterSizeAllocate :: WidgetClass w => w ->
                                     (Allocation -> IO ()) -> IO (ConnectId w)
onSizeAllocate = connect_BOXED__NONE "size_allocate" peek False
afterSizeAllocate = connect_BOXED__NONE "size_allocate" peek True

-- | Query the widget for the size it likes to
-- have.
--
-- * A parent container emits this signal to its child to query the needed
--   height and width of the child. There is not guarantee that the widget
--   will actually get this area.
--
onSizeRequest, afterSizeRequest :: WidgetClass w => w -> (IO Requisition) ->
                                   IO (ConnectId w)
onSizeRequest w fun = connect_PTR__NONE "size_request" False w (\rqPtr -> do
  req <- fun
  unless (rqPtr==nullPtr) $ poke rqPtr req)
afterSizeRequest w fun = connect_PTR__NONE "size_request" True w (\rqPtr -> do
  req <- fun
  unless (rqPtr==nullPtr) $ poke rqPtr req) 

-- | 
--
onStateChanged, afterStateChanged :: WidgetClass w => w ->
                                     (StateType -> IO ()) -> IO (ConnectId w)
onStateChanged = connect_ENUM__NONE "state_changed" False
afterStateChanged = connect_ENUM__NONE "state_changed" True

-- | The widget was removed from screen.
--
onUnmap, afterUnmap :: WidgetClass w => w -> IO () -> IO (ConnectId w)
onUnmap = connect_NONE__NONE "unmap" False
afterUnmap = connect_NONE__NONE "unmap" True

-- | This widget's drawing area is about to be
-- destroyed.
--
onUnrealize, afterUnrealize :: WidgetClass w => w -> IO () -> IO (ConnectId w)
onUnrealize = connect_NONE__NONE "unrealize" False
afterUnrealize = connect_NONE__NONE "unrealize" True

-- | 
--
onVisibilityNotify, afterVisibilityNotify :: WidgetClass w => w ->
                                             (Event -> IO Bool) ->
                                             IO (ConnectId w)
onVisibilityNotify = 
  event "visibility_notify_event" [VisibilityNotifyMask] False
afterVisibilityNotify = 
  event "visibility_notify_event" [VisibilityNotifyMask] True

-- | 
--
onWindowState, afterWindowState :: WidgetClass w => w -> (Event -> IO Bool) ->
                                   IO (ConnectId w)
onWindowState = event "window_state_event" [] False
afterWindowState = event "window_state_event" [] True

