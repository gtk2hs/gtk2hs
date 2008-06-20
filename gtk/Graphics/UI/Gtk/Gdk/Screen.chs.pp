-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Screen
--
--  Author : Duncan Coutts
--
--  Created: 29 October 2007
--
--  Copyright (C) 2007 Duncan Coutts
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
-- Object representing a physical screen
--
-- * Module available since Gdk version 2.2
--
module Graphics.UI.Gtk.Gdk.Screen (

-- * Detail
--
-- | 'Screen' objects are the GDK representation of a physical screen. It is
-- used throughout GDK and Gtk+ to specify which screen the top level windows
-- are to be displayed on. It is also used to query the screen specification
-- and default settings such as the default colormap
-- ('screenGetDefaultColormap'), the screen width ('screenGetWidth'), etc.
--
-- Note that a screen may consist of multiple monitors which are merged to
-- form a large screen area.

-- * Class Hierarchy
--
-- |
-- @
-- |  'GObject'
-- |   +----Screen
-- @

#if GTK_CHECK_VERSION(2,2,0)
-- * Types
  Screen,
  ScreenClass,
  castToScreen,
  toScreen,

-- * Methods
  screenGetDefault,
  screenGetSystemColormap,
--  screenGetSystemVisual,
  screenGetRGBColormap,
--  screenGetRGBVisual,
#if GTK_CHECK_VERSION(2,8,0)
  screenGetRGBAColormap,
--  screenGetRGBAVisual,
#if GTK_CHECK_VERSION(2,10,0)
  screenIsComposited,
#endif
#endif
  screenGetRootWindow,
  screenGetDisplay,
  screenGetNumber,
  screenGetWidth,
  screenGetHeight,
  screenGetWidthMm,
  screenGetHeightMm,
--  screenListVisuals,
--  screenGetToplevelWindows,
  screenMakeDisplayName,
  screenGetNMonitors,
--  screenGetMonitorGeometry,
  screenGetMonitorAtPoint,
  screenGetMonitorAtWindow,
--  screenBroadcastClientMessage,
--  screenGetSetting,
#if GTK_CHECK_VERSION(2,10,0)
  screenGetActiveWindow,
--  screenGetWindowStack,
#endif

-- * Attributes
--  screenFontOptions,
  screenResolution,
  screenDefaultColormap,

-- * Signals
  screenSizeChanged,
#if GTK_CHECK_VERSION(2,10,0)
  screenCompositedChanged,
#endif

#endif
  ) where

import Control.Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Signals
import System.Glib.Attributes
import System.Glib.Properties
{#import Graphics.UI.Gtk.Types#}
import Graphics.UI.Gtk.Signals

{# context lib="gdk" prefix="gdk" #}

#if GTK_CHECK_VERSION(2,2,0)
--------------------
-- Methods

-- | Gets the default screen for the default display. (See
-- 'displayGetDefault').
--
screenGetDefault ::
    IO (Maybe Screen) -- ^ returns a 'Screen', or @Nothing@ if there is no
                      --  default display.
screenGetDefault =
  maybeNull (makeNewGObject mkScreen) $
  {# call gdk_screen_get_default #}

screenGetDefaultColormap :: Screen
 -> IO Colormap -- ^ returns the default 'Colormap'.
screenGetDefaultColormap self =
  makeNewGObject mkColormap $
  {# call gdk_screen_get_default_colormap #}
    self

screenSetDefaultColormap :: Screen
 -> Colormap -- ^ @colormap@ - a 'Colormap'
 -> IO ()
screenSetDefaultColormap self colormap =
  {# call gdk_screen_set_default_colormap #}
    self
    colormap

-- | Gets the system's default colormap for @screen@
--
screenGetSystemColormap :: Screen
 -> IO Colormap -- ^ returns the default colormap for @screen@.
screenGetSystemColormap self =
  makeNewGObject mkColormap $
  {# call gdk_screen_get_system_colormap #}
    self

{-
-- | Get the system's default visual for @screen@. This is the visual for the
-- root window of the display. The return value should not be freed.
--
screenGetSystemVisual :: Screen
 -> IO Visual -- ^ returns the system visual
screenGetSystemVisual self =
  makeNewGObject mkVisual $
  {# call gdk_screen_get_system_visual #}
    self
-}

-- | Gets the preferred colormap for rendering image data on @screen@. Not a
-- very useful function; historically, GDK could only render RGB image data to
-- one colormap and visual, but in the current version it can render to any
-- colormap and visual. So there's no need to call this function.
--
screenGetRGBColormap :: Screen
 -> IO Colormap -- ^ returns the preferred colormap
screenGetRGBColormap self =
  makeNewGObject mkColormap $
  {# call gdk_screen_get_rgb_colormap #}
    self

{-
-- | Gets a \"preferred visual\" chosen by GdkRGB for rendering image data on
-- @screen@. In previous versions of GDK, this was the only visual GdkRGB could
-- use for rendering. In current versions, it's simply the visual GdkRGB would
-- have chosen as the optimal one in those previous versions. GdkRGB can now
-- render to drawables with any visual.
--
screenGetRGBVisual :: Screen
 -> IO Visual -- ^ returns The 'Visual' chosen by GdkRGB.
screenGetRGBVisual self =
  makeNewGObject mkVisual $
  {# call gdk_screen_get_rgb_visual #}
    self
-}

#if GTK_CHECK_VERSION(2,8,0)
-- | Gets a colormap to use for creating windows or pixmaps with an alpha
-- channel. The windowing system on which Gtk+ is running may not support this
-- capability, in which case @Nothing@ will be returned. Even if a
-- non-@Nothing@ value is returned, its possible that the window's alpha
-- channel won't be honored when displaying the window on the screen: in
-- particular, for X an appropriate windowing manager and compositing manager
-- must be running to provide appropriate display.
--
-- * Available since Gdk version 2.8
--
screenGetRGBAColormap :: Screen
 -> IO (Maybe Colormap) -- ^ returns a colormap to use for windows with an
                        -- alpha channel or @Nothing@ if the capability is not
                        -- available.
screenGetRGBAColormap self =
  maybeNull (makeNewGObject mkColormap) $
  {# call gdk_screen_get_rgba_colormap #}
    self

{-
-- | Gets a visual to use for creating windows or pixmaps with an alpha
-- channel. See the docs for 'screenGetRGBAColormap' for caveats.
--
-- * Available since Gdk version 2.8
--
screenGetRGBAVisual :: Screen
 -> IO Visual -- ^ returns a visual to use for windows with an alpha channel
              -- or {@NULL@, FIXME: this should probably be converted to a
              -- Maybe data type} if the capability is not available.
screenGetRGBAVisual self =
  makeNewGObject mkVisual $
  {# call gdk_screen_get_rgba_visual #}
    self
-}

#if GTK_CHECK_VERSION(2,10,0)
-- | Returns whether windows with an RGBA visual can reasonably be expected to
-- have their alpha channel drawn correctly on the screen.
--
-- On X11 this function returns whether a compositing manager is compositing
-- @screen@.
--
-- * Available since Gdk version 2.10
--
screenIsComposited :: Screen
 -> IO Bool -- ^ returns Whether windows with RGBA visuals can reasonably be
            -- expected to have their alpha channels drawn correctly on the
            -- screen.
screenIsComposited self =
  liftM toBool $
  {# call gdk_screen_is_composited #}
    self
#endif
#endif

-- | Gets the root window of @screen@.
--
screenGetRootWindow :: Screen
 -> IO DrawWindow -- ^ returns the root window
screenGetRootWindow self =
  makeNewGObject mkDrawWindow $
  {# call gdk_screen_get_root_window #}
    self

-- | Gets the display to which the @screen@ belongs.
--
screenGetDisplay :: Screen
 -> IO Display -- ^ returns the display to which @screen@ belongs
screenGetDisplay self =
  makeNewGObject mkDisplay $
  {# call gdk_screen_get_display #}
    self

-- | Gets the index of @screen@ among the screens in the display to which it
-- belongs. (See 'screenGetDisplay')
--
screenGetNumber :: Screen
 -> IO Int -- ^ returns the index
screenGetNumber self =
  liftM fromIntegral $
  {# call gdk_screen_get_number #}
    self

-- | Gets the width of @screen@ in pixels
--
screenGetWidth :: Screen
 -> IO Int -- ^ returns the width of @screen@ in pixels.
screenGetWidth self =
  liftM fromIntegral $
  {# call gdk_screen_get_width #}
    self

-- | Gets the height of @screen@ in pixels
--
screenGetHeight :: Screen
 -> IO Int -- ^ returns the height of @screen@ in pixels.
screenGetHeight self =
  liftM fromIntegral $
  {# call gdk_screen_get_height #}
    self

-- | Gets the width of @screen@ in millimeters. Note that on some X servers
-- this value will not be correct.
--
screenGetWidthMm :: Screen
 -> IO Int -- ^ returns the width of @screen@ in millimeters.
screenGetWidthMm self =
  liftM fromIntegral $
  {# call gdk_screen_get_width_mm #}
    self

-- | Returns the height of @screen@ in millimeters. Note that on some X
-- servers this value will not be correct.
--
screenGetHeightMm :: Screen
 -> IO Int -- ^ returns the heigth of @screen@ in millimeters.
screenGetHeightMm self =
  liftM fromIntegral $
  {# call gdk_screen_get_height_mm #}
    self

{-
-- | Lists the available visuals for the specified @screen@. A visual
-- describes a hardware image data format. For example, a visual might support
-- 24-bit color, or 8-bit color, and might expect pixels to be in a certain
-- format.
--
-- Call 'gListFree' on the return value when you\'re finished with it.
--
screenListVisuals :: Screen
 -> IO [{- element type -}] -- ^ returns a list of visuals; the list must be
                            -- freed, but not its contents
screenListVisuals self =
  {# call gdk_screen_list_visuals #}
    self
  >>= fromGList
  >>= mapM (\elemPtr -> {-marshal elem-})


-- | Obtains a list of all toplevel windows known to GDK on the screen
-- @screen@. A toplevel window is a child of the root window (see
-- 'getDefaultRootWindow').
--
-- The returned list should be freed with 'gListFree', but its elements need
-- not be freed.
--
screenGetToplevelWindows :: Screen
 -> IO [{- element type -}] -- ^ returns list of toplevel windows, free with
                            -- 'gListFree'
screenGetToplevelWindows self =
  {# call gdk_screen_get_toplevel_windows #}
    self
  >>= fromGList
  >>= mapM (\elemPtr -> {-marshal elem-})
-}

-- | Determines the name to pass to 'displayOpen' to get a 'Display' with this
-- screen as the default screen.
--
screenMakeDisplayName :: Screen
 -> IO String -- ^ returns a newly allocated string, free with 'gFree'
screenMakeDisplayName self =
  {# call gdk_screen_make_display_name #}
    self
  >>= readUTFString

-- | Returns the number of monitors which @screen@ consists of.
--
screenGetNMonitors :: Screen
 -> IO Int -- ^ returns number of monitors which @screen@ consists of.
screenGetNMonitors self =
  liftM fromIntegral $
  {# call gdk_screen_get_n_monitors #}
    self

{-
-- | Retrieves the {GdkRectangle, FIXME: boxed type} representing the size and
-- position of the individual monitor within the entire screen area.
--
-- Note that the size of the entire screen area can be retrieved via
-- 'screenGetWidth' and 'screenGetHeight'.
--
screenGetMonitorGeometry :: Screen
 -> Int               -- ^ @monitorNum@ - the monitor number.
 -> {-GdkRectangle*-} -- ^ @dest@ - a {GdkRectangle, FIXME: boxed type} to be
                      -- filled with the monitor geometry
 -> IO ()
screenGetMonitorGeometry self monitorNum dest =
  {# call gdk_screen_get_monitor_geometry #}
    self
    (fromIntegral monitorNum)
    {-dest-}
-}

-- | Returns the monitor number in which the point (@x@,@y@) is located.
--
screenGetMonitorAtPoint :: Screen
 -> Int    -- ^ @x@ - the x coordinate in the virtual screen.
 -> Int    -- ^ @y@ - the y coordinate in the virtual screen.
 -> IO Int -- ^ returns the monitor number in which the point (@x@,@y@) lies,
           -- or a monitor close to (@x@,@y@) if the point is not in any
           -- monitor.
screenGetMonitorAtPoint self x y =
  liftM fromIntegral $
  {# call gdk_screen_get_monitor_at_point #}
    self
    (fromIntegral x)
    (fromIntegral y)

-- | Returns the number of the monitor in which the largest area of the
-- bounding rectangle of @window@ resides.
--
screenGetMonitorAtWindow :: Screen
 -> DrawWindow -- ^ @window@ - a 'DrawWindow'
 -> IO Int     -- ^ returns the monitor number in which most of @window@ is
               -- located, or if @window@ does not intersect any monitors, a
               -- monitor, close to @window@.
screenGetMonitorAtWindow self window =
  liftM fromIntegral $
  {# call gdk_screen_get_monitor_at_window #}
    self
    window

{-
-- | On X11, sends an X ClientMessage event to all toplevel windows on
-- @screen@.
--
-- Toplevel windows are determined by checking for the WM_STATE property, as
-- described in the Inter-Client Communication Conventions Manual (ICCCM). If
-- no windows are found with the WM_STATE property set, the message is sent to
-- all children of the root window.
--
-- On Windows, broadcasts a message registered with the name
-- GDK_WIN32_CLIENT_MESSAGE to all top-level windows. The amount of data is
-- limited to one long, i.e. four bytes.
--
screenBroadcastClientMessage :: Screen
 -> {-GdkEvent*-} -- ^ @event@ - the {GdkEvent, FIXME: unknown type\/value}.
 -> IO ()
screenBroadcastClientMessage self event =
  {# call gdk_screen_broadcast_client_message #}
    self
    {-event-}

-- | Retrieves a desktop-wide setting such as double-click time for the
-- 'Screen'@screen@.
--
-- FIXME needs a list of valid settings here, or a link to more information.
--
screenGetSetting :: Screen
 -> String      -- ^ @name@ - the name of the setting
 -> {-GValue*-} -- ^ @value@ - location to store the value of the setting
 -> IO Bool     -- ^ returns @True@ if the setting existed and a value was
                -- stored in @value@, @False@ otherwise.
screenGetSetting self name value =
  liftM toBool $
  withUTFString name $ \namePtr ->
  {# call gdk_screen_get_setting #}
    self
    namePtr
    {-value-}
-}

-- | Returns the screen's currently active window.
--
-- On X11, this is done by inspecting the _NET_ACTIVE_WINDOW property on the
-- root window, as described in the Extended Window Manager Hints. If there is
-- no currently currently active window, or the window manager does not support
-- the _NET_ACTIVE_WINDOW hint, this function returns @Nothing@.
--
-- On other platforms, this function may return @Nothing@, depending on whether
-- it is implementable on that platform.
--
-- * Available since Gdk version 2.10
--
screenGetActiveWindow :: Screen
 -> IO (Maybe DrawWindow) -- ^ returns the currently active window, or
                          -- @Nothing@.
screenGetActiveWindow self =
  maybeNull (makeNewGObject mkDrawWindow) $
  {# call gdk_screen_get_active_window #}
    self

{-
-- | Returns a {GList, FIXME: struct type} of 'DrawWindow's representing the
-- current window stack.
--
-- On X11, this is done by inspecting the _NET_CLIENT_LIST_STACKING property
-- on the root window, as described in the Extended Window Manager Hints. If
-- the window manager does not support the _NET_CLIENT_LIST_STACKING hint, this
-- function returns {@NULL@, FIXME: this should probably be converted to a
-- Maybe data type}.
--
-- On other platforms, this function may return {@NULL@, FIXME: this should
-- probably be converted to a Maybe data type}, depending on whether it is
-- implementable on that platform.
--
-- The returned list is newly allocated and owns references to the windows
-- it contains, so it should be freed using 'gListFree' and its windows unrefed
-- using 'gObjectUnref' when no longer needed.
--
-- * Available since Gdk version 2.10
--
screenGetWindowStack :: Screen
 -> IO [{- element type -}] -- ^ returns a list of 'DrawWindow's for the
                            -- current window stack, or {@NULL@, FIXME: this
                            -- should probably be converted to a Maybe data
                            -- type}.
screenGetWindowStack self =
  {# call gdk_screen_get_window_stack #}
    self
  >>= fromGList
  >>= mapM (\elemPtr -> {-marshal elem-})
#endif
-}

--------------------
-- Attributes

{-
-- | The default font options for the screen.
--
screenFontOptions :: Attr Screen {-gpointer-}
screenFontOptions = newAttrFrom{-gpointer-}Property "font-options"
-}

-- | The resolution for fonts on the screen.
--
-- Default value: -1
--
screenResolution :: Attr Screen Double
screenResolution = newAttrFromDoubleProperty "resolution"

-- | Sets the default @colormap@ for @screen@.
--
-- Gets the default colormap for @screen@.
--
screenDefaultColormap :: Attr Screen Colormap
screenDefaultColormap = newAttr
  screenGetDefaultColormap
  screenSetDefaultColormap

--------------------
-- Signals

-- | The ::size_changed signal is emitted when the pixel width or height of a
-- screen changes.
--
screenSizeChanged :: ScreenClass self => Signal self (IO ())
screenSizeChanged = Signal (connect_NONE__NONE "size_changed")

#if GTK_CHECK_VERSION(2,10,0)
-- | The ::composited_changed signal is emitted when the composited status of
-- the screen changes
--
-- * Available since Gdk version 2.10
--
screenCompositedChanged :: ScreenClass self => Signal self (IO ())
screenCompositedChanged = Signal (connect_NONE__NONE "composited_changed")
#endif
