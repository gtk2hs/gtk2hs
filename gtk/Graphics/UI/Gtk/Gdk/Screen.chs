{-# LANGUAGE CPP #-}
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
  castToScreen, gTypeScreen,
  toScreen,

-- * Methods
  screenGetDefault,
#if GTK_MAJOR_VERSION <3
  screenGetSystemColormap,
#if GTK_CHECK_VERSION(2,8,0)
  screenGetRGBAColormap,
#endif
#ifndef DISABLE_DEPRECATED
  screenGetDefaultColormap,
  screenSetDefaultColormap,
#endif
#endif

  screenGetSystemVisual,
#if GTK_CHECK_VERSION(2,10,0)
  screenIsComposited,
#endif
  screenGetRootWindow,
  screenGetDisplay,
  screenGetNumber,
  screenGetWidth,
  screenGetHeight,
  screenGetWidthMm,
  screenGetHeightMm,
  screenGetWidthMM,
  screenGetHeightMM,
  screenListVisuals,
  screenGetToplevelWindows,
  screenMakeDisplayName,
  screenGetNMonitors,
  screenGetMonitorGeometry,
  screenGetMonitorAtPoint,
  screenGetMonitorAtWindow,
#if GTK_CHECK_VERSION(2,14,0)
  screenGetMonitorHeightMm,
  screenGetMonitorWidthMm,
  screenGetMonitorPlugName,
#endif
--  screenGetSetting,
#if GTK_CHECK_VERSION(2,10,0)
  screenGetActiveWindow,
  screenGetWindowStack,
#endif

-- * Attributes
  screenFontOptions,
  screenResolution,
#if GTK_MAJOR_VERSION < 3
  screenDefaultColormap,
#endif

-- * Signals
  screenSizeChanged,
#if GTK_CHECK_VERSION(2,10,0)
  screenCompositedChanged,
#if GTK_CHECK_VERSION(2,14,0)
  screenMonitorsChanged,
#endif
#endif

#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Signals
import System.Glib.Attributes
import System.Glib.Properties
import System.Glib.GList
{#import Graphics.UI.Gtk.Types#}
import Graphics.UI.Gtk.Signals
import Graphics.Rendering.Cairo.Types ( FontOptions(..), mkFontOptions,
                                        withFontOptions)
import Graphics.UI.Gtk.General.Structs ( Rectangle(..) )

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

#if GTK_MAJOR_VERSION < 3
screenGetDefaultColormap :: Screen
 -> IO Colormap -- ^ returns the default 'Colormap'.
screenGetDefaultColormap self =
  makeNewGObject mkColormap $
  {# call gdk_screen_get_default_colormap #}
    self
{-# DEPRECATED screenGetDefaultColormap "instead of 'screenGetDefaultColormap obj' use 'get obj screenDefaultColormap'" #-}

screenSetDefaultColormap :: Screen
 -> Colormap -- ^ @colormap@ - a 'Colormap'
 -> IO ()
screenSetDefaultColormap self colormap =
  {# call gdk_screen_set_default_colormap #}
    self
    colormap
{-# DEPRECATED screenSetDefaultColormap "instead of 'screenSetDefaultColormap obj value' use 'set obj [ screenDefaultColormap := value ]'" #-}

-- | Gets the system default colormap for @screen@
--
-- Removed in Gtk3.
screenGetSystemColormap :: Screen
 -> IO Colormap -- ^ returns the default colormap for @screen@.
screenGetSystemColormap self =
  makeNewGObject mkColormap $
  {# call gdk_screen_get_system_colormap #}
    self

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
-- Removed in Gtk3.
screenGetRGBAColormap :: Screen
 -> IO (Maybe Colormap) -- ^ returns a colormap to use for windows with an
                        -- alpha channel or @Nothing@ if the capability is not
                       -- available.
screenGetRGBAColormap self =
  maybeNull (makeNewGObject mkColormap) $
  {# call gdk_screen_get_rgba_colormap #}
    self
#endif
#endif

-- | Get the system's default visual for @screen@. This is the visual for the
-- root window of the display.
--
screenGetSystemVisual :: Screen
 -> IO Visual -- ^ returns the system visual
screenGetSystemVisual self =
  makeNewGObject mkVisual $
  {# call gdk_screen_get_system_visual #}
    self

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
screenGetWidthMM :: Screen
 -> IO Int -- ^ returns the width of @screen@ in millimeters.
screenGetWidthMM self =
  liftM fromIntegral $
  {# call gdk_screen_get_width_mm #}
    self

screenGetWidthMm = screenGetWidthMM

-- | Returns the height of @screen@ in millimeters. Note that on some X
-- servers this value will not be correct.
--
screenGetHeightMM :: Screen
 -> IO Int -- ^ returns the heigth of @screen@ in millimeters.
screenGetHeightMM self =
  liftM fromIntegral $
  {# call gdk_screen_get_height_mm #}
    self

screenGetHeightMm = screenGetHeightMM

-- | Lists the available visuals for the specified @screen@. A visual
-- describes a hardware image data format. For example, a visual might support
-- 24-bit color, or 8-bit color, and might expect pixels to be in a certain
-- format.
--
screenListVisuals :: Screen
 -> IO [Visual] -- ^ returns a list of visuals
screenListVisuals self =
  {# call gdk_screen_list_visuals #}
    self
  >>= fromGList
  >>= mapM (makeNewGObject mkVisual . return)


-- | Obtains a list of all toplevel windows known to GDK on the screen
-- @screen@. A toplevel window is a child of the root window (see
-- 'getDefaultRootWindow').
--
screenGetToplevelWindows :: Screen
 -> IO [DrawWindow] -- ^ returns list of toplevel windows
screenGetToplevelWindows self =
  {# call gdk_screen_get_toplevel_windows #}
    self
  >>= fromGList
  >>= mapM (makeNewGObject mkDrawWindow . return)


-- | Determines the name to pass to 'displayOpen' to get a 'Display' with this
-- screen as the default screen.
--
screenMakeDisplayName :: GlibString string => Screen
 -> IO string -- ^ returns a newly allocated string
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

-- | Retrieves the 'Rectangle' representing the size and
-- position of the individual monitor within the entire screen area.
--
-- Note that the size of the entire screen area can be retrieved via
-- 'screenGetWidth' and 'screenGetHeight'.
--
screenGetMonitorGeometry :: Screen
 -> Int               -- ^ @monitorNum@ - the monitor number.
 -> IO Rectangle
screenGetMonitorGeometry self monitorNum =
  alloca $ \rPtr -> do
  {# call gdk_screen_get_monitor_geometry #}
    self
    (fromIntegral monitorNum)
    (castPtr rPtr)
  peek rPtr

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

#if GTK_CHECK_VERSION(2,14,0)
-- | Gets the height in millimeters of the specified monitor.
--
-- * Available since Gdk version 2.14
--
screenGetMonitorHeightMm :: Screen
 -> Int    -- ^ @monitorNum@ - number of the monitor
 -> IO Int -- ^ returns the height of the monitor, or -1 if not available
screenGetMonitorHeightMm self monitorNum =
  liftM fromIntegral $
  {# call gdk_screen_get_monitor_height_mm #}
    self
    (fromIntegral monitorNum)

-- | Gets the width in millimeters of the specified monitor, if available.
--
-- * Available since Gdk version 2.14
--
screenGetMonitorWidthMm :: Screen
 -> Int    -- ^ @monitorNum@ - number of the monitor
 -> IO Int -- ^ returns the width of the monitor, or -1 if not available
screenGetMonitorWidthMm self monitorNum =
  liftM fromIntegral $
  {# call gdk_screen_get_monitor_width_mm #}
    self
    (fromIntegral monitorNum)

-- | Returns the output name of the specified monitor. Usually something like
-- VGA, DVI, or TV, not the actual product name of the display device.
--
-- * Available since Gdk version 2.14
--
screenGetMonitorPlugName :: GlibString string => Screen
 -> Int       -- ^ @monitorNum@ - number of the monitor
 -> IO (Maybe string) -- ^ returns a newly-allocated string containing the name of the
              -- monitor, or @Nothing@ if the name cannot be determined
screenGetMonitorPlugName self monitorNum = do
  sPtr <-
    {# call gdk_screen_get_monitor_plug_name #}
    self
    (fromIntegral monitorNum)
  if sPtr==nullPtr then return Nothing else liftM Just $ readUTFString sPtr
#endif

{-
-- | Retrieves a desktop-wide setting such as double-click time for the
-- 'Screen'@screen@.
--
-- FIXME needs a list of valid settings here, or a link to more information.
--
screenGetSetting :: GlibString string => Screen
 -> string      -- ^ @name@ - the name of the setting
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

-- these are only used for the attributes
screenGetFontOptions :: Screen
 -> IO (Maybe FontOptions)
screenGetFontOptions self = do
  fPtr <- {# call gdk_screen_get_font_options #} self
  if fPtr==nullPtr then return Nothing else liftM Just $ mkFontOptions (castPtr fPtr)

screenSetFontOptions :: Screen
 -> Maybe FontOptions
 -> IO ()
screenSetFontOptions self Nothing =
  {# call gdk_screen_set_font_options #} self nullPtr
screenSetFontOptions self (Just options) =
  withFontOptions options $ \fPtr ->
    {# call gdk_screen_set_font_options #} self (castPtr fPtr)

#if GTK_CHECK_VERSION(2,10,0)
-- | Returns the currently active window of this screen.
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
  maybeNull (wrapNewGObject mkDrawWindow) $
  {# call gdk_screen_get_active_window #}
    self
#endif

-- | Returns a list of 'DrawWindow's representing the
-- current window stack.
--
-- On X11, this is done by inspecting the _NET_CLIENT_LIST_STACKING property
-- on the root window, as described in the Extended Window Manager Hints. If
-- the window manager does not support the _NET_CLIENT_LIST_STACKING hint, this
-- function returns @Nothing@.
--
-- On other platforms, this function may return @Nothing@, depending on whether it is
-- implementable on that platform.
--
-- * Available since Gdk version 2.10
--
screenGetWindowStack :: Screen
 -> IO (Maybe [DrawWindow]) -- ^ returns a list of 'DrawWindow's for the
                            -- current window stack, or @Nothing@.
screenGetWindowStack self = do
  lPtr <- {# call gdk_screen_get_window_stack #} self
  if lPtr==nullPtr then return Nothing else liftM Just $ do
  fromGList lPtr >>= mapM (wrapNewGObject mkDrawWindow . return)
#endif

--------------------
-- Attributes

-- | The default font options for the screen.
--
screenFontOptions :: Attr Screen (Maybe FontOptions)
screenFontOptions = newAttr
  screenGetFontOptions
  screenSetFontOptions

-- | The resolution for fonts on the screen.
--
-- Default value: -1
--
screenResolution :: Attr Screen Double
screenResolution = newAttrFromDoubleProperty "resolution"

#if GTK_MAJOR_VERSION < 3
-- | Sets the default @colormap@ for @screen@.
--
-- Gets the default colormap for @screen@.
--
-- Removed in Gtk3.
screenDefaultColormap :: Attr Screen Colormap
screenDefaultColormap = newAttr
  screenGetDefaultColormap
  screenSetDefaultColormap
#endif

--------------------
-- Signals

-- | The ::size_changed signal is emitted when the pixel width or height of a
-- screen changes.
--
screenSizeChanged :: ScreenClass self => Signal self (IO ())
screenSizeChanged = Signal (connect_NONE__NONE "size-changed")

#if GTK_CHECK_VERSION(2,10,0)
-- | The 'screenCompositedChanged' signal is emitted when the composited status of
-- the screen changes
--
-- * Available since Gdk version 2.10
--
screenCompositedChanged :: ScreenClass self => Signal self (IO ())
screenCompositedChanged = Signal (connect_NONE__NONE "composited-changed")

#if GTK_CHECK_VERSION(2,14,0)
-- | The 'screenMonitorsChanged' signal is emitted when the number, size or
-- position of the monitors attached to the screen change.
--
-- Only for X for now. Future implementations for Win32 and OS X may be a
-- possibility.
--
-- * Available since Gdk version 2.14
--
screenMonitorsChanged :: ScreenClass self => Signal self (IO ())
screenMonitorsChanged = Signal (connect_NONE__NONE "monitors-changed")
#endif
#endif
