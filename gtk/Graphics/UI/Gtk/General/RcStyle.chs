{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) RcStyle
--
--  Author : Axel Simon
--
--  Created: 22 October 2009
--
--  Copyright (C) 2009 Axel Simon
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
-- Routines for handling resource files
--
module Graphics.UI.Gtk.General.RcStyle (

-- * Detail
--
-- | Gtk+ provides resource file mechanism for configuring various aspects of
-- the operation of a Gtk+ program at runtime.

-- ** Default files
--
-- | An application can cause Gtk+ to parse a specific RC file by calling
-- 'rcParse'. In addition to this, certain files will be read at the end of
-- 'initGUI'. Unless modified, the files looked for will be
-- @\<SYSCONFDIR>\/gtk-2.0\/gtkrc@ and @.gtkrc-2.0@ in the users home directory.
-- @(\<SYSCONFDIR>@ defaults to @\/usr\/local\/etc@. It can be changed with the
-- --prefix or --sysconfdir options when configuring Gtk+.) Note that although
-- the filenames contain the version number 2.0, all 2.x versions of Gtk+ look
-- for these files.
--
-- The set of these default files can be retrieved with 'rcGetDefaultFiles'
-- and modified with 'rcAddDefaultFile' and 'rcSetDefaultFiles'. Additionally,
-- the @GTK2_RC_FILES@ environment variable can be set to a
-- @G_SEARCHPATH_SEPARATOR_S@-separated list of
-- files in order to overwrite the set of default files at runtime.
--
-- For each RC file, in addition to the file itself, Gtk+ will look for a
-- locale-specific file that will be parsed after the main file. For instance,
-- if @LANG@ is set to @ja_JP.ujis@, when loading the default file @~\/.gtkrc@ then
-- Gtk+ looks for @~\/.gtkrc.ja_JP@ and @~\/.gtkrc.ja@, and parses the first of
-- those that exists.

-- ** Pathnames and patterns
--
-- | A resource file defines a number of styles and key bindings and attaches
-- them to particular widgets. The attachment is done by the @widget@,
-- @widget_class@, and @class@ declarations. As an example of such a statement:
-- attaches the style @\"my-entry-class\"@ to all widgets whose widget path
-- matches the pattern @\"mywindow.*.GtkEntry\"@. That is, all 'Entry' widgets
-- which are part of a 'Window' named @\"mywindow\"@.
--
-- > widget "mywindow.*.GtkEntry" style "my-entry-class"
--
-- The patterns here are given in the standard shell glob syntax. The
-- @\"?\"@ wildcard matches any character, while @\"*\"@ matches zero or more
-- of any character. The three types of matching are against the widget path,
-- the class path and the class hierarchy. Both the widget path and the class
-- path consist of a @\".\"@ separated list of all the parents of the widget
-- and the widget itself from outermost to innermost. The difference is that in
-- the widget path, the name assigned by 'widgetSetName' is used if present,
-- otherwise the class name of the widget, while for the class path, the class
-- name is always used.
--
-- Since Gtk+ 2.10, @widget_class@ paths can also contain @\<classname>@
-- substrings, which are matching the class with the given name and any derived
-- classes. For instance, will match 'Label' widgets which are contained in any
-- kind of menu item.
--
-- > widget_class "*GtkMenuItem.GtkLabel" style "my-style"
--
-- So, if you have a 'Entry' named @\"myentry\"@, inside of a horizontal box
-- in a window named @\"mywindow\"@, then the widget path is:
-- @\"mywindow.GtkHBox.myentry\"@ while the class path is:
-- @\"GtkWindow.GtkHBox.GtkEntry\"@.
--
-- Matching against class is a little different. The pattern match is done
-- against all class names in the widgets class hierarchy (not the layout
-- hierarchy) in sequence, so the pattern: will match not just 'Button'
-- widgets, but also 'ToggleButton' and 'CheckButton' widgets, since those
-- classes derive from 'Button'.
--
-- > class "GtkButton" style "my-style"
--
-- Additionally, a priority can be specified for each pattern, and styles
-- override other styles first by priority, then by pattern type and then by
-- order of specification (later overrides earlier). The priorities that can be
-- specified are (highest to lowest):
--
-- * @highest@
--
-- * @rc@
--
-- * @theme@
--
-- * @application@
--
-- * @gtk@
--
-- * @lowest@
--
-- @rc@ is the default for styles read from an RC file, @theme@ is the
-- default for styles read from theme RC files, @application@ should be used
-- for styles an application sets up, and @gtk@ is used for styles that Gtk+
-- creates internally.

-- ** Optimizing RC Style Matches
--
-- | Everytime a widget is created and added to the layout hierarchy of a
-- 'Window' (\"anchored\" to be exact), a list of matching RC styles out of all
-- RC styles read in so far is composed. For this, every RC style is matched
-- against the widgets class path, the widgets name path and widgets
-- inheritance hierarchy. As a consequence, significant slowdown can be caused
-- by utilization of many RC styles and by using RC style patterns that are
-- slow or complicated to match against a given widget. The following ordered
-- list provides a number of advices (prioritized by effectiveness) to reduce
-- the performance overhead associated with RC style matches:
--
-- Move RC styles for specific applications into RC files dedicated to those
-- applications and parse application specific RC files only from applications
-- that are affected by them. This reduces the overall amount of RC styles that
-- have to be considered for a match across a group of applications.
--
-- Merge multiple styles which use the same matching rule, for instance: is
-- faster to match as:
--
-- >      style "Foo" { foo_content }
-- >      class "X" style "Foo"
-- >      style "Bar" { bar_content }
-- >      class "X" style "Bar"
--
-- >      style "FooBar" { foo_content bar_content }
-- >      class "X" style "FooBar"
--
-- Use of wildcards should be avoided, this can reduce the individual RC
-- style match to a single integer comparison in most cases.
--
-- To avoid complex recursive matching, specification of full class names
-- (for @class@ matches) or full path names (for @widget@ and @widget_class@
-- matches) is to be preferred over shortened names containing @\"*\"@ or
-- @\"?\"@.
--
-- If at all necessary, wildcards should only be used at the tail or head of
-- a pattern. This reduces the match complexity to a string comparison per RC
-- style.
--
-- When using wildcards, use of @\"?\"@ should be preferred over @\"*\"@.
-- This can reduce the matching complexity from O(n^2) to O(n). For example
-- @\"Gtk*Box\"@ can be turned into @\"Gtk?Box\"@ and will still match 'HBox'
-- and 'VBox'.
--
-- The use of @\"*\"@ wildcards should be restricted as much as possible,
-- because matching @\"A*B*C*RestString\"@ can result in matching complexities
-- of O(n^2) worst case.

-- ** Toplevel declarations
--
-- | An RC file is a text file which is composed of a sequence of
-- declarations. @\'#\'@ characters delimit comments and the portion of a line
-- after a @\'#\'@ is ignored when parsing an RC file.
--
-- The possible toplevel declarations are:
--
-- [@binding name      { ... }@] Declares a binding set.
--
-- [@class pattern           [ style | binding \][ : priority \]           name@]
-- Specifies a style or binding set for a particular branch of the inheritance
-- hierarchy.
--
-- [@include filename@] Parses another file at this point. If filename is
-- not an absolute filename, it is searched in the directories of the currently
-- open RC files. Gtk+ also tries to load a locale-specific variant of the
-- included file.
--
-- [@module_path path@] Sets a path (a list of directories separated by
-- colons) that will be searched for theme engines referenced in RC files.
--
-- [@pixmap_path path@] Sets a path (a list of directories separated by
-- colons) that will be searched for pixmaps referenced in RC files.
--
-- [@im_module_file pathname@] Sets the pathname for the IM modules file.
-- Setting this from RC files is deprecated; you should use the environment
-- variable GTK_IM_MODULE_FILE instead.
--
-- [@style name [ =     parent \] { ... }@] Declares a style.
--
-- [@widget pattern           [ style | binding \][ : priority \]           name@]
-- Specifies a style or binding set for a particular group of widgets by
-- matching on the widget pathname.
--
-- [@widget_class pattern           [ style | binding \][ : priority \]           name@]
-- Specifies a style or binding set for a particular group of widgets by
-- matching on the class pathname.
--
-- [setting = value] Specifies a value for a setting. Note that settings in
-- RC files are overwritten by system-wide settings (which are managed by an
-- XSettings manager on X11).

-- ** Styles
--
-- | A RC style is specified by a @style@ declaration in a RC file, and then
-- bound to widgets with a @widget@, @widget_class@, or @class@ declaration.
-- All styles applying to a particular widget are composited together with
-- @widget@ declarations overriding @widget_class@ declarations which, in turn,
-- override @class@ declarations. Within each type of declaration, later
-- declarations override earlier ones.
--
-- Within a @style@ declaration, the possible elements are:
--
-- [@bg[state\] =       color@] Sets the color used for the background of
-- most widgets.
--
-- [@fg[state\] =       color@] Sets the color used for the foreground of
-- most widgets.
--
-- [@base[state\] =       color@] Sets the color used for the background of
-- widgets displaying editable text. This color is used for the background of,
-- among others, {GtkText, FIXME: unknown type\/value}, 'Entry', 'List', and
-- 'CList'.
--
-- [@text[state\] =       color@] Sets the color used for foreground of
-- widgets using @base@ for the background color.
--
-- [@xthickness =       number@] Sets the xthickness, which is used for
-- various horizontal padding values in Gtk+.
--
-- [@ythickness =       number@] Sets the ythickness, which is used for
-- various vertical padding values in Gtk+.
--
-- [@bg_pixmap[state\] =       pixmap@] Sets a background pixmap to be used
-- in place of the @bg@ color (or for {GtkText, FIXME: unknown type\/value}, in
-- place of the @base@ color. The special value @\"\<parent>\"@ may be used to
-- indicate that the widget should use the same background pixmap as its
-- parent. The special value @\"\<none>\"@ may be used to indicate no
-- background pixmap.
--
-- [@font = font@] Starting with Gtk+ 2.0, the \"font\" and \"fontset\"
-- declarations are ignored; use \"font_name\" declarations instead.
--
-- [@fontset = font@] Starting with Gtk+ 2.0, the \"font\" and \"fontset\"
-- declarations are ignored; use \"font_name\" declarations instead.
--
-- [@font_name = font@] Sets the font for a widget. font must be a Pango
-- font name, e.g. @\"Sans Italic 10\"@. For details about Pango font names,
-- see 'fontDescriptionFromString'.
--
-- [@stock[\"stock-id\"\] = { icon source specifications }@] Defines the
-- icon for a stock item.
--
-- [@color[\"color-name\"\] = color specification@] Since 2.10, this element
-- can be used to defines symbolic colors. See below for the syntax of color
-- specifications.
--
-- [@engine \"engine\" { engine-specific settings }@] Defines the engine to
-- be used when drawing with this style.
--
-- [@class::property = value@] Sets a style property for a widget class.
--
-- The colors and background pixmaps are specified as a function of the
-- state of the widget. The states are:
--
-- [@NORMAL@] A color used for a widget in its normal state.
--
-- [@ACTIVE@] A variant of the @NORMAL@ color used when the widget is in the
-- 'StateActive' state, and also for the trough of a ScrollBar, tabs of a
-- NoteBook other than the current tab and similar areas. Frequently, this
-- should be a darker variant of the @NORMAL@ color.
--
-- [@PRELIGHT@] A color used for widgets in the 'StatePrelight' state. This
-- state is the used for Buttons and MenuItems that have the mouse cursor over
-- them, and for their children.
--
-- [@SELECTED@] A color used to highlight data selected by the user. for
-- instance, the selected items in a list widget, and the selection in an
-- editable widget.
--
-- [@INSENSITIVE@] A color used for the background of widgets that have been
-- set insensitive with 'widgetSetSensitive'.
--
-- Colors can be specified as a string containing a color name (GTK+ knows
-- all names from the X color database \/usr\/lib\/X11\/rgb.txt), in one of the
-- hexadecimal forms @#rrrrggggbbbb@, @#rrrgggbbb@, @#rrggbb@, or @#rgb@, where
-- @r@, @g@ and @b@ are hex digits, or they can be specified as a triplet @{ r,
-- g, b}@, where @r@, @g@ and @b@ are either integers in the range 0-65535 or
-- floats in the range 0.0-1.0.
--
-- Since 2.10, colors can also be specified by refering to a symbolic color,
-- as follows: @\@color-name@, or by using expressions to combine colors. The
-- following expressions are currently supported:
--
-- [mix (factor, color1, color2)] Computes a new color by mixing color1 and
-- color2. The factor determines how close the new color is to color1. A factor
-- of 1.0 gives pure color1, a factor of 0.0 gives pure color2.
--
-- [shade (factor, color)] Computes a lighter or darker variant of color. A
-- factor of 1.0 leaves the color unchanged, smaller factors yield darker
-- colors, larger factors yield lighter colors.
--
-- [lighter (color)] This is an abbreviation for @shade (1.3, color)@.
--
-- [darker (color)] This is an abbreviation for @shade (0.7, color)@.
--
-- Here are some examples of color expressions:
--
-- >  mix (0.5, "red", "blue")
-- >  shade (1.5, mix (0.3, "#0abbc0", { 0.3, 0.5, 0.9 }))
-- >  lighter (@foreground)
--
-- In a @stock@ definition, icon sources are specified as a 4-tuple of image
-- filename or icon name, text direction, widget state, and size, in that
-- order. Each icon source specifies an image filename or icon name to use with
-- a given direction, state, and size. Filenames are specified as a string such
-- as @\"itemltr.png\"@, while icon names (looked up in the current icon
-- theme), are specified with a leading @\@@, such as @\@\"item-ltr\"@. The @*@
-- character can be used as a wildcard, and if direction\/state\/size are
-- omitted they default to @*@. So for example, the following specifies
-- different icons to use for left-to-right and right-to-left languages: This
-- could be abbreviated as follows:
--
-- > stock["my-stock-item"] =
-- > {
-- >   { "itemltr.png", LTR, *, * },
-- >   { "itemrtl.png", RTL, *, * }
-- > }
--
-- > stock["my-stock-item"] =
-- > {
-- >   { "itemltr.png", LTR },
-- >   { "itemrtl.png", RTL }
-- > }
--
-- You can specify custom icons for specific sizes, as follows: The sizes
-- that come with Gtk+ itself are @\"gtk-menu\"@, @\"gtk-small-toolbar\"@,
-- @\"gtk-large-toolbar\"@, @\"gtk-button\"@, @\"gtk-dialog\"@. Applications
-- can define other sizes.
--
-- > stock["my-stock-item"] =
-- > {
-- >   { "itemmenusize.png", *, *, "gtk-menu" },
-- >   { "itemtoolbarsize.png", *, *, "gtk-large-toolbar" }
-- >   { "itemgeneric.png" } /* implicit *, *, * as a fallback */
-- > }
--
-- It's also possible to use custom icons for a given state, for example:
--
-- > stock["my-stock-item"] =
-- > {
-- >   { "itemprelight.png", *, PRELIGHT },
-- >   { "iteminsensitive.png", *, INSENSITIVE },
-- >   { "itemgeneric.png" } /* implicit *, *, * as a fallback */
-- > }
--
-- When selecting an icon source to use, Gtk+ will consider text direction
-- most important, state second, and size third. It will select the best match
-- based on those criteria. If an attribute matches exactly (e.g. you specified
-- @PRELIGHT@ or specified the size), Gtk+ won't modify the image; if the
-- attribute matches with a wildcard, Gtk+ will scale or modify the image to
-- match the state and size the user requested.

-- ** Key bindings
--
-- | Key bindings allow the user to specify actions to be taken on particular
-- key presses. The form of a binding set declaration is:
--
-- key is a string consisting of a series of modifiers followed by the name
-- of a key. The modifiers can be:
--
-- * @\<alt>@
--
-- * @\<ctl>@
--
-- * @\<control>@
--
-- * @\<meta>@
--
-- * @\<hyper>@
--
-- * @\<super>@
--
-- * @\<mod1>@
--
-- * @\<mod2>@
--
-- * @\<mod3>@
--
-- * @\<mod4>@
--
-- * @\<mod5>@
--
-- * @\<release>@
--
-- * @\<shft>@
--
-- * @\<shift>@
--
-- @\<shft>@ is an alias for @\<shift>@, @\<ctl>@ is an alias for
-- @\<control>@, and @\<alt>@ is an alias for @\<mod1>@.
--
-- The action that is bound to the key is a sequence of signal names
-- (strings) followed by parameters for each signal. The signals must be action
-- signals. (See 'gSignalNew'). Each parameter can be a float, integer, string,
-- or unquoted string representing an enumeration value. The types of the
-- parameters specified must match the types of the parameters of the signal.
--
-- Binding sets are connected to widgets in the same manner as styles, with
-- one difference: Binding sets override other binding sets first by pattern
-- type, then by priority and then by order of specification. The priorities
-- that can be specified and their default values are the same as for styles.

-- * Class Hierarchy
--
-- |
-- @
-- |  'GObject'
-- |   +----RcStyle
-- @

-- * Types
  RcStyle,
  RcStyleClass,
  castToRcStyle, gTypeRcStyle,
  toRcStyle,

-- * Constructors
  rcStyleNew,

-- * Methods
  rcStyleCopy,
  rcAddDefaultFile,
  rcGetDefaultFiles,
  rcGetImModuleFile,
  rcGetModuleDir,
  rcGetStyle,
  rcGetStyleByPaths,
  rcGetThemeDir,
  rcParse,
  rcParseString,
  rcReparseAll,
  rcReparseAllForSettings,
  rcResetStyles,
  rcSetDefaultFiles,
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GType (GType)
{#import Graphics.UI.Gtk.Types#}


{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Creates a new 'RcStyle' with no fields set. The 'RcStyle' structure is
-- used to represent a set of information about the appearance of a widget.
-- This can later be composited together with other 'RcStyle' structures to
-- form a 'Style'.
--
rcStyleNew :: IO RcStyle
rcStyleNew =
  wrapNewGObject mkRcStyle $
  {# call gtk_rc_style_new #}

--------------------
-- Methods

-- | Makes a copy of the specified 'RcStyle'. This function will correctly
-- copy an RC style that is a member of a class derived from 'RcStyle'.
--
rcStyleCopy :: RcStyleClass self => self
 -> IO RcStyle -- ^ returns the resulting 'RcStyle'
rcStyleCopy self =
  wrapNewGObject mkRcStyle $
  {# call gtk_rc_style_copy #}
    (toRcStyle self)

-- | Adds a file to the list of files to be parsed at the end of 'initGUI'.
--
rcAddDefaultFile :: GlibString string => string -> IO ()
rcAddDefaultFile filename =
  withUTFString filename $ \filenamePtr ->
  {# call gtk_rc_add_default_file #}
    filenamePtr

-- | etrieves the current list of RC files that will be parsed at the end of
-- 'initGUI'.
--
rcGetDefaultFiles :: GlibString string => IO [string]
rcGetDefaultFiles = do
  aPtr <- {# call gtk_rc_get_default_files #}
  sPtrs <- peekArray0 nullPtr (castPtr aPtr)
  mapM peekUTFString sPtrs

-- | Obtains the path to the IM modules file. See the documentation of the
-- @GTK_IM_MODULE_FILE@ environment variable for more details.
--
rcGetImModuleFile :: GlibString string => IO string
rcGetImModuleFile =
  {# call gtk_rc_get_im_module_file #}
  >>= readUTFString

-- | Returns a directory in which GTK+ looks for theme engines.
--
rcGetModuleDir :: GlibString string => IO string
rcGetModuleDir =
  {# call gtk_rc_get_module_dir #}
  >>= readUTFString

-- | Finds all matching RC styles for a given widget, composites them
-- together, and then creates a GtkStyle representing the composite
-- appearance. (GTK+ actually keeps a cache of previously created styles, so a
-- new style may not be created.)
--
rcGetStyle :: WidgetClass widget => widget -> IO Style
rcGetStyle widget =
  makeNewGObject mkStyle $
  {# call gtk_rc_get_style #}
    (toWidget widget)

-- | Creates up a 'Style' from styles defined in a RC file by providing the
-- raw components used in matching. This function may be useful when creating
-- pseudo-widgets that should be themed like widgets but don't actually have
-- corresponding GTK+ widgets.
--
rcGetStyleByPaths :: GlibString string => Settings
  -> Maybe string
  -- ^ @widgetPath@ : the widget path to use when looking up the style, or
  -- @Nothing@ if no matching against the widget path should be done
  -> Maybe string
  -- ^ @classPath@ :  the class path to use when looking up the style, or
  -- @Nothing@ if no matching against the class path should be done.
  -> GType
  -- ^ @type@ : a type that will be used along with parent types of this type when
  -- matching against class styles, or 'none'
    -> IO Style
rcGetStyleByPaths settings mWidgetPath mClassPath type_ =
  makeNewGObject mkStyle $
  (case mClassPath of
    Just classPath -> withUTFString classPath
    Nothing -> (\act -> act nullPtr)) $ \classPathPtr ->
  (case mWidgetPath of
    Just widgetPath -> withUTFString widgetPath
    Nothing -> (\act -> act nullPtr)) $ \widgetPathPtr ->
  {# call gtk_rc_get_style_by_paths #}
    settings
    widgetPathPtr
    classPathPtr
    type_

-- | Returns the standard directory in which themes should be installed. (GTK+
-- does not actually use this directory itself.)
--
rcGetThemeDir :: GlibString string => IO string
rcGetThemeDir =
  {# call gtk_rc_get_theme_dir #}
  >>= readUTFString

-- | Parses a given resource file.
--
rcParse :: GlibString string => string
  -- ^ @filename@ : the @filename@ of a file to parse. If @filename@ is not
  -- absolute, it is searched in the current directory.
    -> IO ()
rcParse filename =
  withUTFString filename $ \filenamePtr ->
  {# call gtk_rc_parse #}
    filenamePtr

-- | Parses resource information directly from a string.
--
rcParseString :: GlibString string => string -> IO ()
rcParseString rcString =
  withUTFString rcString $ \rcStringPtr ->
  {# call gtk_rc_parse_string #}
    rcStringPtr

-- | If the modification time on any previously read file for the default
-- 'Settings' has changed, discard all style information and then reread all
-- previously read RC files.
--
rcReparseAll :: IO Bool -- ^ @True@ if the files were reread.
rcReparseAll =
  liftM toBool $
  {# call gtk_rc_reparse_all #}

-- | f the modification time on any previously read file for the given
-- 'Settings' has changed, discard all style information and then reread all
-- previously read RC files.
--
rcReparseAllForSettings :: Settings
  -> Bool -- ^ @forceLoad@ : load whether or not anything changed
  -> IO Bool  -- ^ @True@ if the files were reread.
rcReparseAllForSettings settings forceLoad =
  liftM toBool $
  {# call gtk_rc_reparse_all_for_settings #}
    (toSettings settings)
    (fromBool forceLoad)

-- | This function recomputes the styles for all widgets that use a particular
-- 'Settings' object. (There is one 'Settings' object per 'Screen', see
-- 'settingsGetForScreen'.) It is useful when some global parameter has
-- changed that affects the appearance of all widgets, because when a widget
-- gets a new style, it will both redraw and recompute any cached information
-- about its appearance. As an example, it is used when the default font size
-- set by the operating system changes. Note that this function doesn't affect
-- widgets that have a style set explicitely on them with 'widgetSetStyle'.
--
rcResetStyles :: Settings -> IO ()
rcResetStyles settings =
  {# call gtk_rc_reset_styles #}
    (toSettings settings)

-- | Sets the list of files that GTK+ will read at the end of 'initGUI'.
--
rcSetDefaultFiles :: GlibString string => [string] -> IO ()
rcSetDefaultFiles files =
  withUTFStringArray0 files $ \ssPtr ->
  {# call gtk_rc_set_default_files #} ssPtr
