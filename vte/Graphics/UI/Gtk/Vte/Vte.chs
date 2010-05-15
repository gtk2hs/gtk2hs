{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) widget for VTE
--
--  Author : Andy Stewart
--
--  Created: 20 Sep 2009
--
--  Copyright (C) 2009 Andy Stewart <lazycat.manatee@gmail.com>
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
-- A terminal widget
-- 
-----------------------------------------------------------------------------
-- 
module Graphics.UI.Gtk.Vte.Vte (
-- * Types
   Terminal,
   VteSelect,
   VteChar(..),

-- * Enums
   TerminalEraseBinding(..),
   TerminalCursorBlinkMode(..),
   TerminalCursorShape(..),
   RegexCompileFlags(..),
   RegexMatchFlags(..),

-- * Constructors
   terminalNew,

-- * Methods
   terminalImAppendMenuitems,
   terminalForkCommand,
   terminalForkpty,
   terminalSetPty,
   terminalGetPty,
   terminalFeed,
   terminalFeedChild,
   terminalFeedChildBinary,
   terminalGetChildExitStatus,
   terminalSelectAll,
   terminalSelectNone,
   terminalCopyClipboard,
   terminalPasteClipboard,
   terminalCopyPrimary,
   terminalPastePrimary,
   terminalSetSize,
   terminalSetAudibleBell,
   terminalGetAudibleBell,
   terminalSetVisibleBell,
   terminalGetVisibleBell,
   terminalSetAllowBold,
   terminalGetAllowBold,
   terminalSetScrollOnOutput,
   terminalSetScrollOnKeystroke,
   terminalSetColorBold,
   terminalSetColorForeground,
   terminalSetColorBackground,
   terminalSetColorDim,
   terminalSetColorCursor,
   terminalSetColorHighlight,
   terminalSetColors,
   terminalSetDefaultColors,
   terminalSetOpacity,
   terminalSetBackgroundImage,
   terminalSetBackgroundImageFile,
   terminalSetBackgroundSaturation,
   terminalSetBackgroundTransparent,
   terminalSetBackgroundTintColor,
   terminalSetScrollBackground,
   terminalSetCursorShape,
   terminalGetCursorShape,
   terminalSetCursorBlinkMode,
   terminalGetCursorBlinkMode,
   terminalSetScrollbackLines,
   terminalSetFont,
   terminalSetFontFromString,
   terminalGetFont,
   terminalGetHasSelection,
   terminalSetWordChars,
   terminalIsWordChar,
   terminalSetBackspaceBinding,
   terminalSetDeleteBinding,
   terminalSetMouseAutohide,
   terminalGetMouseAutohide,
   terminalReset,
   terminalGetText,
   terminalGetTextIncludeTrailingSpaces,
   terminalGetTextRange,
   terminalGetCursorPosition,
   terminalMatchClearAll,
   terminalMatchAddRegex,
   terminalMatchRemove,
   terminalMatchCheck,
   terminalMatchSetCursor,
   terminalMatchSetCursorType,
   terminalMatchSetCursorName,
   terminalSetEmulation,
   terminalGetEmulation,
   terminalGetDefaultEmulation,
   terminalSetEncoding,
   terminalGetEncoding,
   terminalGetStatusLine,
   terminalGetPadding,
   terminalGetAdjustment,
   terminalGetCharHeight,
   terminalGetCharWidth,
   terminalGetColumnCount,
   terminalGetRowCount,
   terminalGetIconTitle,
   terminalGetWindowTitle,

-- * Attributes
   terminalAllowBold,
   terminalAudibleBell,
   terminalBackgroundImageFile,
   terminalBackgroundImagePixbuf,
   terminalBackgroundOpacity,
   terminalBackgroundSaturation,
   terminalBackgroundTintColor,
   terminalBackgroundTransparent,
   terminalBackspaceBinding,
   terminalCursorBlinkMode,
   terminalCursorShape,
   terminalDeleteBinding,
   terminalEmulation,
   terminalEncoding,
   terminalFontDesc,
   terminalIconTitle,
   terminalPointerAutohide,
   terminalPty,
   terminalScrollBackground,
   terminalScrollOnKeystroke,
   terminalScrollOnOutput,
   terminalScrollbackLines,
   terminalVisibleBell,
   terminalWindowTitle,
   terminalWordChars,

-- * Signals
   beep,
   charSizeChanged,
   childExited,
   commit,
   contentsChanged,
   copyClipboard,
   cursorMoved,
   decreaseFontSize,
   deiconifyWindow,
   emulationChanged,
   encodingChanged,
   eof,
   iconTitleChanged,
   iconifyWindow,
   increaseFontSize,
   lowerWindow,
   maximizeWindow,
   moveWindow,
   pasteClipboard,
   raiseWindow,
   refreshWindow,
   resizeWidnow,
   restoreWindow,
   selectionChanged,
   setScrollAdjustments,
   statusLineChanged,
   textDeleted,
   textInserted,
   textModified,
   textScrolled,
   windowTitleChanged,
   ) where

import Control.Monad		(liftM, unless)
import Data.Char
import Data.Word

import System.Glib.Attributes
import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Properties
import System.Glib.GError
import System.Glib.Flags                        (Flags, fromFlags) 
import Graphics.UI.Gtk.Abstract.Widget (Color)
import Graphics.UI.Gtk.Gdk.Cursor
import Graphics.Rendering.Pango.BasicTypes (FontDescription(FontDescription), makeNewFontDescription)
import Graphics.UI.Gtk.Vte.Structs

{#import Graphics.UI.Gtk.General.Clipboard#}    (selectionPrimary,
                                                 selectionClipboard)
{#import Graphics.UI.Gtk.Abstract.Object#}	(makeNewObject)
{#import Graphics.UI.Gtk.Vte.Types#}
{#import Graphics.UI.Gtk.Vte.Signals#}
{#import System.Glib.GObject#}
{#import System.Glib.GError#}                   (propagateGError)

{#context lib= "vte" prefix= "vte"#}

--------------------
-- Types

-- | A predicate that states which characters are of interest. The predicate
--   @p c r@ where @p :: VteSelect@, should return @True@ if the character at
--   column @c@ and row @r@ should be extracted.
type VteSelect =
    Int
 -> Int 
 -> Bool

{#pointer SelectionFunc#}

-- | A structure describing the individual characters in the visible part of
--   a terminal window.
--
data VteChar = VteChar {
  vcRow :: Int,
  vcCol :: Int,
  vcChar :: Char,
  vcFore :: Color,
  vcBack :: Color,
  vcUnderline :: Bool,
  vcStrikethrough :: Bool
  }

--------------------
-- Utils

-- | Utils function to transform 'VteAttributes' to 'VteChar'.
attrToChar :: Char -> VteAttributes -> VteChar
attrToChar ch (VteAttributes r c f b u s) = VteChar r c ch f b u s
     
foreign import ccall "wrapper" mkVteSelectionFunc ::
  (Ptr Terminal -> {#type glong#} -> {#type glong#} -> Ptr () -> IO {#type gboolean#})
  -> IO SelectionFunc
  
--------------------
-- Enums

-- | Values for what should happen when the user presses backspace\/delete.  
-- Use 'EraseAuto' unless the user can cause them to be overridden.
{#enum VteTerminalEraseBinding as TerminalEraseBinding {underscoreToCase} deriving (Bounded,Eq,Show)#}

-- | Values for the cursor blink setting.
{#enum VteTerminalCursorBlinkMode as TerminalCursorBlinkMode {underscoreToCase} deriving (Bounded,Eq,Show)#}

-- | Values for the cursor shape setting.
{#enum VteTerminalCursorShape as TerminalCursorShape {underscoreToCase} deriving (Bounded,Eq,Show)#}

-- | Flags determining how the regular expression is to be interpreted. See
--   <http://library.gnome.org/devel/glib/stable/glib-Perl-compatible-regular-expressions.html#GRegexCompileFlags>
--   for an explanation of these flags.
--
{#enum GRegexCompileFlags as RegexCompileFlags {underscoreToCase} deriving (Bounded,Eq,Show) #}
instance Flags RegexCompileFlags

-- | Flags determining how the string is matched against the regular
--   expression. See
--   <http://library.gnome.org/devel/glib/stable/glib-Perl-compatible-regular-expressions.html#GRegexMatchFlags>
--   for an explanation of these flags.
--
{#enum GRegexMatchFlags as RegexMatchFlags {underscoreToCase} deriving (Bounded,Eq,Show) #}
instance Flags RegexMatchFlags

--------------------
-- Constructors
-- | Create a new terminal widget.
terminalNew :: IO Terminal
terminalNew = 
    makeNewObject mkTerminal $ liftM castPtr {#call terminal_new#}

--------------------
-- Methods
-- | Appends menu items for various input methods to the given menu. 
-- The user can select one of these items to modify the input method used by the terminal.
terminalImAppendMenuitems :: 
    TerminalClass self => self
 -> MenuShell -- ^ @menushell@ - a menu shell of 'MenuShell'
 -> IO () 
terminalImAppendMenuitems terminal menushell =
    {#call terminal_im_append_menuitems#} (toTerminal terminal) menushell

-- | Starts the specified command under a newly-allocated controlling pseudo-terminal. 
terminalForkCommand :: 
    TerminalClass self => self
 -> (Maybe String)  -- ^ @command@ - the name of a binary to run, or @Nothing@ to get user's shell                                       
 -> (Maybe [String])  -- ^ @argv@ - the argument list to be passed to command, or @Nothing@
 -> (Maybe [String])  -- ^ @envv@ - a list of environment variables to be added to the environment before starting command, or @Nothing@
 -> (Maybe String)  -- ^ @directory@ - the name of a directory the command should start in, or @Nothing@                                   
 -> Bool   -- ^ @lastlog@ - @True@ if the session should be logged to the lastlog
 -> Bool   -- ^ @utmp@ - @True@ if the session should be logged to the utmp/utmpx log                                     
 -> Bool   -- ^ @wtmp@ - @True@ if the session should be logged to the wtmp/wtmpx log                                     
 -> IO Int   -- ^ return the ID of the new process
terminalForkCommand terminal command argv envv directory lastlog utmp wtmp =
    liftM fromIntegral $
    maybeWith withUTFString command $ \commandPtr ->
    maybeWith withUTFString directory $ \dirPtr ->
    maybeWith withUTFStringArray argv $ \argvPtrPtr ->
    maybeWith withUTFStringArray envv $ \envvPtrPtr ->
      {#call terminal_fork_command#} 
      (toTerminal terminal) 
      commandPtr
      argvPtrPtr
      envvPtrPtr
      dirPtr
      (fromBool lastlog) 
      (fromBool utmp)
      (fromBool wtmp)

-- | Starts a new child process under a newly-allocated controlling pseudo-terminal. 
--
-- * Available since Vte version 0.11.11
-- 
terminalForkpty :: 
    TerminalClass self => self
 -> (Maybe [String])   -- ^ @envv@ - a list of environment variables to be added to the environment before starting returning in the child process, or @Nothing@
 -> (Maybe String)   -- ^ @directory@ - the name of a directory the child process should change to, or @Nothing@   
 -> Bool   -- ^ @lastlog@ - @True@ if the session should be logged to the lastlog                                                                   
 -> Bool   -- ^ @utmp@ - @True@ if the session should be logged to the utmp/utmpx log                                                            
 -> Bool   -- ^ @wtmp@ - @True@ if the session should be logged to the wtmp/wtmpx log                                                            
 -> IO Int   -- ^ return the ID of the new process in the parent, 0 in the child, and -1 if there was an error                                 
terminalForkpty terminal envv directory lastlog utmp wtmp =
    liftM fromIntegral $
    maybeWith withUTFString directory $ \dirPtr ->
    maybeWith withUTFStringArray envv $ \envvPtrPtr ->
        {#call terminal_forkpty#} 
        (toTerminal terminal)
        envvPtrPtr
        dirPtr
        (fromBool lastlog)
        (fromBool utmp)
        (fromBool wtmp)

-- | Attach an existing PTY master side to the terminal widget. 
-- Use instead of 'terminalForkCommand' or 'terminalForkpty'.
--
-- * Available since Vte version 0.12.1
--
terminalSetPty :: 
    TerminalClass self => self   
 -> Int   -- ^ @ptyMaster@ - a file descriptor of the master end of a PTY 
 -> IO ()
terminalSetPty terminal ptyMaster =
    {#call terminal_set_pty#} (toTerminal terminal) (fromIntegral ptyMaster)

-- | Returns the file descriptor of the master end of terminal's PTY.
--
-- * Available since Vte version 0.19.1
--
terminalGetPty ::
    TerminalClass self => self
 -> IO Int   -- ^ return the file descriptor, or -1 if the terminal has no PTY. 
terminalGetPty terminal = 
    liftM fromIntegral $ 
    {#call terminal_get_pty#} (toTerminal terminal)

-- | Interprets data as if it were data received from a child process. This
-- can either be used to drive the terminal without a child process, or just
-- to mess with your users.
terminalFeed :: 
    TerminalClass self => self   
 -> String   -- ^ @string@ - a string in the terminal's current encoding 
 -> IO ()
terminalFeed terminal string =
    withUTFStringLen string $ \(strPtr, len) ->
    {#call terminal_feed#} (toTerminal terminal) strPtr (fromIntegral len)

-- | Sends a block of UTF-8 text to the child as if it were entered by the
-- user at the keyboard.
terminalFeedChild :: 
    TerminalClass self => self   
 -> String   -- ^ @string@ - data to send to the child                                
 -> IO ()
terminalFeedChild terminal string =
    withUTFStringLen string $ \(strPtr, len) ->
    {#call terminal_feed_child#} (toTerminal terminal) strPtr (fromIntegral len)

-- | Sends a block of binary data to the child.
--
-- * Available since Vte version 0.12.1
--
terminalFeedChildBinary :: 
    TerminalClass self => self   
 -> [Word8]   -- ^ @data@ - data to send to the child 
 -> IO ()
terminalFeedChildBinary terminal string =
    withArrayLen string $ \len strPtr ->
    {#call terminal_feed_child_binary#} (toTerminal terminal) (castPtr strPtr) (fromIntegral len)
    
-- | Gets the exit status of the command started by 'terminalForkCommand'. 
--
-- * Available since Vte version 0.19.1
--
terminalGetChildExitStatus ::
    TerminalClass self => self    
 -> IO Int   -- ^ return the child's exit status 
terminalGetChildExitStatus terminal =
    liftM fromIntegral $
    {#call terminal_get_child_exit_status#} (toTerminal terminal)

-- | Selects all text within the terminal (including the scrollback buffer).
--
-- * Available since Vte version 0.16
--
terminalSelectAll :: TerminalClass self => self -> IO ()    
terminalSelectAll terminal =
    {#call terminal_select_all#} (toTerminal terminal)
    
-- | Clears the current selection.
--
-- * Available since Vte version 0.16
--
terminalSelectNone :: TerminalClass self => self -> IO ()    
terminalSelectNone terminal =
    {#call terminal_select_none#} (toTerminal terminal)

-- | Places the selected text in the terminal in the 'selectionClipboard'
-- selection.
terminalCopyClipboard :: TerminalClass self => self -> IO ()
terminalCopyClipboard terminal = 
    {#call terminal_copy_clipboard#} (toTerminal terminal)
  
-- | Sends the contents of the 'selectionClipboard' selection to the
-- terminal's child. If necessary, the data is converted from UTF-8 to the
-- terminal's current encoding. It's called on paste menu item, or when user
-- presses Shift+Insert.
terminalPasteClipboard :: TerminalClass self => self -> IO ()
terminalPasteClipboard terminal =
    {#call terminal_paste_clipboard#} (toTerminal terminal)

-- | Places the selected text in the terminal in the
-- 'selectionPrimary' selection.
terminalCopyPrimary :: TerminalClass self => self -> IO ()
terminalCopyPrimary terminal =
    {#call terminal_copy_primary#} (toTerminal terminal)

-- | Sends the contents of the
-- 'selectionPrimary' selection to the
-- terminal's child. If necessary, the data is converted from UTF-8 to the
-- terminal's current encoding. The terminal will call also paste the
-- 'SelectionPrimary' selection when the user clicks with the the second mouse
-- button.
terminalPastePrimary :: TerminalClass self => self -> IO ()
terminalPastePrimary terminal =
    {#call terminal_paste_primary#} (toTerminal terminal)
  
-- | Attempts to change the terminal's size in terms of rows and columns. 
-- If the attempt succeeds, the widget will resize itself to the proper size.
terminalSetSize :: 
    TerminalClass self => self   
 -> Int   -- ^ @columns@ - the desired number of columns 
 -> Int   -- ^ @rows@ - the desired number of rows    
 -> IO ()
terminalSetSize terminal columns rows =
    {#call terminal_set_size#} 
    (toTerminal terminal)
    (fromIntegral columns)
    (fromIntegral rows)
    
-- | Controls whether or not the terminal will beep when the child outputs the \"bl\" sequence.
terminalSetAudibleBell :: 
    TerminalClass self => self   
 -> Bool   -- ^ @isAudible@ - @True@ if the terminal should beep 
 -> IO ()    
terminalSetAudibleBell terminal isAudible =
    {#call terminal_set_audible_bell#} (toTerminal terminal) (fromBool isAudible)
    
-- | Checks whether or not the terminal will beep when the child outputs the \"bl\" sequence.
terminalGetAudibleBell :: 
    TerminalClass self => self   
 -> IO Bool   -- ^ return @True@ if audible bell is enabled, @False@ if not 
terminalGetAudibleBell terminal =
    liftM toBool $
    {#call terminal_get_audible_bell#} (toTerminal terminal)
    
-- | Controls whether or not the terminal will present a visible bell to the user when the child outputs the \"bl\" sequence. 
-- The terminal will clear itself to the default foreground color and then repaint itself.
terminalSetVisibleBell :: 
    TerminalClass self => self   
 -> Bool   -- ^ @isVisible@ - @True@ if the terminal should flash 
 -> IO ()    
terminalSetVisibleBell terminal isVisible =
    {#call terminal_set_visible_bell#} (toTerminal terminal) (fromBool isVisible)
    
-- | Checks whether or not the terminal will present a visible bell to the user when the child outputs the \"bl\" sequence. 
-- The terminal will clear itself to the default foreground color and then repaint itself.
terminalGetVisibleBell :: 
    TerminalClass self => self   
 -> IO Bool   -- ^ return @True@ if visible bell is enabled, @False@ if not 
terminalGetVisibleBell terminal =
    liftM toBool $
    {#call terminal_get_visible_bell#} (toTerminal terminal)
    
-- | Controls whether or not the terminal will attempt to draw bold text, 
-- either by using a bold font variant or by repainting text with a different offset.
terminalSetAllowBold :: 
    TerminalClass self => self   
 -> Bool   -- ^ @allowBold@ - @True@ if the terminal should attempt to draw bold text 
 -> IO ()    
terminalSetAllowBold terminal allowBold =
    {#call terminal_set_allow_bold#} (toTerminal terminal) (fromBool allowBold)
    
-- | Checks whether or not the terminal will attempt to draw bold text by repainting text with a one-pixel offset.
terminalGetAllowBold :: 
    TerminalClass self => self    
 -> IO Bool   -- ^ return @True@ if bolding is enabled, @False@ if not 
terminalGetAllowBold terminal =
    liftM toBool $
    {#call  terminal_get_allow_bold#} (toTerminal terminal)
    
-- | Controls whether or not the terminal will forcibly scroll to the bottom of the viewable history when the new data is received from the child.
terminalSetScrollOnOutput :: 
    TerminalClass self => self   
 -> Bool   -- ^ @scroll@ - @True@ if the terminal should scroll on output 
 -> IO ()    
terminalSetScrollOnOutput terminal scroll =
    {#call terminal_set_scroll_on_output#} (toTerminal terminal) (fromBool scroll)
    
-- | Controls whether or not the terminal will forcibly scroll to the bottom of the viewable history when the user presses a key. 
-- Modifier keys do not trigger this behavior.
terminalSetScrollOnKeystroke :: 
    TerminalClass self => self   
 -> Bool   -- ^ @scroll@ - @True@ if the terminal should scroll on keystrokes 
 -> IO ()
terminalSetScrollOnKeystroke terminal scroll =
    {#call terminal_set_scroll_on_keystroke#} (toTerminal terminal) (fromBool scroll)
  
-- | Sets the color used to draw bold text in the default foreground color.
terminalSetColorBold :: 
    TerminalClass self => self   
 -> Color   -- ^ @bold@ - the new bold color 
 -> IO ()
terminalSetColorBold terminal bold =
    with bold $ \boldPtr ->
    {#call terminal_set_color_bold#} (toTerminal terminal) (castPtr boldPtr)

-- | Sets the foreground color used to draw normal text
terminalSetColorForeground :: 
    TerminalClass self => self    
 -> Color   -- ^ @foreground@ - the new foreground color 
 -> IO ()
terminalSetColorForeground terminal foreground =
    with foreground $ \fgPtr -> 
    {#call terminal_set_color_foreground#} (toTerminal terminal) (castPtr fgPtr)

-- | Sets the background color for text which does not have a specific background color assigned. 
-- Only has effect when no background image is set and when the terminal is not transparent.
terminalSetColorBackground :: 
    TerminalClass self => self   
 -> Color   -- ^ @background@ - the new background color 
 -> IO ()
terminalSetColorBackground terminal background =
    with background $ \bgPtr ->
    {#call terminal_set_color_background#} (toTerminal terminal) (castPtr bgPtr)

-- | Sets the color used to draw dim text in the default foreground color.
terminalSetColorDim :: 
    TerminalClass self => self   
 -> Color   -- ^ @dim@ - the nw dim color
 -> IO ()
terminalSetColorDim terminal dim =
    with dim $ \dimPtr -> 
    {#call terminal_set_color_dim#} (toTerminal terminal) (castPtr dimPtr)

-- | Sets the background color for text which is under the cursor. 
-- If @Nothing@, text under the cursor will be drawn with foreground and background colors reversed.
--
-- * Available since Vte version 0.11.11
--
terminalSetColorCursor :: 
    TerminalClass self => self   
 -> Color   -- ^ @cursor@ - the new color to use for the text cursor 
 -> IO ()
terminalSetColorCursor terminal cursor =
    with cursor $ \cursorPtr -> 
    {#call terminal_set_color_cursor#} (toTerminal terminal) (castPtr cursorPtr)

-- | Sets the background color for text which is highlighted. 
-- If @Nothing@, highlighted text (which is usually highlighted because it is selected) will be drawn with foreground and background colors reversed.
--
-- * Available since Vte version 0.11.11
--
terminalSetColorHighlight :: 
    TerminalClass self => self   
 -> Color   -- ^ @highlight@ - the new color to use for highlighted text 
 -> IO ()
terminalSetColorHighlight terminal highlight =
    with highlight $ \hlPtr -> 
    {#call terminal_set_color_highlight#} (toTerminal terminal) (castPtr hlPtr)

-- | The terminal widget uses a 28-color model comprised of the default foreground and background colors, 
-- the bold foreground color, the dim foreground color, an eight color palette, 
-- bold versions of the eight color palette, and a dim version of the the eight color palette. palette_size must be either 0, 8, 16, or 24. 
-- If foreground is @Nothing@ and palette_size is greater than 0, the new foreground color is taken from palette[7]. 
-- If background is @Nothing@ and palette_size is greater than 0, the new background color is taken from palette[0]. 
-- If palette_size is 8 or 16, the third (dim) and possibly the second (bold) 8-color palettes are extrapolated from the new background color and the items in palette.
terminalSetColors :: 
    TerminalClass self => self   
 -> Color   -- ^ @foreground@ - the new foreground color, or @Nothing@ 
 -> Color   -- ^ @background@ - the new background color, or @Nothing@ 
 -> Color   -- ^ @palette@ - the color palette                 
 -> Int   -- ^ @size@ - the number of entries in palette  
 -> IO ()
terminalSetColors terminal foreground background palette size =
    with foreground $ \fPtr ->
    with background $ \bPtr ->
    with palette $ \pPtr ->
    {#call terminal_set_colors#} 
    (toTerminal terminal) 
    (castPtr fPtr)
    (castPtr bPtr)
    (castPtr pPtr)
    (fromIntegral size)

-- | Reset the terminal palette to reasonable compiled-in defaults.
terminalSetDefaultColors :: TerminalClass self => self -> IO ()
terminalSetDefaultColors terminal =
    {#call terminal_set_default_colors#} (toTerminal terminal)
    
-- | Sets the opacity of the terminal background, were 0 means completely transparent and 65535 means completely opaque.
terminalSetOpacity :: 
    TerminalClass self => self
 -> Int   -- ^ @opacity@ - the new opacity 
 -> IO ()
terminalSetOpacity terminal opacity =
    {#call terminal_set_opacity#} (toTerminal terminal) (fromIntegral opacity)

-- | Sets a background image for the widget. 
-- Text which would otherwise be drawn using the default background color will instead be drawn over the specified image. 
-- If necessary, the image will be tiled to cover the widget's entire visible area. 
-- If specified by 'terminalSetBackgroundSaturation' the terminal will tint its in-memory copy of the image before applying it to the terminal.
terminalSetBackgroundImage :: 
    TerminalClass self => self
 -> Maybe Pixbuf   -- ^ @image@ - a 'Pixbuf' to use, or @Nothing@ to use the default background
 -> IO ()
terminalSetBackgroundImage terminal (Just image) =
    {#call terminal_set_background_image#} (toTerminal terminal) image
terminalSetBackgroundImage terminal Nothing =
    {#call terminal_set_background_image#} (toTerminal terminal) (Pixbuf nullForeignPtr)
    
-- | Sets a background image for the widget. 
-- If specified by 'terminalSetBackgroundSaturation', the terminal will tint its in-memory copy of the image before applying it to the terminal.
terminalSetBackgroundImageFile :: 
    TerminalClass self => self
 -> String   -- ^ @path@ - path to an image file 
 -> IO ()
terminalSetBackgroundImageFile terminal path =
    withUTFString path $ \pathPtr ->
    {#call terminal_set_background_image_file#} (toTerminal terminal) pathPtr
    
-- | If a background image has been set using 'terminalSetBackgroundImage', 'terminalSetBackgroundImageFile', or 'terminalSetBackgroundTransparent', 
-- and the saturation value is less than 1.0, the terminal will adjust the colors of the image before drawing the image. 
-- To do so, the terminal will create a copy of the background image (or snapshot of the root window) and modify its pixel values.
terminalSetBackgroundSaturation :: 
    TerminalClass self => self
 -> Double   -- ^ @saturation@ - a floating point value between 0.0 and 1.0. 
 -> IO ()    
terminalSetBackgroundSaturation terminal saturation =
    {#call terminal_set_background_saturation#} (toTerminal terminal) (realToFrac saturation)
    
-- | Sets the terminal's background image to the pixmap stored in the root window, adjusted so that if there are no windows below your application, 
-- the widget will appear to be transparent.
terminalSetBackgroundTransparent :: 
    TerminalClass self => self
 -> Bool   -- ^ @transparent@ - @True@ if the terminal should fake transparency 
 -> IO ()
terminalSetBackgroundTransparent terminal transparent =
    {#call terminal_set_background_transparent#} (toTerminal terminal) (fromBool transparent)
    
-- | If a background image has been set using 'terminalSetBackgroundImage', 'terminalSetBackgroundImageFile', or 'terminalSetBackgroundTransparent', 
-- and the value set by 'terminalSetBackgroundSaturation' is less than one, the terminal will adjust the color of the image before drawing the image. 
-- To do so, the terminal will create a copy of the background image (or snapshot of the root window) and modify its pixel values. 
-- The initial tint color is black.
--
-- * Available since Vte version 0.11
--
terminalSetBackgroundTintColor :: 
    TerminalClass self => self
 -> Color   -- ^ @color@ - a color which the terminal background should be tinted to if its saturation is not 1.0. 
 -> IO ()
terminalSetBackgroundTintColor terminal color =
    with color $ \cPtr ->
    {#call terminal_set_background_tint_color#} (toTerminal terminal) (castPtr cPtr)

-- | Controls whether or not the terminal will scroll the background image (if one is set) when the text in the window must be scrolled.
--
-- * Available since Vte version 0.11
--
terminalSetScrollBackground :: 
    TerminalClass self => self
 -> Bool   -- ^ @scroll@ - @True@ if the terminal should scroll the background image along with text. 
 -> IO ()
terminalSetScrollBackground terminal scroll =
    {#call terminal_set_scroll_background#} (toTerminal terminal) (fromBool scroll)
    
-- | Sets the shape of the cursor drawn.
--
-- * Available since Vte version 0.19.1
--
terminalSetCursorShape ::
    TerminalClass self => self 
 -> TerminalCursorShape   -- ^ @shape@ - the 'TerminalCursorShape' to use 
 -> IO ()
terminalSetCursorShape terminal shape = 
    {#call terminal_set_cursor_shape#} (toTerminal terminal) $fromIntegral (fromEnum shape)

-- | Returns the currently set cursor shape.
--
-- * Available since Vte version 0.17.6
--
terminalGetCursorShape ::
    TerminalClass self => self
 -> IO TerminalCursorShape   -- ^ return cursor shape
terminalGetCursorShape terminal = 
    liftM (toEnum.fromIntegral) $
    {#call terminal_get_cursor_shape#} (toTerminal terminal)

-- | Returns the currently set cursor blink mode.
--
-- * Available since Vte version 0.17.1
--
terminalGetCursorBlinkMode :: 
    TerminalClass self => self
 -> IO TerminalCursorBlinkMode   -- ^ return cursor blink mode. 
terminalGetCursorBlinkMode terminal = 
    liftM (toEnum.fromIntegral) $
    {#call terminal_get_cursor_blink_mode#} (toTerminal terminal)

-- | Sets whether or not the cursor will blink. 
--
-- * Available since Vte version 0.17.1
--
terminalSetCursorBlinkMode :: 
    TerminalClass self => self
 -> TerminalCursorBlinkMode   -- ^ @mode@ - the 'TerminalCursorBlinkMode' to use 
 -> IO ()
terminalSetCursorBlinkMode terminal mode =
    {#call terminal_set_cursor_blink_mode#} (toTerminal terminal) $fromIntegral (fromEnum mode)

-- | Sets the length of the scrollback buffer used by the terminal. 
-- The size of the scrollback buffer will be set to the larger of this value and the number of visible rows the widget can display, 
-- so 0 can safely be used to disable scrollback. 
-- Note that this setting only affects the normal screen buffer. 
-- For terminal types which have an alternate screen buffer, no scrollback is allowed on the alternate screen buffer.
terminalSetScrollbackLines :: 
    TerminalClass self => self
 -> Int   -- ^ @lines@ - the length of the history buffer 
 -> IO ()
terminalSetScrollbackLines terminal lines =
    {#call terminal_set_scrollback_lines#} (toTerminal terminal) (fromIntegral lines)

-- | Sets the font used for rendering all text displayed by the terminal, overriding any fonts set using 'widgetModifyFont'.
-- The terminal will immediately attempt to load the desired font, retrieve its metrics, and attempt to resize itself to keep the same number of rows and columns.
terminalSetFont :: 
    TerminalClass self => self
 -> FontDescription   -- ^ @fontDesc@ - the 'FontDescription' of the desired font. 
 -> IO ()
terminalSetFont terminal (FontDescription fontDesc) =
    {#call terminal_set_font#} (toTerminal terminal) (castPtr $ unsafeForeignPtrToPtr fontDesc)

-- | A convenience function which converts name into a 'FontDescription' and passes it to 'terminalSetFont'.
terminalSetFontFromString :: 
    TerminalClass self => self
 -> String   -- ^ @name@ - a string describing the font. 
 -> IO ()
terminalSetFontFromString terminal name =
    withUTFString name $ \namePtr -> 
    {#call terminal_set_font_from_string#} (toTerminal terminal) namePtr
    
-- | Queries the terminal for information about the fonts which will be used to draw text in the terminal.
terminalGetFont :: 
    TerminalClass self => self
 -> IO FontDescription   -- ^ return a 'FontDescription' describing the font the terminal is currently using to render text. 
terminalGetFont terminal = do
    fdPtr <- {#call unsafe terminal_get_font#} (toTerminal terminal) 
    makeNewFontDescription (castPtr fdPtr) 

-- | Checks if the terminal currently contains selected text. 
-- Note that this is different from determining if the terminal is the owner of any 'GtkClipboard' items.
terminalGetHasSelection :: 
    TerminalClass self => self
 -> IO Bool   -- ^ return @True@ if part of the text in the terminal is selected. 
terminalGetHasSelection terminal =
    liftM toBool $
    {#call terminal_get_has_selection#} (toTerminal terminal)
    
-- | When the user double-clicks to start selection, the terminal will extend the selection on word boundaries. 
-- It will treat characters included in spec as parts of words, and all other characters as word separators. 
-- Ranges of characters can be specified by separating them with a hyphen.
-- As a special case, if @spec@ is the empty string, the terminal will treat all graphic non-punctuation non-space characters as word characters.
terminalSetWordChars :: 
    TerminalClass self => self
 -> String   -- ^ @spec@ - a specification 
 -> IO ()    
terminalSetWordChars terminal spec =
    withUTFString spec $ \specPtr ->
    {#call terminal_set_word_chars#} (toTerminal terminal) specPtr
    
-- | Checks if a particular character is considered to be part of a word or not, based on the values last passed to 'terminalSetWordChars'.
terminalIsWordChar :: 
    TerminalClass self => self
 -> Char   -- ^ @c@ - a candidate Unicode code point                           
 -> IO Bool   -- ^ return @True@ if the character is considered to be part of a word 
terminalIsWordChar terminal c =
    liftM toBool $
    {#call terminal_is_word_char#} (toTerminal terminal) (fromIntegral $ ord c)

-- | Modifies the terminal's backspace key binding, 
-- which controls what string or control sequence the terminal sends to its child when the user presses the backspace key.
terminalSetBackspaceBinding :: 
    TerminalClass self => self
 -> TerminalEraseBinding   -- ^ @binding@ - a 'TerminalEraseBinding' for the backspace key 
 -> IO ()
terminalSetBackspaceBinding terminal binding =
    {#call terminal_set_backspace_binding#} (toTerminal terminal) (fromIntegral (fromEnum binding))

-- | Modifies the terminal's delete key binding, 
-- which controls what string or control sequence the terminal sends to its child when the user presses the delete key.
terminalSetDeleteBinding :: 
    TerminalClass self => self
 -> TerminalEraseBinding   -- ^ @bindign@ - a 'TerminalEraseBinding' for the delete key 
 -> IO ()
terminalSetDeleteBinding terminal binding =
    {#call terminal_set_delete_binding#} (toTerminal terminal) (fromIntegral (fromEnum binding))
 
-- | Changes the value of the terminal's mouse autohide setting. 
-- When autohiding is enabled, the mouse cursor will be hidden when the user presses a key and shown when the user moves the mouse. 
-- This setting can be read using 'terminalGetMouseAutohide'.
terminalSetMouseAutohide :: 
    TerminalClass self => self
 -> Bool   -- ^ @autohide@ - @True@ if the autohide should be enabled 
 -> IO ()
terminalSetMouseAutohide terminal autohide =
    {#call terminal_set_mouse_autohide#} (toTerminal terminal) (fromBool autohide)
    
-- | Determines the value of the terminal's mouse autohide setting. 
-- When autohiding is enabled, the mouse cursor will be hidden when the user presses a key and shown when the user moves the mouse. 
-- This setting can be changed using 'terminalSetMouseAutohide'.
terminalGetMouseAutohide :: TerminalClass self => self -> IO Bool
terminalGetMouseAutohide terminal =
    liftM toBool $
    {#call terminal_get_mouse_autohide#} (toTerminal terminal)
    
-- | Resets as much of the terminal's internal state as possible, discarding any unprocessed input data, 
-- resetting character attributes, cursor state, national character set state, status line, 
-- terminal modes (insert/delete), selection state, and encoding.
terminalReset :: 
    TerminalClass self => self
 -> Bool   -- ^ @full@ - @True@ to reset tabstops                         
 -> Bool   -- ^ @clearHistory@ - @True@ to empty the terminal's scrollback buffer 
 -> IO ()
terminalReset terminal full clearHistory =
    {#call terminal_reset#} (toTerminal terminal) (fromBool full) (fromBool clearHistory)

-- | Extracts a view of the visible part of the terminal. A selection
--   predicate may be supplied to restrict the inspected characters. The
--   return value is a list of 'VteChar' structures, each detailing the
--   character's position, colors, and other characteristics.
--
terminalGetText ::
    TerminalClass self => self
 -> Maybe VteSelect -- ^ @Just p@ for a predicate @p@ that determines
                    -- which character should be extracted or @Nothing@
                    -- to select all characters
 -> IO [VteChar] -- ^ return a text string
terminalGetText terminal mCB = do
  cbPtr <- case mCB of
    Just cb -> mkVteSelectionFunc $ \_ c r _ ->
      return (fromBool (cb (fromIntegral c) (fromIntegral r)))
    Nothing -> return nullFunPtr
  gArrPtr <- {#call unsafe g_array_new#} 0 0
    (fromIntegral (sizeOf (undefined :: VteAttributes)))
  strPtr <- {#call terminal_get_text #} (toTerminal terminal) cbPtr nullPtr gArrPtr
  str <- if strPtr==nullPtr then return "" else peekUTFString strPtr
  (len,elemPtr) <- gArrayContent (castPtr gArrPtr)
  attrs <- (flip mapM) [0..len-1] $ peekElemOff elemPtr
  unless (cbPtr==nullFunPtr) $ freeHaskellFunPtr cbPtr
  {#call unsafe g_free#} (castPtr strPtr)
  {#call unsafe g_array_free#} gArrPtr 1
  return (zipWith attrToChar str attrs)
  
-- | Extracts a view of the visible part of the terminal. 
-- If is_selected is not @Nothing@, characters will only be read if is_selected returns @True@ after being passed the column and row, respectively. 
-- A 'CharAttributes' structure is added to attributes for each byte added to the returned string detailing the character's position, colors, and other characteristics. 
-- This function differs from 'terminalGetText' in that trailing spaces at the end of lines are included.
--
-- * Available since Vte version 0.11.11
--
terminalGetTextIncludeTrailingSpaces :: 
    TerminalClass self => self
 -> Maybe VteSelect -- ^ @Just p@ for a predicate @p@ that determines
                    -- which character should be extracted or @Nothing@
                    -- to select all characters    
 -> IO [VteChar] -- ^ return a text string
terminalGetTextIncludeTrailingSpaces terminal mCB = do
  cbPtr <- case mCB of
    Just cb -> mkVteSelectionFunc $ \_ c r _ ->
      return (fromBool (cb (fromIntegral c) (fromIntegral r)))
    Nothing -> return nullFunPtr
  gArrPtr <- {#call unsafe g_array_new#} 0 0
    (fromIntegral (sizeOf (undefined :: VteAttributes)))
  strPtr <- {#call terminal_get_text_include_trailing_spaces #} (toTerminal terminal) cbPtr nullPtr gArrPtr
  str <- if strPtr==nullPtr then return "" else peekUTFString strPtr
  (len,elemPtr) <- gArrayContent (castPtr gArrPtr)
  attrs <- (flip mapM) [0..len-1] $ peekElemOff elemPtr
  unless (cbPtr==nullFunPtr) $ freeHaskellFunPtr cbPtr
  {#call unsafe g_free#} (castPtr strPtr)
  {#call unsafe g_array_free#} gArrPtr 1
  return (zipWith attrToChar str attrs)

-- | Extracts a view of the visible part of the terminal. 
-- If is_selected is not @Nothing@, characters will only be read if is_selected returns @True@ after being passed the column and row, respectively. 
-- A 'CharAttributes' structure is added to attributes for each byte added to the returned string detailing the character's position, colors, and other characteristics. 
-- The entire scrollback buffer is scanned, so it is possible to read the entire contents of the buffer using this function.
--
terminalGetTextRange ::
    TerminalClass self => self
 -> Int   -- ^ @sRow@ first row to search for data                              
 -> Int   -- ^ @sCol@ first column to search for data                           
 -> Int   -- ^ @eRow@ last row to search for data                               
 -> Int   -- ^ @eCol@ last column to search for data                            
 -> Maybe VteSelect -- ^ @Just p@ for a predicate @p@ that determines
                    -- which character should be extracted or @Nothing@
                    -- to select all characters
 -> IO [VteChar] -- ^ return a text string
terminalGetTextRange terminal sRow sCol eRow eCol mCB = do 
  cbPtr <- case mCB of
    Just cb -> mkVteSelectionFunc $ \_ c r _ ->
      return (fromBool (cb (fromIntegral c) (fromIntegral r)))
    Nothing -> return nullFunPtr
  gArrPtr <- {#call unsafe g_array_new#} 0 0
    (fromIntegral (sizeOf (undefined :: VteAttributes)))
  strPtr <- {#call terminal_get_text_range #} (toTerminal terminal) (fromIntegral sRow) (fromIntegral sCol) (fromIntegral eRow) (fromIntegral eCol) cbPtr nullPtr gArrPtr
  str <- if strPtr==nullPtr then return "" else peekUTFString strPtr
  (len,elemPtr) <- gArrayContent (castPtr gArrPtr)
  attrs <- (flip mapM) [0..len-1] $ peekElemOff elemPtr
  unless (cbPtr==nullFunPtr) $ freeHaskellFunPtr cbPtr
  {#call unsafe g_free#} (castPtr strPtr)
  {#call unsafe g_array_free#} gArrPtr 1
  return (zipWith attrToChar str attrs)

-- | Reads the location of the insertion cursor and returns it. The row
-- coordinate is absolute.
terminalGetCursorPosition :: 
    TerminalClass self => self
 -> IO (Int, Int)  -- ^ @(column,row)@ the position of the cursor
terminalGetCursorPosition terminal = do
    alloca $ \cPtr ->
        alloca $ \rPtr -> do
            {#call  terminal_get_cursor_position#} (toTerminal terminal) cPtr rPtr
            column <- peek cPtr
            row <- peek rPtr
            return (fromIntegral column,fromIntegral row)

-- | Clears the list of regular expressions the terminal uses to highlight
-- text when the user moves the mouse cursor.
terminalMatchClearAll :: TerminalClass self => self -> IO ()
terminalMatchClearAll terminal =
    {#call terminal_match_clear_all#} (toTerminal terminal)
    
-- | Adds the regular expression to the list of matching expressions.
-- When the user moves the mouse cursor over a section of displayed text which
-- matches this expression, the text will be highlighted.
--
-- See <http://library.gnome.org/devel/glib/stable/glib-regex-syntax.html> for
-- details about the accepted syntex.
--
-- * Available since Vte version 0.17.1
--
terminalMatchAddRegex ::
    TerminalClass self => self
 -> String -- ^ @pattern@ - a regular expression
 -> [RegexCompileFlags] -- ^ @flags@ - specify how to interpret the pattern
 -> [RegexMatchFlags] -- ^ @flags@ - specify how to match
 -> IO Int -- ^ return an integer associated with this expression
terminalMatchAddRegex terminal pattern cFlags mFlags =
  withUTFString pattern $ \pat -> do
    regexPtr <- propagateGError $
      {#call g_regex_new#} pat  (fromIntegral (fromFlags cFlags))
        (fromIntegral (fromFlags mFlags))
    liftM fromIntegral $ {#call terminal_match_add_gregex#}
      (toTerminal terminal) regexPtr (fromIntegral (fromFlags mFlags))

-- | Removes the regular expression which is associated with the given tag from the list of expressions which the terminal will highlight when the user moves the mouse cursor over matching text.
terminalMatchRemove :: 
    TerminalClass self => self
 -> Int   -- ^ @tag@ - the tag of the regex to remove 
 -> IO ()
terminalMatchRemove terminal tag =
    {#call terminal_match_remove#} (toTerminal terminal) (fromIntegral tag)

-- | Checks if the text in and around the specified position matches any of
-- the regular expressions previously registered using
-- 'terminalMatchAddRegex'. If a match exists, the matching string is returned
-- together with the number associated with the matched regular expression. If
-- more than one regular expression matches, the expressions that was
-- registered first will be returned.
--
terminalMatchCheck :: 
    TerminalClass self => self
 -> Int   -- ^ @column@ - the text column                                                                                             
 -> Int   -- ^ @row@ - the text row                                                                                                
 -> IO (Maybe (String, Int))
 -- ^ @Just (str, tag)@ - the string that matched one of the previously set
 -- regular expressions together with the number @tag@ that was returned by
 -- 'terminalMatchAddRegex'
terminalMatchCheck terminal column row = alloca $ \tagPtr -> do
  strPtr <- {#call terminal_match_check#} (toTerminal terminal)
    (fromIntegral column) (fromIntegral row) tagPtr
  if strPtr==nullPtr then return Nothing else do
  str <- peekCString strPtr
  {#call unsafe g_free#} (castPtr strPtr)
  if tagPtr==nullPtr then return Nothing else do
  tag <- peek tagPtr
  return (Just (str,fromIntegral tag))

-- | Sets which cursor the terminal will use if the pointer is over the pattern specified by tag. 
-- The terminal keeps a reference to cursor.
--
-- * Available since Vte version 0.11
--
terminalMatchSetCursor :: 
    TerminalClass self => self
 -> Int   -- ^ @tag@ - the tag of the regex which should use the specified cursor                  
 -> Cursor   -- ^ @cursor@ - the 'Cursor' which the terminal should use when the pattern is highlighted 
 -> IO ()
terminalMatchSetCursor terminal tag (Cursor cur) =
    with (unsafeForeignPtrToPtr cur) $ \curPtr ->
    {#call terminal_match_set_cursor#} (toTerminal terminal) (fromIntegral tag) (castPtr curPtr)

-- | Sets which cursor the terminal will use if the pointer is over the pattern specified by tag.
--
-- * Available since Vte version 0.11.9
--
terminalMatchSetCursorType :: 
    TerminalClass self => self
 -> Int    -- ^ @tag@ the tag of the regex which should use the specified cursor 
 -> CursorType -- ^ @cursorType@ a 'CursorType'
 -> IO ()
terminalMatchSetCursorType terminal tag cursorType = 
    {#call terminal_match_set_cursor_type#} (toTerminal terminal) (fromIntegral tag) $fromIntegral (fromEnum cursorType) 

-- | Sets which cursor the terminal will use if the pointer is over the pattern specified by tag.
--
-- * Available since Vte version 0.17.1
--
terminalMatchSetCursorName :: 
    TerminalClass self => self
 -> Int   -- ^ @tag@ - the tag of the regex which should use the specified cursor 
 -> String   -- ^ @name@ - the name of the cursor                                     
 -> IO ()
terminalMatchSetCursorName terminal tag name =
    withUTFString name $ \namePtr ->
    {#call terminal_match_set_cursor_name#} (toTerminal terminal) (fromIntegral tag) namePtr
    
-- | Sets what type of terminal the widget attempts to emulate by scanning for control sequences defined in the system's termcap file. 
-- Unless you are interested in this feature, always use "xterm".
terminalSetEmulation :: 
    TerminalClass self => self
 -> String   -- ^ @emulation@ - the name of a terminal description 
 -> IO ()    
terminalSetEmulation terminal emulation =
    withUTFString emulation $ \emulationPtr ->
    {#call terminal_set_emulation#} (toTerminal terminal) emulationPtr
    
-- | Queries the terminal for its current emulation, as last set by a call to 'terminalSetEmulation'.
terminalGetEmulation :: 
    TerminalClass self => self
 -> IO String   -- ^ return the name of the terminal type the widget is attempting to emulate 
terminalGetEmulation terminal =
    {#call terminal_get_emulation#} (toTerminal terminal) >>= peekCString
    
-- | Queries the terminal for its default emulation, which is attempted if the terminal type passed to 'terminalSetEmulation' emptry string.
--
-- * Available since Vte version 0.11.11
--
terminalGetDefaultEmulation :: 
    TerminalClass self => self
 -> IO String   -- ^ return the name of the default terminal type the widget attempts to emulate 
terminalGetDefaultEmulation terminal =
    {#call terminal_get_default_emulation#} (toTerminal terminal) >>= peekCString
    
-- | Changes the encoding the terminal will expect data from the child to be encoded with. 
-- For certain terminal types, applications executing in the terminal can change the encoding. 
-- The default encoding is defined by the application's locale settings.
terminalSetEncoding ::
    TerminalClass self => self
 -> String   -- ^ @codeset@ - a valid g_iconv target 
 -> IO ()    
terminalSetEncoding terminal codeset =
    withUTFString codeset $ \codesetPtr ->
    {#call terminal_set_encoding#} (toTerminal terminal) codesetPtr
    
-- | Determines the name of the encoding in which the terminal expects data to be encoded.
terminalGetEncoding :: 
    TerminalClass self => self
 -> IO String   -- ^ return the current encoding for the terminal. 
terminalGetEncoding terminal =
    {#call terminal_get_encoding#} (toTerminal terminal) >>= peekCString
    
-- | Some terminal emulations specify a status line which is separate from the main display area, 
-- and define a means for applications to move the cursor to the status line and back.
terminalGetStatusLine :: 
    TerminalClass self => self
 -> IO String   -- ^ The current content of the terminal's status line. For terminals like "xterm", this will usually be the empty string.
terminalGetStatusLine terminal =
    {#call terminal_get_status_line#} (toTerminal terminal) >>= peekCString
    
-- | Determines the amount of additional space the widget is using to pad the edges of its visible area. 
-- This is necessary for cases where characters in the selected font don't themselves include a padding area and the text itself would otherwise be contiguous with the window border. 
-- Applications which use the widget's row_count, column_count, char_height, and char_width fields to set geometry hints using 'windowSetGeometryHints' will need to add this value to the base size. 
-- The values returned in xpad and ypad are the total padding used in each direction, and do not need to be doubled.
terminalGetPadding :: 
    TerminalClass self => self
 -> IO (Int, Int)   -- ^ @(lr,tb)@ - the left\/right-edge and top\/bottom-edge padding 
terminalGetPadding terminal =
    alloca $ \xPtr ->
        alloca $ \yPtr -> do
            {#call terminal_get_padding#} (toTerminal terminal) xPtr yPtr
            xpad <- peek xPtr
            ypad <- peek yPtr
            return (fromIntegral xpad,fromIntegral ypad)

-- | Get 'Adjustment' of terminal widget.
terminalGetAdjustment :: 
    TerminalClass self => self
 -> IO Adjustment   -- ^ return the contents of terminal's adjustment field 
terminalGetAdjustment terminal =
    makeNewObject mkAdjustment $ {#call terminal_get_adjustment#} (toTerminal terminal)

-- | Get terminal's char height.
terminalGetCharHeight :: 
    TerminalClass self => self
 -> IO Int   -- ^ return the contents of terminal's char_height field 
terminalGetCharHeight terminal =
    liftM fromIntegral $
    {#call terminal_get_char_height#} (toTerminal terminal)

-- | Get terminal's char width.
terminalGetCharWidth :: 
    TerminalClass self => self
 -> IO Int   -- ^ return the contents of terminal's char_width field 
terminalGetCharWidth terminal =
    liftM fromIntegral $
    {#call terminal_get_char_width#} (toTerminal terminal)

-- | Get terminal's column count.
terminalGetColumnCount :: 
    TerminalClass self => self
 -> IO Int   -- ^ return the contents of terminal's column_count field 
terminalGetColumnCount terminal =
    liftM fromIntegral $
    {#call terminal_get_column_count#} (toTerminal terminal)
    
-- | Get terminal's row count.
terminalGetRowCount :: 
    TerminalClass self => self
 -> IO Int   -- ^ return the contents of terminal's row_count field 
terminalGetRowCount terminal =
    liftM fromIntegral $
    {#call terminal_get_row_count#} (toTerminal terminal)

-- | Get icon title.
terminalGetIconTitle :: 
    TerminalClass self => self
 -> IO String   -- ^ return the contents of terminal's icon_title field 
terminalGetIconTitle terminal =
    {#call terminal_get_icon_title#} (toTerminal terminal) >>= peekCString
    
-- | Get window title.
terminalGetWindowTitle :: 
    TerminalClass self => self
 -> IO String   -- ^ return the contents of terminal's window_title field 
terminalGetWindowTitle terminal =
    {#call terminal_get_window_title#} (toTerminal terminal) >>= peekCString

--------------------
-- Attributes
-- | Controls whether or not the terminal will attempt to draw bold text. 
-- This may happen either by using a bold font variant, or by repainting text with a different offset.
--
-- Default value: @True@
--
-- * Available since Vte version 0.19.1
--
terminalAllowBold :: TerminalClass self => Attr self Bool
terminalAllowBold = newAttr
  terminalGetAllowBold
  terminalSetAllowBold

-- | Controls whether or not the terminal will beep when the child outputs the \"bl\" sequence.
--
-- Default value: @True@
--
--
-- * Available since Vte version 0.19.1
--
terminalAudibleBell :: TerminalClass self => Attr self Bool
terminalAudibleBell = newAttr
  terminalGetAudibleBell
  terminalSetAudibleBell

-- | Sets a background image file for the widget. 
-- If specified by "background-saturation:", the terminal will tint its in-memory copy of the image before applying it to the terminal.
--
-- Default value: \"\"
--
-- * Available since Vte version 0.19.1
--
terminalBackgroundImageFile :: TerminalClass self => Attr self String
terminalBackgroundImageFile = 
  newAttrFromStringProperty "background-image-file"                              

-- | Sets a background image for the widget. 
-- Text which would otherwise be drawn using the default background color will instead be drawn over the specified image. 
-- If necessary, the image will be tiled to cover the widget's entire visible area. 
-- If specified by "background-saturation:", the terminal will tint its in-memory copy of the image before applying it to the terminal.
--
-- * Available since Vte version 0.19.1
--
terminalBackgroundImagePixbuf :: TerminalClass self => Attr self (Maybe Pixbuf)
terminalBackgroundImagePixbuf =
  newAttrFromMaybeObjectProperty "background-image-pixbuf" 
  {#call pure gdk_pixbuf_get_type#}

-- | Sets the opacity of the terminal background, were 0.0 means completely transparent and 1.0 means completely opaque.
-- 
-- Allowed values: [0,1]
--
-- Default values: 1
--
-- * Available since Vte version 0.19.1
--
terminalBackgroundOpacity :: TerminalClass self => Attr self Double
terminalBackgroundOpacity =
  newAttrFromDoubleProperty "background-opacity"

-- | If a background image has been set using "background-image-file:" or "background-image-pixbuf:", or "background-transparent:", 
-- and the saturation value is less than 1.0, the terminal will adjust the colors of the image before drawing the image. 
-- To do so, the terminal will create a copy of the background image (or snapshot of the root window) and modify its pixel values.
--
-- Allowed values: [0,1]
--
-- Default value: 0.4
--
-- * Available since Vte version 0.19.1
--
terminalBackgroundSaturation :: TerminalClass self => Attr self Double
terminalBackgroundSaturation =
  newAttrFromDoubleProperty "background-saturation"

-- | If a background image has been set using "background-image-file:" or "background-image-pixbuf:", or "background-transparent:", 
-- and the value set by 'Terminal' background-saturation: is less than 1.0, the terminal will adjust the color of the image before drawing the image. 
-- To do so, the terminal will create a copy of the background image (or snapshot of the root window) and modify its pixel values. 
-- The initial tint color is black.
--
-- * Available since Vte version 0.19.1
--
terminalBackgroundTintColor :: TerminalClass self => Attr self Color
terminalBackgroundTintColor =
  newAttrFromBoxedStorableProperty "background-tint-color"
  {#call pure unsafe gdk_color_get_type#}

-- | Sets whther the terminal uses the pixmap stored in the root window as the background, 
-- adjusted so that if there are no windows below your application, the widget will appear to be transparent.
--
-- NOTE: When using a compositing window manager, you should instead set a RGBA colourmap on the toplevel window, so you get real transparency.
-- 
-- Default value: @False@
--
-- * Available since Vte version 0.19.1
--
terminalBackgroundTransparent :: TerminalClass self => Attr self Bool
terminalBackgroundTransparent =
  newAttrFromBoolProperty "background-transparent"

-- | *Controls what string or control sequence the terminal sends to its child when the user presses the backspace key.
--
-- Default value: 'EraseAuto'
--
-- * Available since Vte version 0.19.1
--
terminalBackspaceBinding :: TerminalClass self => Attr self TerminalEraseBinding
terminalBackspaceBinding =
  newAttrFromEnumProperty "backspace-binding" 
  {#call pure unsafe terminal_erase_binding_get_type#}

-- | Sets whether or not the cursor will blink. 
-- Using 'CursorBlinkSystem' will use the "gtk-cursor-blink" setting.
--
-- Default value: 'CursorBlinkSystem'
--
-- * Available since Vte version 0.19.1
--
terminalCursorBlinkMode :: TerminalClass self => Attr self TerminalCursorBlinkMode
terminalCursorBlinkMode = newAttr
  terminalGetCursorBlinkMode
  terminalSetCursorBlinkMode

-- | Controls the shape of the cursor.
--
-- Default value: 'CursorShapeBlock'
--
-- * Available since Vte version 0.19.1
--
terminalCursorShape :: TerminalClass self => Attr self TerminalCursorShape
terminalCursorShape = newAttr
  terminalGetCursorShape
  terminalSetCursorShape

-- | Controls what string or control sequence the terminal sends to its child when the user presses the delete key.
--
-- Default value: 'EraseAuto'
-- 
-- * Available since Vte version 0.19.1
--
terminalDeleteBinding :: TerminalClass self => Attr self TerminalEraseBinding
terminalDeleteBinding =
  newAttrFromEnumProperty "delete-binding"
  {#call pure unsafe terminal_erase_binding_get_type#}

-- | Sets what type of terminal the widget attempts to emulate by scanning for control sequences defined in the system's termcap file. 
-- Unless you are interested in this feature, always use the default which is "xterm".
--
-- Default value: "xterm"
--
-- * Available since Vte version 0.19.1
--
terminalEmulation :: TerminalClass self => Attr self String
terminalEmulation = newAttr
  terminalGetEmulation
  terminalSetEmulation

-- | Controls the encoding the terminal will expect data from the child to be encoded with. 
-- For certain terminal types, applications executing in the terminal can change the encoding. 
-- The default is defined by the application's locale settings.
--
-- Default value: \"\"
--
-- * Available since Vte version 0.19.1
--
terminalEncoding :: TerminalClass self => Attr self String
terminalEncoding = newAttr
  terminalGetEncoding
  terminalSetEncoding

-- | Specifies the font used for rendering all text displayed by the terminal, overriding any fonts set using 'widgetModifyFont'.
-- The terminal will immediately attempt to load the desired font, retrieve its metrics, 
-- and attempt to resize itself to keep the same number of rows and columns.
--
-- * Available since Vte version 0.19.1
--
terminalFontDesc :: TerminalClass self => Attr self FontDescription
terminalFontDesc = newAttr
  terminalGetFont
  terminalSetFont

-- | The terminal's so-called icon title, or empty if no icon title has been set.
--
-- Default value: \"\"
--
-- * Available since Vte version 0.19.1
--
terminalIconTitle :: TerminalClass self => ReadAttr self String
terminalIconTitle = readAttrFromStringProperty "icon-title"

-- | Controls the value of the terminal's mouse autohide setting. 
-- When autohiding is enabled, the mouse cursor will be hidden when the user presses a key and shown when the user moves the mouse.
--
-- Default value: @False@
--
-- * Available since Vte version 0.19.1
--
terminalPointerAutohide :: TerminalClass self => Attr self Bool
terminalPointerAutohide = newAttr
  terminalGetMouseAutohide
  terminalSetMouseAutohide

-- | The file descriptor of the master end of the terminal's PTY.
--
-- Allowed values: [-1 ...]
--
-- Default values: -1
--
-- * Available since Vte version 0.19.1
--
terminalPty :: TerminalClass self => Attr self Int
terminalPty = newAttr
  terminalGetPty
  terminalSetPty

-- | Controls the value of the terminal's mouse autohide setting. 
-- When autohiding is enabled, the mouse cursor will be hidden when the user presses a key and shown when the user moves the mouse.
--
-- Default value: @False@
--
-- * Available since Vte version 0.19.1
--
terminalScrollBackground :: TerminalClass self => Attr self Bool
terminalScrollBackground =
  newAttrFromBoolProperty "scroll-background"

-- | Controls whether or not the terminal will forcibly scroll to the bottom of the viewable history when the user presses a key. 
-- Modifier keys do not trigger this behavior.
--
-- Default value: @False@
--
-- * Available since Vte version 0.19.1
--
terminalScrollOnKeystroke :: TerminalClass self => Attr self Bool
terminalScrollOnKeystroke =
  newAttrFromBoolProperty "scroll-on-keystroke"

-- | Controls whether or not the terminal will forcibly scroll to the bottom of the viewable history when the new data is received from the child.
-- 
-- Default value: @True@
--
-- * Available since Vte version 0.19.1
--
terminalScrollOnOutput :: TerminalClass self => Attr self Bool
terminalScrollOnOutput =
  newAttrFromBoolProperty "scroll-on-output"

-- | The length of the scrollback buffer used by the terminal. The size of the
-- scrollback buffer will be set to the larger of this value and the number of
-- visible rows the widget can display, so 0 can safely be used to disable
-- scrollback. Note that this setting only affects the normal screen buffer.
-- For terminal types which have an alternate screen buffer, no scrollback is
-- allowed on the alternate screen buffer.
--
-- Default value: 100
--
-- * Available since Vte version 0.19.1
--
terminalScrollbackLines :: TerminalClass self => Attr self Int
terminalScrollbackLines =
  newAttrFromUIntProperty "scrollback-lines"

-- | Controls whether the terminal will present a visible bell to the user when the child outputs the \"bl\" sequence. 
-- The terminal will clear itself to the default foreground color and then repaint itself.
--
-- Default value: @False@
--
-- * Available since Vte version 0.19.1
--
terminalVisibleBell :: TerminalClass self => Attr self Bool
terminalVisibleBell = newAttr
  terminalGetVisibleBell
  terminalSetVisibleBell

-- | The terminal's title.
--
-- Default value: \"\"
--
-- * Available since Vte version 0.19.1
--
terminalWindowTitle :: TerminalClass self => ReadAttr self String
terminalWindowTitle = readAttrFromStringProperty "window-title"

-- | When the user double-clicks to start selection, the terminal will extend the selection on word boundaries. 
-- It will treat characters the word-chars characters as parts of words, and all other characters as word separators. 
-- Ranges of characters can be specified by separating them with a hyphen.
-- As a special case, when setting this to the empty string, the terminal will treat all graphic non-punctuation non-space characters as word
-- characters.
-- 
-- Defalut value: \"\"
--
-- * Available since Vte version 0.19.1
--
terminalWordChars :: TerminalClass self => Attr self String
terminalWordChars =
  newAttrFromStringProperty "word-chars"    

--------------------
-- Signals

-- | This signal is emitted when the a child sends a beep request to the terminal.
beep :: TerminalClass self => Signal self (IO ())
beep = Signal (connect_NONE__NONE "beep")

-- | Emitted whenever selection of a new font causes the values of the char_width or char_height fields to change.
charSizeChanged :: TerminalClass self => Signal self (Int -> Int -> IO ())
charSizeChanged = Signal (connect_INT_INT__NONE "char-size-changed")

-- | This signal is emitted when the terminal detects that a child started using 'terminalForkCommand' has exited.
childExited :: TerminalClass self => Signal self (IO ())
childExited = Signal (connect_NONE__NONE "child-exited")

-- | Emitted whenever the terminal receives input from the user and prepares to send it to the child process. 
-- The signal is emitted even when there is no child process.
commit :: TerminalClass self => Signal self (String -> Int -> IO ())
commit = Signal (connect_STRING_INT__NONE "commit")

-- | Emitted whenever the visible appearance of the terminal has changed. Used primarily by 'TerminalAccessible'.
contentsChanged :: TerminalClass self => Signal self (IO ())
contentsChanged = Signal (connect_NONE__NONE "contents-changed")

-- | Emitted whenever 'terminalCopyClipboard' is called.
copyClipboard :: TerminalClass self => Signal self (IO ())
copyClipboard = Signal (connect_NONE__NONE "copy-clipboard")

-- | Emitted whenever the cursor moves to a new character cell. Used primarily by 'TerminalAccessible'.
cursorMoved :: TerminalClass self => Signal self (IO ())
cursorMoved = Signal (connect_NONE__NONE "cursor-moved")

-- | Emitted when the user hits the '-' key while holding the Control key.
decreaseFontSize :: TerminalClass self => Signal self (IO ())
decreaseFontSize = Signal (connect_NONE__NONE "decrease-font-size")

-- | Emitted at the child application's request.
deiconifyWindow :: TerminalClass self => Signal self (IO ())
deiconifyWindow = Signal (connect_NONE__NONE "deiconify-window")

-- | Emitted whenever the terminal's emulation changes, only possible at the parent application's request.
emulationChanged :: TerminalClass self => Signal self (IO ())
emulationChanged = Signal (connect_NONE__NONE "emulation-changed")

-- | Emitted whenever the terminal's current encoding has changed, 
-- either as a result of receiving a control sequence which toggled between the local and UTF-8 encodings, or at the parent application's request.
encodingChanged :: TerminalClass self => Signal self (IO ())
encodingChanged = Signal (connect_NONE__NONE "encoding-changed")

-- | Emitted when the terminal receives an end-of-file from a child which is running in the terminal. 
-- This signal is frequently (but not always) emitted with a 'childExited' signal.
eof :: TerminalClass self => Signal self (IO ())
eof = Signal (connect_NONE__NONE "eof")

-- | Emitted when the terminal's icon_title field is modified.
iconTitleChanged :: TerminalClass self => Signal self (IO ())
iconTitleChanged = Signal (connect_NONE__NONE "icon-title-changed")

-- | Emitted at the child application's request.
iconifyWindow :: TerminalClass self => Signal self (IO ())
iconifyWindow = Signal (connect_NONE__NONE "iconify-window")

-- | Emitted when the user hits the '+' key while holding the Control key.
increaseFontSize :: TerminalClass self => Signal self (IO ())
increaseFontSize = Signal (connect_NONE__NONE "increase-font-size")

-- | Emitted at the child application's request.
lowerWindow :: TerminalClass self => Signal self (IO ())
lowerWindow = Signal (connect_NONE__NONE "lower-window")

-- | Emitted at the child application's request.
maximizeWindow :: TerminalClass self => Signal self (IO ())
maximizeWindow = Signal (connect_NONE__NONE "maximize-window")

-- | Emitted when user move terminal window.
moveWindow :: TerminalClass self => Signal self (Word -> Word -> IO ())
moveWindow = Signal (connect_WORD_WORD__NONE "move-window")

-- | Emitted whenever 'terminalPasteClipboard' is called.
pasteClipboard :: TerminalClass self => Signal self (IO ())
pasteClipboard = Signal (connect_NONE__NONE "paste-clipboard")

-- | Emitted at the child application's request.
raiseWindow :: TerminalClass self => Signal self (IO ())
raiseWindow = Signal (connect_NONE__NONE "raise-window")

-- | Emitted at the child application's request.
refreshWindow :: TerminalClass self => Signal self (IO ())
refreshWindow = Signal (connect_NONE__NONE "refresh-window")

-- | Emitted at the child application's request.
resizeWidnow :: TerminalClass self => Signal self (Int -> Int -> IO ())                                                  
resizeWidnow = Signal (connect_INT_INT__NONE "resize-window")

-- | Emitted at the child application's request.
restoreWindow :: TerminalClass self => Signal self (IO ())
restoreWindow = Signal (connect_NONE__NONE "restore-window")

-- | Emitted at the child application's request.
selectionChanged :: TerminalClass self => Signal self (IO ())
selectionChanged = Signal (connect_NONE__NONE "selection-changed")

-- | Set the scroll adjustments for the terminal. 
-- Usually scrolled containers like 'ScrolledWindow' will emit this 
-- signal to connect two instances of 'Scrollbar' to the scroll directions of the 'Terminal'.
setScrollAdjustments :: TerminalClass self => Signal self (Adjustment -> Adjustment -> IO ())
setScrollAdjustments = Signal (connect_OBJECT_OBJECT__NONE "set-scroll-adjustments")

-- | Emitted whenever the contents of the status line are modified or cleared.
statusLineChanged :: TerminalClass self => Signal self (IO ())
statusLineChanged = Signal (connect_NONE__NONE "status-line-changed")

-- | An internal signal used for communication between the terminal and its accessibility peer. 
-- May not be emitted under certain circumstances.
textDeleted :: TerminalClass self => Signal self (IO ())
textDeleted = Signal (connect_NONE__NONE "text-deleted")

-- | An internal signal used for communication between the terminal and its accessibility peer. 
-- May not be emitted under certain circumstances.
textInserted :: TerminalClass self => Signal self (IO ())
textInserted = Signal (connect_NONE__NONE "text-inserted")

-- | An internal signal used for communication between the terminal and its accessibility peer. 
-- May not be emitted under certain circumstances.
textModified :: TerminalClass self => Signal self (IO ())
textModified = Signal (connect_NONE__NONE "text-modified")

-- | An internal signal used for communication between the terminal and its accessibility peer. 
-- May not be emitted under certain circumstances.
textScrolled :: TerminalClass self => Signal self (Int -> IO ())
textScrolled = Signal (connect_INT__NONE "text-scrolled")

-- | Emitted when the terminal's window_title field is modified.
windowTitleChanged :: TerminalClass self => Signal self (IO ())
windowTitleChanged = Signal (connect_NONE__NONE "window-title-changed")
