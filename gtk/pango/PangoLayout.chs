--  GIMP Toolkit (GTK) - text layout functions @entry layout@
--
--  Author : Axel Simon
--          
--  Created: 8 Feburary 2003
--
--  Version $Revision: 1.1 $ from $Date: 2003/02/09 10:43:01 $
--
--  Copyright (c) 1999..2003 Axel Simon
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
-- @description@ --------------------------------------------------------------
--
-- * Functions to run the rendering pipeline.
--
-- @documentation@ ------------------------------------------------------------
--
-- * The Pango rendering pipeline takes a string of Unicode characters
--   and converts it into glyphs.  The functions described in this module
--   accomplish various steps of this process.
--
-- @todo@ ---------------------------------------------------------------------
--
-- * Functions that are missing:
--   pango_layout_set_attributes, pango_layout_get_attributes,
--   pango_layout_set_font_description, pango_layout_set_tabs,
--   pango_layout_get_tabs, pango_layout_get_log_attrs, 
--   pango_layout_iter_get_run
--
-- * The following functions cannot be bound due to Unicode/UTF8 issues:
--   pango_layout_xy_to_index, pango_layout_index_to_pos,
--   pango_layout_get_cursor_pos, pango_layout_move_cursor_visually,
--   pango_layout_iter_get_index
--
-- * These functions are not bound, because they're too easy:
--   pango_layout_get_size, pango_layout_get_pixel_size,
--   pango_layout_get_line 
--
module PangoLayout(
  PangoLayout
  ) where

import Monad    (liftM)
import Foreign
import UTFCForeign
{#import Hierarchy#}
import GObject  (makeNewGObject)
import Markup	(Markup)
import Char	(ord, chr)
import Enums
import Structs	(Rectangle)
import GList	(readGSList)

{# context lib="pango" prefix="pango" #}

-- @method layoutCopy@ Create a copy of the @ref data layout@.
--
layoutCopy :: PangoLayout -> IO PangoLayout
layoutCopy pl = makeNewGObject mkPangoLayout 
		 ({#call unsafe layout_copy#} (toPangoLayout pl))

-- @method layoutGetContext@ Retrieves the @ref data PangoContext@ from this
-- layout.
--
layoutGetContext :: PangoLayout -> IO PangoContext
layoutGetContext pl = makeNewGObject mkPangoContext
		      ({#call unsafe layout_get_context#} pl)

-- @method layoutContextChanged@ Signal a @ref data Context@ change.
--
-- * Forces recomputation of any state in the @ref data PangoLayout@ that
--   might depend on the layout's context. This function should
--   be called if you make changes to the context subsequent
--   to creating the layout.
--
layoutContextChanged :: PangoLayout -> IO ()
layoutContextChanged pl = {#call unsafe layout_context_changed#} pl

-- @method layoutSetText@ Set the string in the layout.
--
layoutSetText :: PangoLayout -> String -> IO ()
layoutSetText pl txt = withCStringLen txt $ \(strPtr,len) ->
  {#call unsafe layout_set_text#} pl strPtr (fromIntegral len)

-- @method layoutGetText@ Retrieve the string in the layout.
--
layoutGetText :: PangoLayout -> IO String
layoutGetText pl = {#call unsafe layout_get_text#} pl >>= peekCString

-- @method layoutSetMarkup@ Set the string in the layout.
--
-- * The string may include @ref data Markup@.
--
layoutSetMarkup :: PangoLayout -> Markup -> IO ()
layoutSetMarkup pl txt = withCStringLen txt $ \(strPtr,len) ->
  {#call unsafe layout_set_markup#} pl strPtr (fromIntegral len)

-- @method layoutSetMarkupWithAccel@ Set the string in the layout.
--
-- * The string may include @ref data Markup@. Furthermore, any underscore
--   character indicates that the next character should be
--   marked as accelerator (i.e. underlined). A literal underscore character
--   can be produced by placing it twice in the string.
--
-- * The character which follows the underscore is
--   returned so it can be used to add the actual keyboard shortcut. 
--
layoutSetMarkupWithAccel :: PangoLayout -> Markup -> IO Char
layoutSetMarkupWithAccel pl txt =
  alloca $ \chrPtr -> 
  withCStringLen txt $ \(strPtr,len) -> do
    {#call unsafe layout_set_markup_with_accel#} pl strPtr (fromIntegral len)
      (fromIntegral (ord '_')) chrPtr
    liftM (chr.fromIntegral) $ peek chrPtr


-- there are a couple of functions missing here

-- @method layoutSetWidth@ Set the width of this paragraph.
--
-- * Sets the width to which the lines of the @ref data PangoLayout@
--   should be wrapped.
--
-- * @ref arg width$ is the desired width, or @literal -1@ to indicate that
--   no wrapping should be performed.
--
layoutSetWidth :: PangoLayout -> Int -> IO ()
layoutSetWidth pl width =
  {#call unsafe layout_set_width#} pl (fromIntegral width)

-- @method layoutGetWidth@ Gets the width of this paragraph.
--
-- * Gets the width to which the lines of the @ref data PangoLayout@
--   should be wrapped.
--
-- * Returns is the current width, or @literal -1@ to indicate that
--   no wrapping is performed.
--
layoutGetWidth :: PangoLayout -> IO Int
layoutGetWidth pl = liftM fromIntegral $ {#call unsafe layout_get_width#} pl


-- @data LayoutWarpMode@ Enumerates how a line can be wrapped.
--
-- @variant WrapWord@ Breaks lines only between words.
-- @variant WrapChar@ Break lines anywhere.
-- @variant WrapWordChar@ Wrap within a word if it is the only one on
-- this line.
--
{#enum PangoWrapMode as LayoutWrapMode {underscoreToCase}#}

-- @method layoutSetWrap@ Set how this paragraph is wrapped.
--
-- * Sets the wrap style; the wrap style only has an effect if a width
--   is set on the layout with @ref method layoutSetWidth@. To turn off
--   wrapping, set the width to -1.
--
layoutSetWrap :: PangoLayout -> LayoutWrapMode -> IO ()
layoutSetWrap pl wm =
  {#call unsafe layout_set_wrap#} pl ((fromIntegral.fromEnum) wm)


-- @method layoutGetWrap@ Get the wrap mode for the layout.
--
layoutGetWrap :: PangoLayout -> IO LayoutWrapMode
layoutGetWrap pl = liftM (toEnum.fromIntegral) $
  {#call unsafe layout_get_wrap#} pl

-- @method layoutSetIndent@ Set the indentation of this paragraph.
--
-- * Sets the amount by which the first line should be shorter than
--   the rest of the lines. This may be negative, in which case the
--   subsequent lines will be shorter than the first line. (However, in
--   either case, the entire width of the layout will be given by the
--   value.
--
layoutSetIndent :: PangoLayout -> Int -> IO ()
layoutSetIndent pl indent =
  {#call unsafe layout_set_indent#} pl (fromIntegral indent)

-- @method layoutGetIndent@ Gets the indentation of this paragraph.
--
-- * Gets the amount by which the first line should be shorter than 
--   the rest of the lines.
--
layoutGetIndent :: PangoLayout -> IO Int
layoutGetIndent pl = liftM fromIntegral $ {#call unsafe layout_get_indent#} pl


-- @method layoutSetSpacing@ Set the spacing between lines of this paragraph.
--
layoutSetSpacing :: PangoLayout -> Int -> IO ()
layoutSetSpacing pl spacing =
  {#call unsafe layout_set_spacing#} pl (fromIntegral spacing)

-- @method layoutGetSpacing@ Gets the spacing between the lines.
--
layoutGetSpacing :: PangoLayout -> IO Int
layoutGetSpacing pl = 
  liftM fromIntegral $ {#call unsafe layout_get_spacing#} pl

-- @method layoutSetJustify@ Set if text should be streched to fit width.
--
-- * Sets whether or not each complete line should be stretched to
--   fill the entire width of the layout. This stretching is typically
--   done by adding whitespace, but for some scripts (such as Arabic),
--   the justification is done by extending the characters.
--
layoutSetJustify :: PangoLayout -> Bool -> IO ()
layoutSetJustify pl j = {#call unsafe layout_set_justify#} pl (fromBool j)

-- @method layoutGetJustify@ Retrieve the justification flag.
--
-- * See @ref method layoutSetJustify@.
--
layoutGetJustify :: PangoLayout -> IO Bool
layoutGetJustify pl = liftM toBool $ {#call unsafe layout_get_justify#} pl

-- @data LayoutAlignment@ Enumerate to which side incomplete lines are flushed.
--
{#enum PangoAlignment as LayoutAlignment {underscoreToCase}#}

-- @method layoutSetAlignment@ Set how this paragraph is aligned.
--
-- * Sets the alignment for the layout (how partial lines are
--   positioned within the horizontal space available.)
--
layoutSetAlignment :: PangoLayout -> LayoutAlignment -> IO ()
layoutSetAlignment pl am =
  {#call unsafe layout_set_alignment#} pl ((fromIntegral.fromEnum) am)


-- @method layoutGetAlignment@ Get the alignment for the layout.
--
layoutGetAlignment :: PangoLayout -> IO LayoutAlignment
layoutGetAlignment pl = liftM (toEnum.fromIntegral) $
  {#call unsafe layout_get_alignment#} pl

-- functions are missing here

-- @method layoutSetSingleParagraphMode@ Honor newlines or not.
--
-- * If @ref arg honor@ is @literal True@, do not treat newlines and
--   similar characters as paragraph separators; instead, keep all text in
--   a single paragraph, and display a glyph for paragraph separator
--   characters. Used when you want to allow editing of newlines on a
--   single text line.
--
layoutSetSingleParagraphMode :: PangoLayout -> Bool -> IO ()
layoutSetSingleParagraphMode pl honor = 
  {#call unsafe layout_set_single_paragraph_mode#} pl (fromBool honor)

-- @method layoutGetSingleParagraphMode@ Retrieve if newlines are honored.
--
-- * See @ref method layoutSetSingleParagraphMode@.
--
layoutGetSingleParagraphMode :: PangoLayout -> IO Bool
layoutGetSingleParagraphMode pl = 
  liftM toBool $ {#call unsafe layout_get_single_paragraph_mode#} pl

-- a function is missing here

-- @method layoutGetExtents@ Compute the physical size of the layout.
--
-- * Computes the logical and the ink size of the @ref data Layout@. The
--   logical layout is used for positioning, the ink size is the smallest
--   bounding box that includes all character pixels. The ink size can be
--   smaller or larger that the logical layout.
--
-- * All values are in layout units. To get to device units (pixel for
--   @ref data Drawable@s) divide by @ref constant pangoScale@.
--
layoutGetExtents :: PangoLayout -> IO (Rectangle, Rectangle)
layoutGetExtents pl = alloca $ \logPtr -> alloca $ \inkPtr -> do
  {#call unsafe layout_get_extents#} pl (castPtr logPtr) (castPtr inkPtr)
  log <- peek logPtr
  ink <- peek inkPtr
  return (log,ink)


-- @method layoutGetPixelExtents@ Compute the physical size of the layout.
--
-- * Computes the logical and the ink size of the @ref data Layout@. The
--   logical layout is used for positioning, the ink size is the smallest
--   bounding box that includes all character pixels. The ink size can be
--   smaller or larger that the logical layout.
--
-- * All values are in device units. This function is a wrapper around
--   @ref method layoutGetExtents@ with scaling.
--
layoutGetPixelExtents :: PangoLayout -> IO (Rectangle, Rectangle)
layoutGetPixelExtents pl = alloca $ \logPtr -> alloca $ \inkPtr -> do
  {#call unsafe layout_get_pixel_extents#} pl (castPtr logPtr) (castPtr inkPtr)
  log <- peek logPtr
  ink <- peek inkPtr
  return (log,ink)

-- @method layoutGetLineCount@ Ask for the number of lines in this layout.
--
layoutGetLineCound :: PangoLayout -> IO Int
layoutGetLineCound pl = liftM fromIntegral $
  {#call unsafe layout_get_line_count#} pl

-- @method layoutGetLines@ Extract the single lines of the layout.
--
-- * The lines of each layout are regenerated if any attribute changes.
--   Thus the returned list does not reflect the current state of lines
--   after a change has been made.
--
layoutGetLines :: PangoLayout -> IO [LayoutLine]
layoutGetLines pl = do
  listPtr <- {#call unsafe layout_get_lines#} pl
  list <- readGSList listPtr
  mapM mkLayoutLine list

-- @data LayoutIter@ An iterator to examine a layout.
--
{#pointer *PangoLayoutIter as LayoutIter foreign newtype #}

foreign import ccall "pango_layout_iter_free" unsafe
  layout_iter_free :: Ptr LayoutIter -> IO ()

-- @constructor layoutGetIter@ Create an iterator to examine a layout.
--
layoutGetIter :: PangoLayout -> IO LayoutIter
layoutGetIter pl = do
  iterPtr <- {#call unsafe layout_get_iter#} pl
  liftM LayoutIter $ newForeignPtr iterPtr (layout_iter_free iterPtr)

-- @method layoutNextRun@ Move to the next run.
--
-- * Returns @literal False@ if this was the last run in the layout.
--
layoutIterNextRun :: LayoutIter -> IO Bool
layoutIterNextRun = liftM toBool . {#call unsafe layout_iter_next_run#}

-- @method layoutNextChar@ Move to the next char.
--
-- * Returns @literal False@ if this was the last char in the layout.
--
layoutIterNextChar :: LayoutIter -> IO Bool
layoutIterNextChar = liftM toBool . {#call unsafe layout_iter_next_char#}

-- @method layoutNextCluster@ Move to the next cluster.
--
-- * Returns @literal False@ if this was the last cluster in the layout.
--
layoutIterNextCluster :: LayoutIter -> IO Bool
layoutIterNextCluster = liftM toBool . {#call unsafe layout_iter_next_cluster#}

-- @method layoutNextLine@ Move to the next line.
--
-- * Returns @literal False@ if this was the last line in the layout.
--
layoutIterNextLine :: LayoutIter -> IO Bool
layoutIterNextLine = liftM toBool . {#call unsafe layout_iter_next_line#}

-- @method layoutAtLastLine@ Check if the iterator is on the last line.
--
-- * Returns @literal True@ if the iterator is on the last line of this
--   paragraph.
--
layoutIterAtLastLine :: LayoutIter -> IO Bool
layoutIterAtLastLine = liftM toBool . {#call unsafe layout_iter_at_last_line#}

-- @method layoutIterGetBaseline@ Query the vertical position within the
-- layout.
--
-- * Gets the y position of the current line's baseline, in layout
--   coordinates (origin at top left of the entire layout).
--
layoutIterGetBaseline :: LayoutIter -> IO Int
layoutIterGetBaseline = 
  liftM fromIntegral . {#call unsafe pango_layout_iter_get_baseline#}

-- @method layoutIterGetLine@ Extract the line under the iterator.
--
layoutIterGetLine :: LayoutIter -> IO (Maybe LayoutLine)
layoutIterGetLine li = do
  llPtr <- liftM castPtr $ {#call unsafe pango_layout_iter_get_line#} li
  if (llPtr==nullPtr) then return Nothing else 
    liftM Just $ mkLayoutLine llPtr



-- @data LayoutLine@ A single line in a @ref data PangoLayout@.
--
{#pointer *PangoLayoutLine as LayoutLine foreign newtype #}

foreign import ccall "pango_layout_line_ref" unsafe
  layout_line_ref :: Ptr LayoutLine -> IO ()

foreign import ccall "pango_layout_line_unref" unsafe
  layout_line_unref :: Ptr LayoutLine -> IO ()

mkLayoutLine :: Ptr LayoutLine -> IO LayoutLine
mkLayoutLine llPtr = do
  layout_line_ref llPtr
  liftM LayoutLine $ newForeignPtr llPtr (layout_line_unref llPtr)