{-# LANGUAGE CPP, OverloadedStrings #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Pango text layout functions
--
--  Author : Axel Simon
--
--  Created: 8 Feburary 2003
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
-- Not bound:
--
--  - pango_layout_get_log_attrs : difficult since it returns an array, where
--    each element corresponds to a UTF8 character, conversion to wide
--    characters means we need to do some semantic merging
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Functions to run the rendering pipeline.
--
-- * The 'PangoLayout' object defined in this module contain a rendered
--   paragraph of text. This interface is the easiest way to render text into
--   a 'Graphics.UI.Gtk.Gdk.DrawWindow.DrawWindow' using Cairo.
--
module Graphics.Rendering.Pango.Layout (
  PangoRectangle(..),
  PangoLayout,
  layoutEmpty,
  layoutText,
  layoutCopy,
  layoutGetContext,
  layoutContextChanged,
  layoutSetText,
  layoutGetText,
  layoutSetMarkup,
  escapeMarkup,
  layoutSetMarkupWithAccel,
  layoutSetAttributes,
  layoutGetAttributes,
  layoutSetFontDescription,
#if PANGO_VERSION_CHECK(1,8,0)
  layoutGetFontDescription,
#endif
  layoutSetWidth,
  layoutGetWidth,
  LayoutWrapMode(..),
  layoutSetWrap,
  layoutGetWrap,
#if PANGO_VERSION_CHECK(1,6,0)
  EllipsizeMode(..),
  layoutSetEllipsize,
  layoutGetEllipsize,
#endif
  layoutSetIndent,
  layoutGetIndent,
  layoutSetSpacing,
  layoutGetSpacing,
  layoutSetJustify,
  layoutGetJustify,
#if PANGO_VERSION_CHECK(1,4,0)
  layoutSetAutoDir,
  layoutGetAutoDir,
#endif
  LayoutAlignment(..),
  layoutSetAlignment,
  layoutGetAlignment,
  TabAlign,
  TabPosition,
  layoutSetTabs,
  layoutResetTabs,
  layoutGetTabs,
  layoutSetSingleParagraphMode,
  layoutGetSingleParagraphMode,
  layoutXYToIndex,
  layoutIndexToPos,
  layoutGetCursorPos,
  CursorPos(..),
  layoutMoveCursorVisually,
  layoutGetExtents,
  layoutGetPixelExtents,
  layoutGetLineCount,
  layoutGetLine,
  layoutGetLines,
  LayoutIter,
  layoutGetIter,
  layoutIterNextItem,
  layoutIterNextChar,
  layoutIterNextCluster,
  layoutIterNextLine,
  layoutIterAtLastLine,
  layoutIterGetIndex,
  layoutIterGetBaseline,
#if PANGO_VERSION_CHECK(1,2,0)
  layoutIterGetItem,
#endif
  layoutIterGetLine,
  layoutIterGetCharExtents,
  layoutIterGetClusterExtents,
  layoutIterGetRunExtents,
  layoutIterGetLineYRange,
  layoutIterGetLineExtents,
  LayoutLine,
  layoutLineGetExtents,
  layoutLineGetPixelExtents,
  layoutLineIndexToX,
  layoutLineXToIndex,
  layoutLineGetXRanges
  ) where

import Control.Monad    (liftM)
import Data.Char     (ord, chr)
import Data.Text (Text)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GList                (readGSList)
import System.Glib.GObject              (wrapNewGObject, makeNewGObject)
import Graphics.Rendering.Pango.Structs
{#import Graphics.Rendering.Pango.BasicTypes#}
import Graphics.Rendering.Pango.Types
#if PANGO_VERSION_CHECK(1,6,0)
{#import Graphics.Rendering.Pango.Enums#}       (EllipsizeMode(..))
#endif
import Graphics.Rendering.Pango.Rendering  -- for haddock
import Graphics.Rendering.Pango.Attributes ( withAttrList, fromAttrList)
import Data.IORef
import Control.Exception (throwIO, ArrayException(IndexOutOfBounds))

{# context lib="pango" prefix="pango" #}

-- | Create an empty 'Layout'.
--
layoutEmpty :: PangoContext -> IO PangoLayout
layoutEmpty pc = do
  pl <- wrapNewGObject mkPangoLayoutRaw
    ({#call unsafe layout_new#} (toPangoContext pc))
  ps <- makeNewPangoString ("" :: Text)
  psRef <- newIORef ps
  return (PangoLayout psRef pl)

-- | Create a new layout.
--
layoutText :: GlibString string => PangoContext -> string -> IO PangoLayout
layoutText pc txt = do
  pl <- wrapNewGObject mkPangoLayoutRaw
    ({#call unsafe layout_new#} (toPangoContext pc))
  withUTFStringLen txt $ \(strPtr,len) ->
    {#call unsafe layout_set_text#} pl strPtr (fromIntegral len)
  ps <- makeNewPangoString txt
  psRef <- newIORef ps
  return (PangoLayout psRef pl)

-- | Create a copy of the 'Layout'.
--
layoutCopy :: PangoLayout -> IO PangoLayout
layoutCopy (PangoLayout uc pl) = do
  pl <- wrapNewGObject mkPangoLayoutRaw
    ({#call unsafe layout_copy#} pl)
  return (PangoLayout uc pl)

-- | Retrieves the 'PangoContext' from this layout.
--
layoutGetContext :: PangoLayout -> IO PangoContext
layoutGetContext (PangoLayout _ pl) = makeNewGObject mkPangoContext $ do
  {#call unsafe layout_get_context#} pl

-- | Signal a 'PangoContext' change.
--
-- * Forces recomputation of any state in the 'PangoLayout' that
--   might depend on the layout's context. This function should
--   be called if you make changes to the context subsequent
--   to creating the layout.
--
layoutContextChanged :: PangoLayout -> IO ()
layoutContextChanged (PangoLayout _ pl) =
  {#call unsafe layout_context_changed#} pl

-- | Set the string in the layout.
--
layoutSetText :: GlibString string => PangoLayout -> string -> IO ()
layoutSetText (PangoLayout psRef pl) txt = do
  withUTFStringLen txt $ \(strPtr,len) ->
    {#call unsafe layout_set_text#} pl strPtr (fromIntegral len)
  ps <- makeNewPangoString txt
  writeIORef psRef ps

-- | Retrieve the string in the layout.
--
layoutGetText :: GlibString string => PangoLayout -> IO string
layoutGetText (PangoLayout _ pl) =
  {#call unsafe layout_get_text#} pl >>= peekUTFString

-- | Set the text of the layout, including attributes.
--
-- The string may include 'Markup'. To print markup characters like
-- @\'<\'@, or @\'-\'@, apply 'escapeMarkup' to the string first.
--
-- The function returns  the text that is actually shown.
--
layoutSetMarkup :: (GlibString markup, GlibString string)
    => PangoLayout -> markup -> IO string
layoutSetMarkup pl@(PangoLayout psRef plr) txt = do
  withUTFStringLen txt $ \(strPtr,len) ->
    {#call unsafe layout_set_markup#} plr strPtr (fromIntegral len)
  txt' <- layoutGetText pl
  ps <- makeNewPangoString txt'
  writeIORef psRef ps
  return txt'

-- | Escape markup characters.
--
-- * Used to display characters that normally denote markup. Note that this
--   function is strict in that it forces all characters in the input string
--   as soon as a single output character is requested.
--
escapeMarkup :: GlibString string => string -> string
escapeMarkup str = unsafePerformIO $ withUTFStringLen str $ \(strPtr,l) -> do
  resPtr <- {#call unsafe g_markup_escape_text#} strPtr (fromIntegral l)
  res <- peekUTFString resPtr
  {#call unsafe g_free#} (castPtr resPtr)
  return res

-- | Set the string in the layout.
--
-- * The string may include 'Markup'. Furthermore, any underscore
--   character indicates that the next character will be
--   marked as accelerator (i.e. underlined). A literal underscore character
--   can be produced by placing it twice in the string.
--
-- * The character which follows the underscore is
--   returned so it can be used to add the actual keyboard shortcut.
--   The second element is the string after parsing.
--
layoutSetMarkupWithAccel :: (GlibString markup, GlibString string)
    => PangoLayout -> markup -> IO (Char, string)
layoutSetMarkupWithAccel pl@(PangoLayout psRef plr) txt = do
  modif <- alloca $ \chrPtr ->
    withUTFStringLen txt $ \(strPtr,len) -> do
      {#call unsafe layout_set_markup_with_accel#} plr strPtr
        (fromIntegral len) (fromIntegral (ord '_')) chrPtr
      liftM (chr.fromIntegral) $ peek chrPtr
  txt' <- layoutGetText pl
  ps <- makeNewPangoString txt'
  writeIORef psRef ps
  return (modif, txt')


-- | Set text attributes of the text in the layout.
--
-- * This function replaces any text attributes that this layout contained,
--   even those that were set by using 'layoutSetMarkup'.
--
layoutSetAttributes :: PangoLayout -> [PangoAttribute] -> IO ()
layoutSetAttributes (PangoLayout psRef plr) attrs = do
  ps <- readIORef psRef
  withAttrList ps attrs $ \alPtr ->
    {#call unsafe pango_layout_set_attributes#} plr alPtr

-- | Gets the list of attributes of the layout, if any.
--
-- * The attribute list is a list of lists of attribute. Each list describes
--   the attributes for the same span.
--
layoutGetAttributes :: PangoLayout -> IO [[PangoAttribute]]
layoutGetAttributes (PangoLayout psRef plr) = do
  (PangoString correct _ _) <- readIORef psRef
  attrListPtr <- {#call unsafe pango_layout_get_attributes#} plr
  fromAttrList correct attrListPtr

-- | Set a specific font description for this layout.
--
-- * Specifying @Nothing@ will unset the current font description, that is,
--   the 'PangoLayout' will use the font description in the current
--   'PangoContext'.
--
layoutSetFontDescription :: PangoLayout -> Maybe FontDescription -> IO ()
layoutSetFontDescription (PangoLayout _ plr) (Just fd) =
  {#call unsafe layout_set_font_description#} plr fd
layoutSetFontDescription (PangoLayout _ (PangoLayoutRaw plr)) Nothing =
  withForeignPtr plr $ \plrPtr ->
  pango_layout_set_font_description plrPtr nullPtr

#if PANGO_VERSION_CHECK(1,8,0)
-- | Ask for the specifically set font description of this layout.
--
-- * Returns @Nothing@ if this layout uses the font description in the
--   'PangoContext' it was created in.
--
-- * Only available in Pango 1.8.0 or higher.
--
layoutGetFontDescription :: PangoLayout -> IO (Maybe FontDescription)
layoutGetFontDescription (PangoLayout _ plr) = do
  fdPtr <- {#call unsafe layout_get_font_description#} plr
  if fdPtr==nullPtr then return Nothing else liftM Just $ do
    fdPtr' <- font_description_copy fdPtr
    makeNewFontDescription fdPtr'

foreign import ccall unsafe "pango_font_description_copy"
  font_description_copy :: Ptr FontDescription -> IO (Ptr FontDescription)
#endif


-- | Set the width of this paragraph.
--
-- * Sets the width to which the lines of the 'PangoLayout'
--   should be wrapped.
--
-- * Pass in @Nothing@ to indicate that no wrapping is to be performed.
--
layoutSetWidth :: PangoLayout -> Maybe Double -> IO ()
layoutSetWidth (PangoLayout _ pl) Nothing =
  {#call unsafe layout_set_width#} pl (-1)
layoutSetWidth (PangoLayout _ pl) (Just pu) =
  {#call unsafe layout_set_width#} pl (puToInt pu)

-- | Gets the width of this paragraph.
--
-- * Gets the width to which the lines of the 'PangoLayout'
--   should be wrapped.
--
-- * Returns is the current width, or @Nothing@ to indicate that
--   no wrapping is performed.
--
layoutGetWidth :: PangoLayout -> IO (Maybe Double)
layoutGetWidth (PangoLayout _ pl) = do
  w <- {#call unsafe layout_get_width#} pl
  return (if w==(-1) then Nothing else Just (intToPu w))

-- | Enumerates how a line can be wrapped.
--
-- [@WrapWholeWords@] Breaks lines only between words.
--
-- * This variant does not guarantee that the requested width is not
--   exceeded. A word that is longer than the paragraph width is not
--   split.
--
-- [@WrapAnywhere@] Break lines anywhere.
--
-- [@WrapPartialWords@] Wrap within a word if it is the only one on
-- this line.
--
-- * This option acts like 'WrapWholeWords' but will split
--   a word if it is the only one on this line and it exceeds the
--   specified width.
--
{#enum PangoWrapMode as LayoutWrapMode
  {underscoreToCase,
  PANGO_WRAP_WORD as WrapWholeWords,
  PANGO_WRAP_CHAR as WrapAnywhere,
  PANGO_WRAP_WORD_CHAR as WrapPartialWords}#}

-- | Set how this paragraph is wrapped.
--
-- * Sets the wrap style; the wrap style only has an effect if a width
--   is set on the layout with 'layoutSetWidth'. To turn off
--   wrapping, call 'layoutSetWidth' with @Nothing@.
--
layoutSetWrap :: PangoLayout -> LayoutWrapMode -> IO ()
layoutSetWrap (PangoLayout _ pl) wm =
  {#call unsafe layout_set_wrap#} pl ((fromIntegral.fromEnum) wm)


-- | Get the wrap mode for the layout.
--
layoutGetWrap :: PangoLayout -> IO LayoutWrapMode
layoutGetWrap (PangoLayout _ pl) = liftM (toEnum.fromIntegral) $
  {#call unsafe layout_get_wrap#} pl

#if PANGO_VERSION_CHECK(1,6,0)
-- | Set how long lines should be abbreviated.
--
layoutSetEllipsize :: PangoLayout -> EllipsizeMode -> IO ()
layoutSetEllipsize (PangoLayout _ pl) em =
  {#call unsafe layout_set_ellipsize#} pl ((fromIntegral.fromEnum) em)

-- | Get the ellipsize mode for this layout.
--
layoutGetEllipsize :: PangoLayout -> IO EllipsizeMode
layoutGetEllipsize (PangoLayout _ pl) = liftM (toEnum.fromIntegral) $
  {#call unsafe layout_get_ellipsize#} pl
#endif

-- | Set the indentation of this paragraph.
--
-- * Sets the amount by which the first line should
--   be indented. A negative value will produce a hanging indent, that is,
--   all subsequent lines will be indented while the first line has full
--   width.
--
layoutSetIndent :: PangoLayout -> Double -> IO ()
layoutSetIndent (PangoLayout _ pl) indent =
  {#call unsafe layout_set_indent#} pl (puToInt indent)

-- | Gets the indentation of this paragraph.
--
-- * Gets the amount by which the first line or the rest of the paragraph
--   is indented.
--
layoutGetIndent :: PangoLayout -> IO Double
layoutGetIndent (PangoLayout _ pl) =
  liftM intToPu $ {#call unsafe layout_get_indent#} pl


-- | Set the spacing between lines of this paragraph.
--
layoutSetSpacing :: PangoLayout -> Double -> IO ()
layoutSetSpacing (PangoLayout _ pl) spacing =
  {#call unsafe layout_set_spacing#} pl (puToInt spacing)

-- | Gets the spacing between the lines.
--
layoutGetSpacing :: PangoLayout -> IO Double
layoutGetSpacing (PangoLayout _ pl) =
  liftM intToPu $ {#call unsafe layout_get_spacing#} pl

-- | Set if text should be streched to fit width.
--
-- * Sets whether or not each complete line should be stretched to
--   fill the entire width of the layout. This stretching is typically
--   done by adding whitespace, but for some scripts (such as Arabic),
--   the justification is done by extending the characters.
--
-- * Note that  as of Pango 1.4, this functionality is not yet implemented.
--
layoutSetJustify :: PangoLayout -> Bool -> IO ()
layoutSetJustify (PangoLayout _ pl) j =
  {#call unsafe layout_set_justify#} pl (fromBool j)

-- | Retrieve the justification flag.
--
-- * See 'layoutSetJustify'.
--
layoutGetJustify :: PangoLayout -> IO Bool
layoutGetJustify (PangoLayout _ pl) =
  liftM toBool $ {#call unsafe layout_get_justify#} pl

#if PANGO_VERSION_CHECK(1,4,0)
-- | Set if the base text direction should be overridden.
--
-- * Sets whether to calculate the bidirectional base direction for the
--   layout according to the contents of the layout; when this flag is on
--   (the default), then paragraphs in layout that begin with strong
--   right-to-left characters (Arabic and Hebrew principally), will have
--   right-to-left layout, paragraphs with letters from other scripts will
--   have left-to-right layout. Paragraphs with only neutral characters get
--   their direction from the surrounding paragraphs.
--
-- * When @False@, the choice between left-to-right and right-to-left
--   layout is done by according to the base direction of the layout's
--   'PangoContext'. (See 'Graphics.Rendering.Pango.Context.contextSetTextDir').
--
-- * When the auto-computed direction or a paragraph differs from the base
--   direction of the context, then the interpretation of
--   'AlignLeft' and 'AlignRight' are swapped.
--
layoutSetAutoDir :: PangoLayout -> Bool -> IO ()
layoutSetAutoDir (PangoLayout _ pl) j =
  {#call unsafe layout_set_auto_dir#} pl (fromBool j)

-- | Retrieve the auto direction flag.
--
-- * See 'layoutSetAutoDir'.
--
layoutGetAutoDir :: PangoLayout -> IO Bool
layoutGetAutoDir (PangoLayout _ pl) =
  liftM toBool $ {#call unsafe layout_get_auto_dir#} pl
#endif


-- | Enumerate to which side incomplete lines are flushed.
--
{#enum PangoAlignment as LayoutAlignment {underscoreToCase}#}

-- | Set how this paragraph is aligned.
--
-- * Sets the alignment for the layout (how partial lines are
--   positioned within the horizontal space available.)
--
layoutSetAlignment :: PangoLayout -> LayoutAlignment -> IO ()
layoutSetAlignment (PangoLayout _ pl) am =
  {#call unsafe layout_set_alignment#} pl ((fromIntegral.fromEnum) am)


-- | Get the alignment for the layout.
--
layoutGetAlignment :: PangoLayout -> IO LayoutAlignment
layoutGetAlignment (PangoLayout _ pl) = liftM (toEnum.fromIntegral) $
  {#call unsafe layout_get_alignment#} pl

-- | Specify where the Tab stop appears relative to the text.
--
-- * Only Tab stops that align text to the left are supported right now.
--
{#enum PangoTabAlign as TabAlign {underscoreToCase}#}

-- | A Tab position.
--
type TabPosition = (Double, TabAlign)

-- | Set a list of Tab positoins.
--
layoutSetTabs :: PangoLayout -> [TabPosition] -> IO ()
layoutSetTabs (PangoLayout _ pl) tabs = do
  let len = fromIntegral (length tabs)
  tabPtr <- {#call unsafe tab_array_new#} len (fromBool False)
  mapM_ (\(idx, (pos, align)) ->
         {#call unsafe tab_array_set_tab#} tabPtr idx
            (fromIntegral (fromEnum align)) (puToInt pos)) (zip [0..] tabs)
  {#call unsafe layout_set_tabs#} pl tabPtr
  {#call unsafe tab_array_free#} tabPtr

-- | Reset the original set of Tab positions.
--
-- * Restore the default which is a Tab stop every eight characters.
--
layoutResetTabs :: PangoLayout -> IO ()
layoutResetTabs (PangoLayout _ pl) = {#call unsafe layout_set_tabs#} pl nullPtr

-- | Retrieve the list of current Tab positions.
--
-- * If no Tab position where set, @Nothing@ is returned. In this case, Tab
--   positions are implicit at every eight characters.
--
layoutGetTabs :: PangoLayout -> IO (Maybe [TabPosition])
layoutGetTabs (PangoLayout _ pl) = do
  tabPtr <- {#call unsafe layout_get_tabs#} pl
  if tabPtr == nullPtr then return Nothing else liftM Just $ do
    len <- {#call unsafe tab_array_get_size#} tabPtr
    mapM (\idx -> alloca $ \posPtr -> alloca $ \alignPtr -> do
          {#call unsafe tab_array_get_tab#} tabPtr idx alignPtr posPtr
          align <- peek alignPtr
          pos <- peek posPtr
          return (intToPu pos, toEnum (fromIntegral align))) [0..len-1]

-- | Honor newlines or not.
--
-- * If @honor@ is @True@, do not treat newlines and
--   similar characters as paragraph separators; instead, keep all text in
--   a single paragraph, and display a glyph for paragraph separator
--   characters. Used when you want to allow editing of newlines on a
--   single text line.
--
layoutSetSingleParagraphMode :: PangoLayout -> Bool -> IO ()
layoutSetSingleParagraphMode (PangoLayout _ pl) honor =
  {#call unsafe layout_set_single_paragraph_mode#} pl (fromBool honor)

-- | Retrieve if newlines are honored.
--
-- * See 'layoutSetSingleParagraphMode'.
--
layoutGetSingleParagraphMode :: PangoLayout -> IO Bool
layoutGetSingleParagraphMode (PangoLayout _ pl) =
  liftM toBool $ {#call unsafe layout_get_single_paragraph_mode#} pl

-- a function is missing here

-- | Converts a device unit to a character index.
--
-- * Converts from @x@ and @y@ position within a layout to the index of
--   the closest character. If the @y@ position is not inside the layout,
--   the closest position is chosen (the position will be clamped inside
--   the layout). If the @x@ position is not within the layout, then the
--   start or the end of the line is chosen. If either the @x@ or @y@
--   positions were not inside the layout, then the function returns @False@;
--   on an exact hit, it returns @True@.
--
-- * The function returns the flag for the exact hit and the index into
--   the string. The third value is zero if the character corresponds to
--   one grapheme. If the grapheme is the result of a cluster, this value
--   may be greater than one, indicating where in the grapheme the position
--   lies. Zero represents the trailing edge on the grapheme.
--
layoutXYToIndex :: PangoLayout -> Double -- ^ the @x@ position
                -> Double -- ^ the @y@ position
                -> IO (Bool, Int, Int)
layoutXYToIndex (PangoLayout psRef pl) x y =
  alloca $ \idxPtr -> alloca $ \trailPtr -> do
    res <- {#call unsafe layout_xy_to_index#} pl (puToInt x) (puToInt y)
      idxPtr trailPtr
    idx <- peek idxPtr
    trail <- peek trailPtr
    (PangoString uc _ _) <- readIORef psRef
    return (toBool res,
            ofsFromUTF (fromIntegral idx) uc,
            ofsFromUTF (fromIntegral trail) uc)

-- | Return the rectangle of the glyph at the given index.
--
-- * Converts from an index within a 'PangoLayout' to the onscreen position
--   corresponding to the grapheme at that index, which is represented as
--   rectangle. Note that, given a @PangoRectangle x y width height@, @x@
--   is always the leading edge of the grapheme and @x + width@ the
--   trailing edge of the grapheme. If the directionality of the grapheme
--   is right-to-left, then @width@ will be negative.
--
layoutIndexToPos :: PangoLayout -> Int -> IO PangoRectangle
layoutIndexToPos (PangoLayout psRef plr) pos = do
  (PangoString uc _ _) <- readIORef psRef
  alloca $ \rectPtr -> do
    {#call unsafe layout_index_to_pos#} plr (fromIntegral (ofsToUTF pos uc))
                                            (castPtr rectPtr)
    peek rectPtr

twoRect :: (Ptr () -> Ptr () -> IO ()) ->
           IO (PangoRectangle, PangoRectangle)
twoRect f =
  alloca $ \inkPtr -> alloca $ \logPtr -> do
  f (castPtr inkPtr) (castPtr logPtr)
  ink <- peek inkPtr
  log <- peek logPtr
  return (ink, log)

-- | Return a cursor position.
--
-- * Given an index within a layout, determines the positions that of the
--   strong and weak cursors if the insertion point is at that index.
--   The position of each cursor is stored as a zero-width rectangle.
--   The strong cursor location is the location where characters of the
--   directionality equal to the base direction of the layout are inserted.
--   The weak cursor location is the location where characters of the
--   directionality opposite to the base direction of the layout are
--   inserted. The first element of the typle is the strong position,
--   the second the weak.
--
layoutGetCursorPos :: PangoLayout -> Int ->
                      IO (PangoRectangle, PangoRectangle) -- ^ @(strong, weak)@
layoutGetCursorPos (PangoLayout psRef plr) pos = do
  (PangoString uc _ _) <- readIORef psRef
  twoRect $ {#call unsafe layout_get_cursor_pos#} plr (fromIntegral (ofsToUTF pos uc))

-- | A new cursor position.
--
-- See 'layoutMoveCursorVisually'.
--
data CursorPos
  = CursorPosPrevPara -- ^ The cursor should move to the previous paragraph.
  | CursorPos Int Int -- ^ The sum of the indices is the new cursor position.
  | CursorPosNextPara -- ^ The cursor should advance to the next paragraph.

-- | Move a cursor visually.
--
-- * Compute a new cursor position from a previous cursor position. A value
--   of @True@ for the direction will move it to the right, independant of
--   the underlying direction. Hence the cursor position might jump if
--   left-to-right text is mixed with right-to-left text.
--
-- * The first flag should be @True@ if this cursor is the strong cursor.
--   The strong cursor is the cursor of the base direction of the current
--   layout (see 'layoutSetAutoDir'). The weak cursor is that of the
--   opposite direction.
--
-- * The previous cursor position is given by @idx@. If this text at this
--   position is a cluster, the cursor will only move to the end or
--   beginning of the cluster as opposed to past the next character.
--   The return value is either 'CursorPosNextPara' if the cursor moved
--   beyond this paragraph, it is 'CursorPosPrevPara' if the cursor moved
--   in front of this paragraph and it is 'CursorPos' @idx@ @trail@ to denote
--   the new cursor position @idx@. Note that @idx@ will always denote an
--   insertion point, that is, @idx@ will never point into the middle of
--   a cluster. The @trail@ value can contain a positive
--   value if the current cursor position is at the end of the current line.
--   In this case, @idx@ points past the last character of this line while
--   @trail@ contains the number of characters that are reponsible for the
--   line break such as newlines. The actual cursor position is always
--   @idx+trail@ where the visual cursor should be shown.
--
layoutMoveCursorVisually :: PangoLayout
                         -> Bool -- ^ @True@ to create a strong cursor.
                         -> Int -- ^ The previous position.
                         -> Bool -- ^ @True@ if the cursor should move right.
                         -> IO CursorPos
layoutMoveCursorVisually (PangoLayout psRef plr) strong index dir = do
  (PangoString uc _ _) <- readIORef psRef
  alloca $ \idxPtr -> alloca $ \trailPtr -> do
    {#call unsafe layout_move_cursor_visually#} plr (fromBool strong)
      (fromIntegral (ofsToUTF index uc)) 0
      (if dir then 1 else (-1)) idxPtr trailPtr
    idx <- peek idxPtr
    trail <- peek trailPtr
    return (if idx==(-1) then CursorPosPrevPara else
            if idx==maxBound then CursorPosNextPara else
            CursorPos (ofsFromUTF (fromIntegral idx) uc) (fromIntegral trail))

-- | Computes the logical and ink extents of the 'PangoLayout'.
--
-- Logical extents are usually what you want for positioning things. Note that
-- both extents may have non-zero x and y. You may want to use those to offset
-- where you render the layout. Not doing that is a very typical bug that
-- shows up as right-to-left layouts not being correctly positioned in a
-- layout with a set width.
--
-- Layout coordinates begin at the top left corner of the layout.
--
layoutGetExtents :: PangoLayout
                 -> IO (PangoRectangle, PangoRectangle) -- ^ @(ink, logical)@
layoutGetExtents (PangoLayout _ pl) =
  twoRect $ {#call unsafe layout_get_extents#} pl

-- | Compute the physical size of the layout.
--
-- * Computes the ink and the logical size of the 'Layout' in device units,
--   that is, pixels for a screen. Identical to 'layoutGetExtents' and
--   converting the 'Double's in the 'PangoRectangle' to integers.
--
layoutGetPixelExtents :: PangoLayout -> IO (Rectangle, Rectangle) -- ^ @(ink, logical)@
layoutGetPixelExtents (PangoLayout _ pl) =
  alloca $ \inkPtr -> alloca $ \logPtr -> do
  {#call unsafe layout_get_pixel_extents#} pl (castPtr inkPtr) (castPtr logPtr)
  ink <- peekIntPangoRectangle inkPtr
  log <- peekIntPangoRectangle logPtr
  return (ink,log)

-- | Ask for the number of lines in this layout.
--
layoutGetLineCount :: PangoLayout -> IO Int
layoutGetLineCount (PangoLayout _ pl) = liftM fromIntegral $
  {#call unsafe layout_get_line_count#} pl

-- | Extract a single lines of the layout.
--
-- * The given index starts from 0. The function throws an
--   'ArrayException' if the index is out of bounds.
--
-- * The lines of each layout are regenerated if any attribute changes.
--   Thus the returned list does not reflect the current state of lines
--   after a change has been made.
--
layoutGetLine :: PangoLayout -> Int -> IO LayoutLine
layoutGetLine (PangoLayout psRef pl) idx = do
  llPtr <-
#if PANGO_VERSION_CHECK(1,16,0)
    -- use the optimised read-only version if available
    {#call unsafe layout_get_line_readonly#}
#else
    {#call unsafe layout_get_line#}
#endif
      pl (fromIntegral idx)
  if llPtr==nullPtr then
     throwIO (IndexOutOfBounds
      ("Graphics.Rendering.Pango.Layout.layoutGetLine: "++
       "no line at index "++show idx)) else do
  ll <- makeNewLayoutLineRaw llPtr
  {#call unsafe layout_line_ref#} ll
  return (LayoutLine psRef ll)

-- | Extract the lines of the layout.
--
-- * The lines of each layout are regenerated if any attribute changes.
--   Thus the returned list does not reflect the current state of lines
--   after a change has been made.
--
layoutGetLines :: PangoLayout -> IO [LayoutLine]
layoutGetLines (PangoLayout psRef pl) = do
  listPtr <-
#if PANGO_VERSION_CHECK(1,16,0)
    -- use the optimised read-only version if available
    {#call unsafe layout_get_lines_readonly#}
#else
    {#call unsafe layout_get_lines#}
#endif
    pl
  list <- readGSList listPtr
  pls <- mapM makeNewLayoutLineRaw list
  mapM_ {#call unsafe layout_line_ref#} pls
  return (map (LayoutLine psRef) pls)

-- | Create an iterator to examine a layout.
--
layoutGetIter :: PangoLayout -> IO LayoutIter
layoutGetIter (PangoLayout psRef pl) = do
  iterPtr <- {#call unsafe layout_get_iter#} pl
  liftM (LayoutIter psRef) $ makeNewLayoutIterRaw iterPtr

-- | Move to the next 'GlyphItem'.
--
-- * Returns @False@ if this was the last item in the layout.
--
layoutIterNextItem :: LayoutIter -> IO Bool
layoutIterNextItem (LayoutIter _ li) =
  liftM toBool $ {#call unsafe layout_iter_next_run#} li

-- | Move to the next char.
--
-- * Returns @False@ if this was the last char in the layout.
--
layoutIterNextChar :: LayoutIter -> IO Bool
layoutIterNextChar (LayoutIter _ li) =
  liftM toBool $ {#call unsafe layout_iter_next_char#} li

-- | Move to the next cluster.
--
-- * Returns @False@ if this was the last cluster in the layout.
--
layoutIterNextCluster :: LayoutIter -> IO Bool
layoutIterNextCluster (LayoutIter _ li) =
  liftM toBool $ {#call unsafe layout_iter_next_cluster#} li

-- | Move to the next line.
--
-- * Returns @False@ if this was the last line in the layout.
--
layoutIterNextLine :: LayoutIter -> IO Bool
layoutIterNextLine (LayoutIter _ li) =
  liftM toBool $ {#call unsafe layout_iter_next_line#} li

-- | Check if the iterator is on the last line.
--
-- * Returns @True@ if the iterator is on the last line of this
--   paragraph.
--
layoutIterAtLastLine :: LayoutIter -> IO Bool
layoutIterAtLastLine (LayoutIter _ li) =
  liftM toBool $ {#call unsafe layout_iter_at_last_line#} li

-- | Get the character index.
--
-- * Note that iterating forward by char moves in visual order, not
--   logical order, so indexes may not be sequential. Also, the index
--   may be equal to the length of the text in the layout.
--
layoutIterGetIndex :: LayoutIter -> IO Int
layoutIterGetIndex (LayoutIter psRef li) = do
  (PangoString uc _ _) <- readIORef psRef
  idx <- {#call unsafe layout_iter_get_index#} li
  return (ofsFromUTF (fromIntegral idx) uc)

-- | Query the vertical position within the layout.
--
-- * Gets the y position of the current line's baseline (origin at top
--   left of the entire layout).
--
layoutIterGetBaseline :: LayoutIter -> IO Double
layoutIterGetBaseline (LayoutIter _ li) =
  liftM intToPu $ {#call unsafe pango_layout_iter_get_baseline#} li

#if PANGO_VERSION_CHECK(1,2,0)
-- | Retrieve the current 'GlyphItem' under the iterator.
--
-- * Each 'LayoutLine' contains a list of 'GlyphItem's. This function
--   returns the 'GlyphItem' under the current iterator. If the iterator
--   is positioned past the last charactor of the paragraph, the function
--   returns @Nothing@.
--
layoutIterGetItem :: LayoutIter -> IO (Maybe GlyphItem)
layoutIterGetItem (LayoutIter psRef li) = do
  giPtr <- {#call unsafe layout_iter_get_run#} li
  if giPtr==nullPtr then return Nothing else liftM Just $ do
    (PangoString uc _ _) <- readIORef psRef
    pirPtr <- {#get PangoGlyphItem.item#} giPtr
    gsrPtr <- {#get PangoGlyphItem.glyphs#} giPtr
    let dummy = {#call unsafe pango_item_copy#}
    let dummy = {#call unsafe pango_glyph_string_copy#}
    pirPtr' <- pango_item_copy pirPtr
    gsrPtr' <- pango_glyph_string_copy gsrPtr
    pir <- makeNewPangoItemRaw pirPtr'
    gsr <- makeNewGlyphStringRaw gsrPtr'
    ps <- readIORef psRef
    return (GlyphItem (PangoItem ps pir) gsr)
#endif

-- | Extract the line under the iterator.
--
layoutIterGetLine :: LayoutIter -> IO (Maybe LayoutLine)
layoutIterGetLine (LayoutIter psRef li) = do
  llPtr <- liftM castPtr $ {#call unsafe pango_layout_iter_get_line#} li
  if (llPtr==nullPtr) then return Nothing else do
    ll <- makeNewLayoutLineRaw llPtr
    {#call unsafe layout_line_ref#} ll
    return (Just (LayoutLine psRef ll))

-- | Retrieve a rectangle surrounding a character.
--
-- * Get the extents of the current character
--   (origin is the top left of the entire layout). Only logical extents
--   can sensibly be obtained for characters; ink extents make sense only
--   down to the level of clusters.
--
layoutIterGetCharExtents :: LayoutIter -> IO PangoRectangle
layoutIterGetCharExtents (LayoutIter _ li) = alloca $ \logPtr ->
  {#call unsafe layout_iter_get_char_extents#} li (castPtr logPtr) >>
  peek logPtr

-- | Compute the physical size of the cluster.
--
-- * Computes the ink and the logical size of the cluster pointed to by
--   'LayoutIter'.
--
layoutIterGetClusterExtents :: LayoutIter -> IO (PangoRectangle, PangoRectangle) -- ^ @(ink, logical)@
layoutIterGetClusterExtents (LayoutIter _ li) =
  twoRect $ {#call unsafe layout_iter_get_cluster_extents#} li

-- | Compute the physical size of the run.
--
-- * Computes the ink and the logical size of the run pointed to by
--   'LayoutIter'.
--
layoutIterGetRunExtents :: LayoutIter -> IO (PangoRectangle, PangoRectangle)
layoutIterGetRunExtents (LayoutIter _ li) =
  twoRect $ {#call unsafe layout_iter_get_run_extents#} li

-- | Retrieve vertical extent of this line.
--
-- * Divides the vertical space in the 'PangoLayout' being
--   iterated over between the lines in the layout, and returns the
--   space belonging to the current line. A line's range includes the
--   line's logical extents, plus half of the spacing above and below
--   the line, if 'layoutSetSpacing' has been called
--   to set layout spacing. The y positions are in layout coordinates
--   (origin at top left of the entire layout).
--
-- * The first element in the returned tuple is the start, the second is
--   the end of this line.
--
layoutIterGetLineYRange :: LayoutIter -> IO (Double, Double)
layoutIterGetLineYRange (LayoutIter _ li) =
  alloca $ \sPtr -> alloca $ \ePtr -> do
  {#call unsafe layout_iter_get_line_extents#} li (castPtr sPtr) (castPtr ePtr)
  start <- peek sPtr
  end <- peek ePtr
  return (intToPu start, intToPu end)

-- | Compute the physical size of the line.
--
-- * Computes the ink and the logical size of the line pointed to by
--   'LayoutIter'. See 'layoutGetExtents'.
--
-- * Extents are in layout coordinates (origin is the top-left corner
--   of the entire 'PangoLayout'). Thus the extents returned
--   by this function will be the same width\/height but not at the
--   same x\/y as the extents returned from
--   'layoutLineGetExtents'.
--
layoutIterGetLineExtents :: LayoutIter -> IO (PangoRectangle, PangoRectangle)
layoutIterGetLineExtents (LayoutIter _ li) =
  twoRect $ {#call unsafe layout_iter_get_line_extents#} li

-- | Compute the physical size of the line.
--
-- * Computes the ink and the logical size of the 'LayoutLine'.
--   See 'layoutGetExtents'.
--
layoutLineGetExtents :: LayoutLine -> IO (PangoRectangle, PangoRectangle)
layoutLineGetExtents (LayoutLine _ ll) =
  twoRect $ {#call unsafe layout_line_get_extents#} ll

-- | Compute the physical size of the line.
--
-- * Computes the ink and the logical size of the 'LayoutLine'.
--   See 'layoutGetExtents'. The returned values are in device units, that
--   is, pixels for the screen and points for printers.
--
layoutLineGetPixelExtents :: LayoutLine -> IO (Rectangle, Rectangle) -- ^ (ink, logical)
layoutLineGetPixelExtents (LayoutLine _ ll) =
  alloca $ \inkPtr -> alloca $ \logPtr -> do
  {#call unsafe layout_line_get_pixel_extents#} ll (castPtr inkPtr) (castPtr logPtr)
  ink <- peekIntPangoRectangle inkPtr
  log <- peekIntPangoRectangle logPtr
  return (ink,log)

-- | Request the horizontal position of a character.
--
layoutLineIndexToX :: LayoutLine
                   -> Int -- ^ the index into the string
                   -> Bool -- ^ return the beginning (@False@) or the end
                            -- of the character
                   -> IO Double
layoutLineIndexToX (LayoutLine psRef ll) pos beg =
  alloca $ \intPtr -> do
    (PangoString uc _ _) <- readIORef psRef
    {#call unsafe layout_line_index_to_x#} ll (fromIntegral (ofsToUTF pos uc))
      (fromBool beg) intPtr
    liftM intToPu $ peek intPtr


-- | Request the character index of a given horizontal position.
--
-- * Converts from an x offset to the index of the corresponding
--   character within the text of the layout. If the @x@ parameter is
--   outside the line, a triple @(False, index, trailing)@ is returned
--   where @index@ and @trailing@ will point to the very
--   first or very last position in the line. This notion of first and last
--   position is based on the direction of the paragraph; for example,
--   if the direction is right-to-left, then an @x@ position to the
--   right of the line results in 0 being returned for @index@ and
--   @trailing@. An @x@ position to the left of the line results in
--   @index@ pointing to the (logical) last grapheme in the line and
--   trailing pointing to the number of characters in that grapheme.
--   The reverse is true for a left-to-right line. If the boolean flag in
--   the result is @True@ then @x@ was within the layout line and
--   @trailing@ indicates where in a cluster the @x@ position lay. It is
--   0 for the trailing edge of the cluster.
--
layoutLineXToIndex :: LayoutLine
                   -> Double -- ^ The @x@ position.
                   -> IO (Bool, Int, Int)
layoutLineXToIndex (LayoutLine psRef ll) pos =
  alloca $ \idxPtr -> alloca $ \trailPtr -> do
    (PangoString uc _ _) <- readIORef psRef
    inside <- {#call unsafe layout_line_x_to_index#} ll
      (puToInt pos) idxPtr trailPtr
    idx <- peek idxPtr
    trail <- peek trailPtr
    return (toBool inside, ofsFromUTF (fromIntegral idx) uc,
            fromIntegral trail)

-- | Retrieve bounding boxes for a given piece of text contained in this
--   'LayoutLine'.
--
-- * The result is a list to accommodate for mixed left-to-right and
--   right-to-left text. Even if the text is not mixed, several
--   ranges might be returned that are adjacent. The ranges are always
--   sorted from left to right. The values are with respect to the left
--   edge of the entire layout, not with respect to the line (which might
--   be indented or not left aligned).
--
layoutLineGetXRanges :: LayoutLine -- ^ The line of interest.
                     -> Int -- ^ The index of the start character
                            -- (counting from 0). If this value is
                            -- less than the start index for the line,
                            -- then the first range will extend all the
                            -- way to the leading edge of the layout.
                            -- Otherwise it will start at the leading
                            -- edge of the first character.
                     -> Int -- ^ The index after the last character.
                            -- If this value is greater than the end
                            -- index for the line, then the last range
                            -- will extend all the way to the trailing
                            -- edge of the layout. Otherwise, it will end
                            -- at the trailing edge of the last
                            -- character.
                     -> IO [(Double, Double)]
layoutLineGetXRanges (LayoutLine psRef ll) start end = do
  PangoString uc _ _ <- readIORef psRef
  alloca $ \arrPtr -> alloca $ \szPtr -> do
    {#call unsafe layout_line_get_x_ranges#} ll
      (fromIntegral (ofsToUTF start uc))
      (fromIntegral (ofsToUTF end uc))
      arrPtr szPtr
    sz <- peek szPtr
    arr <- peek arrPtr
    elems <- peekArray (2*fromIntegral sz) (castPtr arr:: Ptr {#type gint#})
    {#call unsafe g_free#} (castPtr arr)
    let toRange (s:e:rs) = (intToPu s, intToPu e):toRange rs
        toRange [] = []
    return (toRange elems)

