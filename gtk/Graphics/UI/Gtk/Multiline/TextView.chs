-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget TextView
--
--  Author : Axel Simon
--
--  Created: 23 February 2002
--
--  Version $Revision: 1.3 $ from $Date: 2005/02/25 01:11:36 $
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
-- TODO
--
-- If PangoTabArray is bound: do textViewSetTabs and textViewGetTabs
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Widget that displays a 'TextBuffer'
--
module Graphics.UI.Gtk.Multiline.TextView (
-- * Description
-- 
-- | You may wish to begin by reading the text widget conceptual overview
-- which gives an overview of all the objects and data types related to the
-- text widget and how they work together.
--
-- * Throughout we distinguish between buffer coordinates which are pixels
--   with the origin at the upper left corner of the first character on the
--   first line. Window coordinates are relative to the top left pixel which
--   is visible in the current 'TextView'. Coordinates from Events 
--   are in the latter relation. The conversion can be done with 
--   'textViewWindowToBufferCoords'.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----TextView
-- |
-- |
-- |  'GObject'
-- |   +----TextChildAnchor
-- @

-- * Types
  TextView,
  TextViewClass,
  TextChildAnchor,
  TextChildAnchorClass,
  castToTextView,
  DeleteType(..),
  DirectionType(..),
  Justification(..),
  MovementStep(..),
  TextWindowType(..),
  WrapMode(..),

-- * Constructors
  textViewNew,
  textViewNewWithBuffer,

-- * Methods
  textViewSetBuffer,
  textViewGetBuffer,
  textViewScrollToMark,
  textViewScrollToIter,
  textViewScrollMarkOnscreen,
  textViewMoveMarkOnscreen,
  textViewPlaceCursorOnscreen,
  textViewGetLineAtY,
  textViewGetLineYrange,
  textViewGetIterAtLocation,
  textViewBufferToWindowCoords,
  textViewWindowToBufferCoords,
  textViewGetWindow,
  textViewGetWindowType,
  textViewSetBorderWindowSize,
  textViewGetBorderWindowSize,
  textViewForwardDisplayLine,
  textViewBackwardDisplayLine,
  textViewForwardDisplayLineEnd,
  textViewBackwardDisplayLineEnd,
  textViewForwardDisplayLineStart,
  textViewBackwardDisplayLineStart,
  textViewStartsDisplayLine,
  textViewMoveVisually,
  textViewAddChildAtAnchor,
  textChildAnchorNew,
  textChildAnchorGetWidgets,
  textChildAnchorGetDeleted,
  textViewAddChildInWindow,
  textViewMoveChild,
  textViewSetWrapMode,
  textViewGetWrapMode,
  textViewSetEditable,
  textViewGetEditable,
  textViewSetCursorVisible,
  textViewGetCursorVisible,
  textViewSetPixelsAboveLines,
  textViewGetPixelsAboveLines,
  textViewSetPixelsBelowLines,
  textViewGetPixelsBelowLines,
  textViewSetPixelsInsideWrap,
  textViewGetPixelsInsideWrap,
  textViewSetJustification,
  textViewGetJustification,
  textViewSetLeftMargin,
  textViewGetLeftMargin,
  textViewSetRightMargin,
  textViewGetRightMargin,
  textViewSetIndent,
  textViewGetIndent,
  textViewGetDefaultAttributes,

-- * Signals
  onCopyClipboard,
  afterCopyClipboard,
  onCutClipboard,
  afterCutClipboard,
  onDeleteFromCursor,
  afterDeleteFromCursor,
  onInsertAtCursor,
  afterInsertAtCursor,
  onMoveCursor,
  afterMoveCursor,
  onMoveFocus,
  afterMoveFocus,
  onPageHorizontally,
  afterPageHorizontally,
  onPasteClipboard,
  afterPasteClipboard,
  onPopulatePopup,
  afterPopulatePopup,
  onSetAnchor,
  afterSetAnchor,
  onSetScrollAdjustments,
  afterSetScrollAdjustments,
  onToggleOverwrite,
  afterToggleOverwrite) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
import System.Glib.GObject		(makeNewGObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
{#import Graphics.UI.Gtk.Multiline.TextIter#}
{#import Graphics.UI.Gtk.Multiline.TextTag#}
import Graphics.UI.Gtk.General.Enums	(TextWindowType(..), DeleteType(..),
					 DirectionType(..), Justification(..),
					 MovementStep(..), WrapMode(..))
import System.Glib.GList		(fromGList)
import Graphics.UI.Gtk.General.Structs	(Rectangle(..))

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Create a new 'TextView' widget with a
-- default 'TextBuffer'.
--
textViewNew :: IO TextView
textViewNew  = makeNewGObject mkTextView $ liftM castPtr 
  {#call unsafe text_view_new#}

-- | Create a new 'TextView' widget with
-- the given 'TextBuffer'.
--
textViewNewWithBuffer :: TextBuffer -> IO TextView
textViewNewWithBuffer tb = makeNewGObject mkTextView $ liftM castPtr $
  {#call unsafe text_view_new_with_buffer#} tb

--------------------
-- Methods

-- | Set the 'TextBuffer' for a given 'TextView' widget.
--
textViewSetBuffer :: TextViewClass tv => tv -> TextBuffer -> IO ()
textViewSetBuffer tv tb = 
  {#call text_view_set_buffer#} (toTextView tv) tb

-- | Returns the 'TextBuffer' being displayed by this text view.
--
textViewGetBuffer :: TextViewClass tv => tv -> IO TextBuffer
textViewGetBuffer tv = makeNewGObject mkTextBuffer $
  {#call unsafe text_view_get_buffer#} (toTextView tv)

-- | Scroll to the position of the supplied
-- 'TextMark'.
--
-- * Supplying @xalign@, @yalign@ gives a goal position of
--   the 'TextMark' within screen bounds. 0,0 means left, top and
--   1.0,1.0 means right, bottom.
--
-- * Supply @Nothing@ if the goal is to bring the position into 
--   view with the minimum of scrolling.
--
-- * @withinMargin@ is within \[0.0 .. 0.5) and imposes an extra margin
--   at all four sides of the window within which @xalign@ and
--   @yalign@ are evaluated.
--
-- * The line distances are calculated in an idle handler. Calling this
--   function ensures that the line heights are indeed evaluated before the
--   scroll is carried out.
--
textViewScrollToMark :: TextViewClass tv => tv -> TextMark -> Double ->
                        Maybe (Double, Double) -> IO ()
textViewScrollToMark tv tm withinMargin (Just (xalign, yalign)) = 
  {#call unsafe text_view_scroll_to_mark#} (toTextView tv) tm 
  (realToFrac withinMargin) 1 (realToFrac xalign) (realToFrac yalign)
textViewScrollToMark tv tm withinMargin Nothing = 
  {#call unsafe text_view_scroll_to_mark#} (toTextView tv) tm 
  (realToFrac withinMargin) 0 (0.0) (0.0)

-- | Scroll to the position of the supplied
-- 'TextIter'.
--
-- * The position might not be correct due to the delayed calculation of the
--   line heights.
--
-- * Returns True if the function actually scrolled.
--
textViewScrollToIter :: TextViewClass tv => tv -> TextIter -> Double ->
                        Maybe (Double, Double) -> IO Bool
textViewScrollToIter tv ti withinMargin (Just (xalign, yalign)) = 
  liftM toBool $ {#call unsafe text_view_scroll_to_iter#} (toTextView tv) 
    ti (realToFrac withinMargin) 1 (realToFrac xalign) 
    (realToFrac yalign)
textViewScrollToIter tv ti withinMargin Nothing = liftM toBool $
  {#call unsafe text_view_scroll_to_iter#} (toTextView tv) ti 
    (realToFrac withinMargin) 0 (0.0) (0.0)

-- | Scroll the visible area of the widget
-- so the 'TextMark' becomes visible.
--
-- * This call is equivalent to 'textViewScrollToMark' tm 0.0
--   Nothing tv.
--
textViewScrollMarkOnscreen :: TextViewClass tv => tv -> TextMark -> IO ()
textViewScrollMarkOnscreen tv tm = 
  {#call unsafe text_view_scroll_mark_onscreen#} (toTextView tv) tm

-- | Move a 'TextMark' within the
-- buffer until it is in the currently visible area of the widget.
--
-- * Returns True if the Mark was moved.
--
textViewMoveMarkOnscreen :: TextViewClass tv => tv -> TextMark -> IO Bool
textViewMoveMarkOnscreen tv tm = liftM toBool $ 
  {#call unsafe text_view_move_mark_onscreen#} (toTextView tv) tm

-- | Move the cursor within the buffer
-- until it is in the currently visible area of the widget.
--
-- * Returns True if the Mark was moved.
--
textViewPlaceCursorOnscreen :: TextViewClass tv => tv -> IO Bool
textViewPlaceCursorOnscreen tv = liftM toBool $ 
  {#call unsafe text_view_place_cursor_onscreen#} (toTextView tv)

-- | Get the currently visible rectangle.
--
-- * Use 'textViewBufferToWindowCoords' to convert into window 
-- coordinates.
--
textViewGetVisibleRect :: TextViewClass tv => tv -> IO Rectangle
textViewGetVisibleRect tv = alloca $ \rectPtr -> do
  {#call unsafe text_view_get_visible_rect#} (toTextView tv) (castPtr rectPtr)
  peek rectPtr


-- | Get a rectangle that roughly contains the 
-- character at 'TextIter'.
--
-- * Use 'textViewBufferToWindowCoords' to convert into window 
--   cooridnates.
--
textViewGetIterLocation :: TextViewClass tv => tv -> TextIter -> IO Rectangle
textViewGetIterLocation tv tm = alloca $ \rectPtr -> do
  {#call unsafe text_view_get_iter_location#} (toTextView tv) tm 
    (castPtr rectPtr)
  peek rectPtr



-- | Get the 'TextIter' at the start of the
-- line containing the coordinate @y@.
--
-- * @y@ is in buffer coordinates.
--
-- * Returns the 'TextIter' and the top of the line.
--
textViewGetLineAtY :: TextViewClass tv => tv -> Int -> IO (TextIter,Int)
textViewGetLineAtY tv y = do
  iter <- makeEmptyTextIter
  lineTop <- liftM fromIntegral $ alloca $ \ltPtr -> do
    {#call unsafe text_view_get_line_at_y#} (toTextView tv) iter 
      (fromIntegral y) ltPtr
    peek ltPtr
  return (iter, lineTop)

-- | Get the y coordinate of the top and the
-- height of the line 'TextIter' is on.
--
textViewGetLineYrange :: TextViewClass tv => tv -> TextIter -> IO (Int,Int)
textViewGetLineYrange tv ti = alloca $ \yPtr -> alloca $ \heightPtr -> do
  {#call unsafe text_view_get_line_yrange#} (toTextView tv) ti yPtr heightPtr
  y <- peek yPtr
  height <- peek heightPtr
  return (fromIntegral y, fromIntegral height)

-- | Retrieves the 'TextIter' at
-- buffer coordinates @x@ and @y@.
--
textViewGetIterAtLocation :: TextViewClass tv => tv -> Int -> Int ->
                             IO TextIter
textViewGetIterAtLocation tv x y = do
  iter <- makeEmptyTextIter
  {#call unsafe text_view_get_iter_at_location#} (toTextView tv) iter 
    (fromIntegral x) (fromIntegral y)
  return iter

-- | Convert buffer cooridnates into
-- window coordinates.
--
textViewBufferToWindowCoords :: TextViewClass tv => tv -> TextWindowType ->
                                (Int,Int) -> IO (Int,Int)
textViewBufferToWindowCoords tv wt (x,y) = 
  alloca $ \xPtr -> alloca $ \yPtr -> do
    {#call unsafe text_view_buffer_to_window_coords#} (toTextView tv) 
      ((fromIntegral.fromEnum) wt) (fromIntegral x) (fromIntegral y) 
      xPtr yPtr
    x' <- peek xPtr
    y' <- peek yPtr
    return (fromIntegral x', fromIntegral y')

-- | Convert window cooridnates into
-- buffer coordinates.
--
textViewWindowToBufferCoords :: TextViewClass tv => tv -> TextWindowType ->
                                (Int,Int) -> IO (Int,Int)
textViewWindowToBufferCoords tv wt (x,y) = 
  alloca $ \xPtr -> alloca $ \yPtr -> do
    {#call unsafe text_view_window_to_buffer_coords#} (toTextView tv) 
      ((fromIntegral.fromEnum) wt) (fromIntegral x) (fromIntegral y) xPtr yPtr
    x' <- peek xPtr
    y' <- peek yPtr
    return (fromIntegral x', fromIntegral y')


-- | Get the underlying 'DrawWindow'.
--
-- * The 'TextWindowType' determines which window of the
--   'TextWidget' we would like to receive.
--
-- * Returns Nothing if there is no 'DrawWindow' of the specified type.
--
textViewGetWindow :: TextViewClass tv => tv -> TextWindowType ->
                     IO (Maybe DrawWindow)
textViewGetWindow tv wt = do
  winPtr <- {#call unsafe text_view_get_window#} (toTextView tv) 
    ((fromIntegral.fromEnum) wt)
  if winPtr==nullPtr then return Nothing else liftM Just $
    makeNewGObject mkDrawWindow (return winPtr)

-- | Retrieve the type of window the
-- 'TextView' widget contains.
--
-- * Usually used to find out which window an event corresponds to. An
--   emission of an event signal of 'TextView' yields a
--   'DrawWindow'. This function can be used to see if the event
--   actually belongs to the main text window.
--
textViewGetWindowType :: TextViewClass tv => tv -> DrawWindow ->
                         IO TextWindowType
textViewGetWindowType tv win = liftM (toEnum.fromIntegral) $
  {#call unsafe text_view_get_window_type#} (toTextView tv) win


-- | Set the border width of the
-- 'TextView' widget.
--
-- * Sets the width of 'TextWindowLeft' or
--   'TextWindowRight', or the height of 'TextWindowTop' or
--   'TextWindowBottom'. Automatically destroys the corresponding
--   window if the size is set to 0 and creates the window if the size is set
--   to non-zero. This function can only used with the four window types
--   mentioned.
--
textViewSetBorderWindowSize :: TextViewClass tv => tv -> TextWindowType ->
                               Int -> IO ()
textViewSetBorderWindowSize tv wt size = 
  {#call unsafe text_view_set_border_window_size#} (toTextView tv) 
  ((fromIntegral.fromEnum) wt) (fromIntegral size)

-- | Retrieve the border width of the
-- specified window.
--
-- * See 'textViewSetBorderWindowSize'.
--
textViewGetBorderWindowSize :: TextViewClass tv => tv -> TextWindowType ->
                               IO Int
textViewGetBorderWindowSize tv wt = liftM fromIntegral $
  {#call unsafe text_view_get_border_window_size#} (toTextView tv) 
  ((fromIntegral.fromEnum) wt)

-- | Move the iterator forwards by one display line.
--
-- * Moves the given 'TextIter' forward by one display (wrapped)
--   line. A display line is different from a paragraph. Paragraphs are
--   separated by newlines or other paragraph separator characters. Display
--   lines are created by line-wrapping a paragraph. If wrapping is turned
--   off, display lines and paragraphs will be the same. Display lines are
--   divided differently for each view, since they depend on the view's width;
--   paragraphs are the same in all views, since they depend on the contents
--   of the 'TextBuffer'.
--
textViewForwardDisplayLine :: TextViewClass tv => tv -> TextIter -> IO Bool
textViewForwardDisplayLine tv ti = liftM toBool $
  {#call unsafe text_view_forward_display_line#} (toTextView tv) ti

-- | Move the iterator backwards by one
-- display line.
--
-- * See 'textViewForwardDisplayLine'.
--
textViewBackwardDisplayLine :: TextViewClass tv => tv -> TextIter -> IO Bool
textViewBackwardDisplayLine tv ti = liftM toBool $
  {#call unsafe text_view_backward_display_line#} (toTextView tv) ti
 
-- | Move the iterator forwards and to the end.
--
-- * Like 'textViewForwardDisplayLine' but moves to the end of 
--   the line as well.
--
textViewForwardDisplayLineEnd :: TextViewClass tv => TextIter -> tv -> IO Bool
textViewForwardDisplayLineEnd ti tv = liftM toBool $
  {#call unsafe text_view_forward_display_line_end#} (toTextView tv) ti

-- | Move the iterator backwards and to the end.
--
-- * See 'textViewForwardDisplayLineEnd'.
--
textViewBackwardDisplayLineEnd :: TextViewClass tv => tv -> TextIter -> IO Bool
textViewBackwardDisplayLineEnd tv ti = liftM toBool $
  {#call unsafe text_view_backward_display_line_start#} (toTextView tv) ti

-- | Move the iterator forwards and to the start.
--
-- * Like 'textViewForwardDisplayLine' but moves to the start of
--   the line as well.
--
textViewForwardDisplayLineStart :: TextViewClass tv => tv -> TextIter ->
                                   IO Bool
textViewForwardDisplayLineStart tv ti = liftM toBool $
  {#call unsafe text_view_forward_display_line_end#} (toTextView tv) ti

-- | Move the iterator backwards and to the start.
--
-- * See 'textViewForwardDisplayLineStart'.
--
textViewBackwardDisplayLineStart :: TextViewClass tv => tv -> TextIter ->
                                    IO Bool
textViewBackwardDisplayLineStart tv ti = liftM toBool $
  {#call unsafe text_view_backward_display_line_start#} (toTextView tv) ti

-- | Determines whether the iter is at the start of a display line.
--
-- * See 'textViewForwardDisplayLine' for an explanation of display lines vs.
-- paragraphs.
--
textViewStartsDisplayLine :: TextViewClass tv => tv -> TextIter -> IO Bool
textViewStartsDisplayLine tv ti = liftM toBool $
  {#call unsafe text_view_starts_display_line#} (toTextView tv) ti 

-- | Move the iterator a number of lines.
--
-- * The @count@ is in display lines. See 'textViewForwardDisplayLine'.
--
textViewMoveVisually :: TextViewClass tv => tv -> TextIter -> Int -> IO Bool
textViewMoveVisually tv ti count = liftM toBool $
  {#call unsafe text_view_move_visually#} (toTextView tv) ti 
    (fromIntegral count)


-- | Add a child widget in the
-- 'TextBuffer' at a given 'TextChildAnchor'.
--
textViewAddChildAtAnchor :: (TextViewClass tv , WidgetClass w) => tv -> w ->
                            TextChildAnchor -> IO ()
textViewAddChildAtAnchor tv w anchor = 
  {#call unsafe text_view_add_child_at_anchor#} (toTextView tv) (toWidget w) 
    anchor

-- | Create a new 'TextChildAnchor'.
--
-- * Using 'textBufferCreateChildAnchor' is usually simpler then
--   executing this function and 'textBufferInsertChildAnchor'.
--
textChildAnchorNew :: IO TextChildAnchor
textChildAnchorNew  = makeNewGObject mkTextChildAnchor 
  {#call unsafe text_child_anchor_new#}


-- | Retrieve all 'Widget's at this
-- 'TextChildAnchor'.
--
-- * The widgets in the returned list need to be upcasted to what they were.
--
textChildAnchorGetWidgets :: TextChildAnchor -> IO [Widget]
textChildAnchorGetWidgets tca = do
  gList <- {#call text_child_anchor_get_widgets#} tca
  wList <- fromGList gList
  mapM (makeNewObject mkWidget) (map return wList)

-- | Query if an anchor was deleted.
--
textChildAnchorGetDeleted :: TextChildAnchor -> IO Bool
textChildAnchorGetDeleted tca = liftM toBool $
  {#call unsafe text_child_anchor_get_deleted#} tca

-- | Place a widget in within the text.
--
-- * This function places a 'Widget' at an absolute pixel position
--   into the 'TextView'. Note that any scrolling will leave the
--   widget in the same spot as it was.
--
-- * The position @x@, @y@ is relative to the 
--   'DrawWindow' specified by 'TextWindowType'.
--
textViewAddChildInWindow :: (TextViewClass tv , WidgetClass w) => tv -> w ->
			    TextWindowType -> Int -> Int -> IO ()
textViewAddChildInWindow tv w twt x y = {#call text_view_add_child_in_window#}
  (toTextView tv) (toWidget w) ((fromIntegral.fromEnum) twt)
  (fromIntegral x) (fromIntegral y)

-- | Move a child widget within the
-- 'TextView'.
--
textViewMoveChild :: (TextViewClass tv , WidgetClass w) => tv -> w ->
							   Int -> Int -> IO ()
textViewMoveChild tv w x y = {#call text_view_move_child#}
  (toTextView tv) (toWidget w) (fromIntegral x) (fromIntegral y)

-- | Specify how to wrap text.
--
textViewSetWrapMode :: TextViewClass tv => tv -> WrapMode -> IO ()
textViewSetWrapMode tv wm = {#call text_view_set_wrap_mode#} (toTextView tv)
  ((fromIntegral.fromEnum) wm)

-- | Query how text is wrapped.
--
textViewGetWrapMode :: TextViewClass tv => tv -> IO WrapMode
textViewGetWrapMode tv = liftM (toEnum.fromIntegral) $
  {#call unsafe text_view_get_wrap_mode#} (toTextView tv)

-- | Toggle whether the text in the
-- 'TextView' is editable or not.
--
textViewSetEditable :: TextViewClass tv => tv -> Bool -> IO ()
textViewSetEditable tv editable =
  {#call text_view_set_editable#} (toTextView tv) (fromBool editable)

-- | Retrieve information whether a
-- 'TextView' is editable or not.
--
textViewGetEditable :: TextViewClass tv => tv -> IO Bool
textViewGetEditable tv = liftM toBool $
  {#call unsafe text_view_get_editable#} (toTextView tv)

-- | Toggle whether the cursor in the
-- 'TextView' is visible or not.
--
textViewSetCursorVisible :: TextViewClass tv => tv -> Bool -> IO ()
textViewSetCursorVisible tv editable =
  {#call text_view_set_cursor_visible#} (toTextView tv) (fromBool editable)

-- | Retrieve information whether the cursor
-- in a 'TextView' is visible or not.
--
textViewGetCursorVisible :: TextViewClass tv => tv -> IO Bool
textViewGetCursorVisible tv = liftM toBool $
  {#call unsafe text_view_get_cursor_visible#} (toTextView tv)

-- | Set the number of pixels above each
-- paragraph.
--
-- * Tags in the buffer may override this default.
--
textViewSetPixelsAboveLines :: TextViewClass tv => tv -> Int -> IO ()
textViewSetPixelsAboveLines tv p = {#call text_view_set_pixels_above_lines#}
  (toTextView tv) (fromIntegral p)

-- | Get the number of pixels above each
-- paragraph.
--
-- * Tags in the buffer may override this default.
--
textViewGetPixelsAboveLines :: TextViewClass tv => tv -> IO Int
textViewGetPixelsAboveLines tv = liftM (fromIntegral) $
  {#call unsafe text_view_get_pixels_above_lines#} (toTextView tv)

-- | Set the number of pixels below each
-- paragraph.
--
-- * Tags in the buffer may override this default.
--
textViewSetPixelsBelowLines :: TextViewClass tv => tv -> Int -> IO ()
textViewSetPixelsBelowLines tv p = {#call text_view_set_pixels_below_lines#}
  (toTextView tv) (fromIntegral p)

-- | Get the number of pixels below each
-- paragraph.
--
-- * Tags in the buffer may override this default.
--
textViewGetPixelsBelowLines :: TextViewClass tv => tv -> IO Int
textViewGetPixelsBelowLines tv = liftM (fromIntegral) $
  {#call unsafe text_view_get_pixels_below_lines#} (toTextView tv)

-- | Set the number of pixels between
-- lines inside a wraped paragraph.
--
-- * Tags in the buffer may override this default.
--
textViewSetPixelsInsideWrap :: TextViewClass tv => tv -> Int -> IO ()
textViewSetPixelsInsideWrap tv p = 
  {#call text_view_set_pixels_inside_wrap#} (toTextView tv) (fromIntegral p)

-- | Get the number of pixels between
-- lines inside a wraped paragraph.
--
-- * Tags in the buffer may override this default.
--
textViewGetPixelsInsideWrap :: TextViewClass tv => tv -> IO Int
textViewGetPixelsInsideWrap tv = liftM (fromIntegral) $
  {#call unsafe text_view_get_pixels_inside_wrap#} (toTextView tv)

-- | Specify how to wrap text.
--
textViewSetJustification :: TextViewClass tv => tv -> Justification -> IO ()
textViewSetJustification tv j = {#call text_view_set_justification#}
  (toTextView tv) ((fromIntegral.fromEnum) j)

-- | Query how text is wrapped.
--
textViewGetJustification :: TextViewClass tv => tv -> IO Justification
textViewGetJustification tv = liftM (toEnum.fromIntegral) $
  {#call unsafe text_view_get_justification#} (toTextView tv)

-- | Set the number of pixels in the margin.
--
-- * Tags in the buffer may override this default.
--
textViewSetLeftMargin :: TextViewClass tv => tv -> Int -> IO ()
textViewSetLeftMargin tv p = {#call text_view_set_left_margin#}
  (toTextView tv) (fromIntegral p)

-- | Get the number of pixels in the margin.
--
-- * Tags in the buffer may override this default.
--
textViewGetLeftMargin :: TextViewClass tv => tv -> IO Int
textViewGetLeftMargin tv = liftM (fromIntegral) $
  {#call unsafe text_view_get_left_margin#} (toTextView tv)

-- | Set the number of pixels in the margin.
--
-- * Tags in the buffer may override this default.
--
textViewSetRightMargin :: TextViewClass tv => tv -> Int -> IO ()
textViewSetRightMargin tv p = {#call text_view_set_right_margin#}
  (toTextView tv) (fromIntegral p)

-- | Get the number of pixels in the margin.
--
-- * Tags in the buffer may override this default.
--
textViewGetRightMargin :: TextViewClass tv => tv -> IO Int
textViewGetRightMargin tv = liftM (fromIntegral) $
  {#call unsafe text_view_get_right_margin#} (toTextView tv)

-- | Set the indentation in pixels for the first line
-- in a paragraph.
--
-- * Tags in the buffer may override this default.
--
-- * The indentation may be negative.
--
textViewSetIndent :: TextViewClass tv => tv -> Int -> IO ()
textViewSetIndent tv p = {#call text_view_set_indent#}
  (toTextView tv) (fromIntegral p)

-- | Get the indentation in pixels for the first line
-- in a paragraph.
--
-- * Tags in the buffer may override this default.
--
-- * The indentation may be negative.
--
textViewGetIndent :: TextViewClass tv => tv -> IO Int
textViewGetIndent tv = liftM (fromIntegral) $
  {#call unsafe text_view_get_indent#} (toTextView tv)

-- | Obtains a copy of the default text attributes. These are the attributes
-- used for text unless a tag overrides them.
--
textViewGetDefaultAttributes :: TextViewClass tv => tv -> IO TextAttributes
textViewGetDefaultAttributes tv =
  {#call gtk_text_view_get_default_attributes#} (toTextView tv)
   >>= makeNewTextAttributes


-- Signals

-- | Copying to the clipboard.
--
-- * This signal is emitted when a selection is copied to the clipboard. 
--
-- * The action itself happens when the 'TextView' processes this
--   signal.
--
onCopyClipboard, afterCopyClipboard :: TextViewClass tv => tv -> IO () ->
							   IO (ConnectId tv)
onCopyClipboard = connect_NONE__NONE "copy_clipboard" False
afterCopyClipboard = connect_NONE__NONE "copy_clipboard" True

-- | Cutting to the clipboard.
--
-- * This signal is emitted when a selection is cut out and copied to the
--   clipboard. The action itself happens when the textview processed this
--   request.
--
onCutClipboard, afterCutClipboard :: TextViewClass tv => tv -> IO () ->
							 IO (ConnectId tv)
onCutClipboard = connect_NONE__NONE "cut_clipboard" False
afterCutClipboard = connect_NONE__NONE "cut_clipboard" True

-- | Deleting text.
--
-- * The widget will remove the specified number of units in the text where
--   the meaning of units depends on the kind of deletion.
--
-- * The action itself happens when the 'TextView' processes this
--   signal.
--
onDeleteFromCursor, afterDeleteFromCursor :: TextViewClass tv => tv ->
					     (DeleteType -> Int -> IO ()) ->
					     IO (ConnectId tv)
onDeleteFromCursor = connect_ENUM_INT__NONE "delete_from_cursor" False
afterDeleteFromCursor = connect_ENUM_INT__NONE "delete_from_cursor" True

-- | Inserting text.
--
-- * The widget will insert the string into the text where the meaning
--   of units depends on the kind of deletion.
--
-- * The action itself happens when the 'TextView' processes this
--   signal.
--
onInsertAtCursor, afterInsertAtCursor :: TextViewClass tv => tv ->
							(String -> IO ()) ->
							IO (ConnectId tv)
onInsertAtCursor = connect_STRING__NONE "insert_at_cursor" False
afterInsertAtCursor = connect_STRING__NONE "insert_at_cursor" True

-- | Moving the cursor.
--
-- * The signal specifies what kind and how many steps the cursor will do.
--   The flag is set to @True@ if this movement extends a selection.
--
-- * The action itself happens when the 'TextView' processes this
--   signal.
--
onMoveCursor, afterMoveCursor :: TextViewClass tv => tv ->
				 (MovementStep -> Int -> Bool -> IO ()) ->
				 IO (ConnectId tv)
onMoveCursor = connect_ENUM_INT_BOOL__NONE "move_cursor" False
afterMoveCursor = connect_ENUM_INT_BOOL__NONE "move_cursor" True

-- | Moving the focus.
--
-- * The action itself happens when the 'TextView' processes this
--   signal.
--
onMoveFocus, afterMoveFocus :: TextViewClass tv => tv ->
			       (DirectionType -> IO ()) ->
			       IO (ConnectId tv)
onMoveFocus = connect_ENUM__NONE "move_focus" False
afterMoveFocus = connect_ENUM__NONE "move_focus" True

-- | Page change signals.
--
-- * The signal specifies how many pages the view should move up or down.
--   The flag is set to @True@ if this movement extends a selection.
--
-- * The action itself happens when the 'TextView' processes this
--   signal.
--
-- * Figure out why this signal is called horizontally, not vertically.
--
onPageHorizontally, afterPageHorizontally :: TextViewClass tv => tv ->
					     (Int -> Bool -> IO ()) ->
					     IO (ConnectId tv)
onPageHorizontally = connect_INT_BOOL__NONE "page_horizontally" False
afterPageHorizontally = connect_INT_BOOL__NONE "page_horizontally" True


-- | Pasting from the clipboard.
--
-- * This signal is emitted when something is pasted from the clipboard. 
--
-- * The action itself happens when the 'TextView' processes this
--   signal.
--
onPasteClipboard, afterPasteClipboard :: TextViewClass tv => tv -> IO () ->
							IO (ConnectId tv)
onPasteClipboard = connect_NONE__NONE "paste_clipboard" False
afterPasteClipboard = connect_NONE__NONE "paste_clipboard" True

-- | Add menu entries to context menus.
--
-- * This signal is emitted if a context menu within the 'TextView'
--   is opened. This signal can be used to add application specific menu
--   items to this popup.
--
onPopulatePopup, afterPopulatePopup :: TextViewClass tv => tv ->
						      (Menu -> IO ()) ->
						      IO (ConnectId tv)
onPopulatePopup = connect_OBJECT__NONE "populate_popup" False
afterPopulatePopup = connect_OBJECT__NONE "populate_popup" True

-- | Inserting an anchor.
--
-- * This signal is emitted when anchor is inserted into the text. 
--
-- * The action itself happens when the 'TextView' processes this
--   signal.
--
onSetAnchor, afterSetAnchor :: TextViewClass tv => tv -> IO () ->
					      IO (ConnectId tv)
onSetAnchor = connect_NONE__NONE "set_anchor" False
afterSetAnchor = connect_NONE__NONE "set_anchor" True

-- | The scroll-bars changed.
--
--
onSetScrollAdjustments, afterSetScrollAdjustments ::
  TextViewClass tv => tv -> (Adjustment -> Adjustment -> IO ()) -> 
  IO (ConnectId tv)
onSetScrollAdjustments = 
  connect_OBJECT_OBJECT__NONE "set_scroll_adjustments" False
afterSetScrollAdjustments = 
  connect_OBJECT_OBJECT__NONE "set_scroll_adjustments" True

-- | Insert\/Overwrite mode has changed.
--
-- * This signal is emitted when the 'TextView' changes from
--   inserting mode to overwriting mode and vice versa. 
--
-- * The action itself happens when the 'TextView' processes this
--   signal.
--
onToggleOverwrite, afterToggleOverwrite :: TextViewClass tv => tv -> IO () ->
							  IO (ConnectId tv)
onToggleOverwrite = connect_NONE__NONE "toggle_overwrite" False
afterToggleOverwrite = connect_NONE__NONE "toggle_overwrite" True

