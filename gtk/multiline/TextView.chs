-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget TextView@
--
--  Author : Axel Simon
--          
--  Created: 23 February 2002
--
--  Version $Revision: 1.5 $ from $Date: 2002/08/12 10:43:56 $
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
--
-- @documentation@ ------------------------------------------------------------
--
-- * Through out we distinguish between buffer cooridinates which are pixels
--   with the origin at the upper left corner of the first character on the
--   first line. Window coordinates are relative to the top left pixel which
--   is visible in the current @ref data TextView@. Coordinates from Events 
--   are in the latter relation. The conversion can be done with 
--   @ref method textViewWindowToBufferCoords@.
--
-- @todo@ ---------------------------------------------------------------------
--
-- * Do GdkRectangle and then the following:
--     gtk_text_view_get_visible_rect
--     gtk_text_view_get_iter_location
--
-- * Everyting after textChildAnchorGetDeleted, except SetEditable, GetEditable,
--   SetCursorVisible, GetCursorVisible
--
module TextView(
  TextView,
  TextViewClass,
  castToTextView,
  textViewNew,
  textViewNewWithBuffer,
  textViewSetBuffer,
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
  textViewMoveVisually,
  textViewAddChildAtAnchor,
  textViewSetEditable,
  textViewGetEditable,
  textViewSetCursorVisible,
  textViewGetCursorVisible,
  textChildAnchorNew,
  textChildAnchorGetWidgets,
  textChildAnchorGetDeleted
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
import GObject	(makeNewGObject)
{#import Hierarchy#}
{#import Signal#}
{#import TextIter#}
import Enums	(TextWindowType)
import GList	(fromGList)

{# context lib="gtk" prefix="gtk" #}


-- methods

-- @constructor textViewNew@ Create a new @ref type TextView@ widget with a
-- default @ref type TextBuffer@.
--
textViewNew :: IO TextView
textViewNew  = makeNewGObject mkTextView $ liftM castPtr 
  {#call unsafe text_view_new#}

-- @method textViewNewWithBuffer@ Create a new @ref type TextView@ widget with
-- the given @ref type TextBuffer@.
--
textViewNewWithBuffer :: TextBuffer -> IO TextView
textViewNewWithBuffer tb = makeNewGObject mkTextView $ liftM castPtr $
  {#call unsafe text_view_new_with_buffer#} tb

-- @method textViewSetBuffer@ Set the @ref type TextBuffer@ for a given
-- @ref type TextView@ widget.
--
textViewSetBuffer :: TextViewClass tv => tv -> TextBuffer -> IO ()
textViewSetBuffer tv tb = 
  {#call unsafe text_view_set_buffer#} (toTextView tv) tb

-- @method textViewScrollToMark@ Scroll to the position of the supplied
-- @ref data TextMark@.
--
-- * Supplying @ref arg xalign@, @ref arg yalign@ gives a goal position of
--   the @ref data TextMark@ within screen bounds. 0,0 means left, top and
--   1.0,1.0 means right, bottom.
--
-- * Supply @ref arg Nothing@ if the goal is to bring the position into 
--   view with the minimum of scrolling.
--
-- * @ref arg withinMargin@ is within [0.0 .. 0.5) and imposes an extra margin
--   at all four sides of the window within which @ref arg xalign@ and
--   @ref arg yalign@ are evaluated.
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

-- @method textViewScrollToIter@ Scroll to the position of the supplied
-- @ref type TextIter@.
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

-- @method textViewScrollMarkOnscreen@ Scroll the visible area of the widget
-- so the @ref type TextMark@ becomes visible.
--
-- * This call is equivalent to @ref method textViewScrollToMark@ tm 0.0
--   Nothing tv.
--
textViewScrollMarkOnscreen :: TextViewClass tv => tv -> TextMark -> IO ()
textViewScrollMarkOnscreen tv tm = 
  {#call unsafe text_view_scroll_mark_onscreen#} (toTextView tv) tm

-- @method textViewMoveMarkOnscreen@ Move a @ref type TextMark@ within the
-- buffer until it is in the currently visible area of the widget.
--
-- * Returns True if the Mark was moved.
--
textViewMoveMarkOnscreen :: TextViewClass tv => tv -> TextMark -> IO Bool
textViewMoveMarkOnscreen tv tm = liftM toBool $ 
  {#call unsafe text_view_move_mark_onscreen#} (toTextView tv) tm

-- @method textViewPlaceCursorOnscreen@ Move the cursor within the buffer
-- until it is in the currently visible area of the widget.
--
-- * Returns True if the Mark was moved.
--
textViewPlaceCursorOnscreen :: TextViewClass tv => tv -> IO Bool
textViewPlaceCursorOnscreen tv = liftM toBool $ 
  {#call unsafe text_view_place_cursor_onscreen#} (toTextView tv)

-- @method textViewGetVisible@ Get the currently visible rectangle.
--
-- * Use @ref method textViewBufferToWindowCoords@ to convert into window 
-- cooridnates.
--
--textViewGetVisibleRect :: TextViewClass tv => tv -> IO GdkRectangle
--textViewGetVisibleRect tv = alloca $ \rectPtr -> do
--  {#call unsafe text_view_visible_rect#} (toTextView tv) rect
--  peek rectPtr


-- @method textViewGetIterLocation@ Get a rectangle that roughly contains the 
-- character at @ref data TextIter@.
--
-- * Use @ref method textViewBufferToWindowCoords@ to convert into window 
--   cooridnates.
--
--textViewGetIterLocation :: TextViewClass tv => TextMark -> tv -> IO GdkRectangle
--textViewGetIterLocation tm tv = alloca $ \rectPtr -> do
--  {#call unsafe text_view_iter_location#} (toTextView tv) tm rect
--  peek rectPtr



-- @method textViewGetLineAtY@ Get the @ref type TextIter@ at the start of the
-- line containing the coordinate @ref arg y@.
--
-- * @ref arg y@ is in buffer coordinates.
--
-- * Returns the @ref type TextIter@ and the top of the line.
--
textViewGetLineAtY :: TextViewClass tv => tv -> Int -> IO (TextIter,Int)
textViewGetLineAtY tv y = do
  iter <- makeEmptyTextIter
  lineTop <- liftM fromIntegral $ alloca $ \ltPtr -> do
    {#call unsafe text_view_get_line_at_y#} (toTextView tv) iter 
      (fromIntegral y) ltPtr
    peek ltPtr
  return (iter, lineTop)

-- @method textViewGetLineYrange@ Get the y coordinate of the top and the
-- height of the line @ref type TextIter@ is on.
--
textViewGetLineYrange :: TextViewClass tv => tv -> TextIter -> IO (Int,Int)
textViewGetLineYrange tv ti = alloca $ \yPtr -> alloca $ \heightPtr -> do
  {#call unsafe text_view_get_line_yrange#} (toTextView tv) ti yPtr heightPtr
  y <- peek yPtr
  height <- peek heightPtr
  return (fromIntegral y, fromIntegral height)

-- @method textViewGetIterAtLocation@ Retrieves the @ref type TextIter@ at
-- buffer coordinates @ref arg x@ and @ref arg y@.
--
textViewGetIterAtLocation :: TextViewClass tv => tv -> Int -> Int ->
                             IO TextIter
textViewGetIterAtLocation tv x y = do
  iter <- makeEmptyTextIter
  {#call unsafe text_view_get_iter_at_location#} (toTextView tv) iter 
    (fromIntegral x) (fromIntegral y)
  return iter

-- @method textViewBufferToWindowCoords@ Convert buffer cooridnates into
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

-- @method textViewWindowToBufferCoords@ Convert window cooridnates into
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


-- @method textViewGetWindow@ Get the underlying @ref arg GdkWindow@.
--
-- * The @ref type TextWindowType@ determines which window of the
--   @ref type TextWidget@ we would like to receive.
--
-- * Returns Nothing if there is no @ref arg GdkWindow@ of the specified type.
--
textViewGetWindow :: TextViewClass tv => tv -> TextWindowType ->
                     IO (Maybe GdkWindow)
textViewGetWindow tv wt = do
  winPtr <- {#call unsafe text_view_get_window#} (toTextView tv) 
    ((fromIntegral.fromEnum) wt)
  if winPtr==nullPtr then return Nothing else liftM Just $
    makeNewGObject mkGdkWindow (return winPtr)

-- @method textViewGetWindowType@ Retrieve the type of window the
-- @ref type TextView@ widget contains.
--
-- * Usually used to find out which window an event corresponds to. An
--   emission of an event signal of @ref type TextView@ yields a
--   @ref arg GdkWindow@. This function can be used to see if the event
--   actually belongs to the main text window.
--
textViewGetWindowType :: TextViewClass tv => tv -> GdkWindow ->
                         IO TextWindowType
textViewGetWindowType tv win = liftM (toEnum.fromIntegral) $
  {#call unsafe text_view_get_window_type#} (toTextView tv) win


-- @method textViewSetBorderWindowSize@ Set the border width of the
-- @ref type TextView@ widget.
--
-- * Sets the width of @ref type TextWindowLeft@ or
--   @ref type TextWindowRight@, or the height of @ref type TextWindowTop@ or
--   @ref type TextWindowBottom@. Automatically destroys the corresponding
--   window if the size is set to 0 and creates the window if the size is set
--   to non-zero. This function can only used with the four window types
--   mentioned.
--
textViewSetBorderWindowSize :: TextViewClass tv => tv -> TextWindowType ->
                               Int -> IO ()
textViewSetBorderWindowSize tv wt size = 
  {#call unsafe text_view_set_border_window_size#} (toTextView tv) 
  ((fromIntegral.fromEnum) wt) (fromIntegral size)

-- @method textViewGetBorderWindowSize@ Retrieve the border width of the
-- specified window.
--
-- * See @ref method textViewSetBorderWindowSize@.
--
textViewGetBorderWindowSize :: TextViewClass tv => tv -> TextWindowType ->
                               IO Int
textViewGetBorderWindowSize tv wt = liftM fromIntegral $
  {#call unsafe text_view_get_border_window_size#} (toTextView tv) 
  ((fromIntegral.fromEnum) wt)

-- @method textViewForwardDisplayLine@ Move the iterator forwards by one
-- display line.
--
-- * Moves the given @ref type TextIter@ forward by one display (wrapped)
--   line. A display line is different from a paragraph. Paragraphs are
--   separated by newlines or other paragraph separator characters. Display
--   lines are created by line-wrapping a paragraph. If wrapping is turned
--   off, display lines and paragraphs will be the same. Display lines are
--   divided differently for each view, since they depend on the view's width;
--   paragraphs are the same in all views, since they depend on the contents
--   of the @ref type TextBuffer@.
--
textViewForwardDisplayLine :: TextViewClass tv => tv -> TextIter -> IO Bool
textViewForwardDisplayLine tv ti = liftM toBool $
  {#call unsafe text_view_forward_display_line#} (toTextView tv) ti

-- @method textViewBackwardDisplayLine@ Move the iterator backwards by one
-- display line.
--
-- * See @ref method textViewForwardDisplayLine@.
--
textViewBackwardDisplayLine :: TextViewClass tv => tv -> TextIter -> IO Bool
textViewBackwardDisplayLine tv ti = liftM toBool $
  {#call unsafe text_view_backward_display_line#} (toTextView tv) ti
 
-- @method textViewForwardDisplayLineEnd@ Move the iterator forwards and to
-- the end.
--
-- * Like @ref method textViewForwardDisplayLine@ but moves to the end of 
--   the line as well.
--
textViewForwardDisplayLineEnd :: TextViewClass tv => TextIter -> tv -> IO Bool
textViewForwardDisplayLineEnd ti tv = liftM toBool $
  {#call unsafe text_view_forward_display_line_end#} (toTextView tv) ti

-- @method textViewBackwardDisplayLineEnd@ Move the iterator backwards and to
-- the end.
--
-- * See @ref method textViewForwardDisplayLineEnd@.
--
textViewBackwardDisplayLineEnd :: TextViewClass tv => tv -> TextIter -> IO Bool
textViewBackwardDisplayLineEnd tv ti = liftM toBool $
  {#call unsafe text_view_backward_display_line_start#} (toTextView tv) ti

-- @method textViewForwardDisplayLineStart@ Move the iterator forwards and to
-- the start.
--
-- * Like @ref method textViewForwardDisplayLine@ but moves to the start of
--   the line as well.
--
textViewForwardDisplayLineStart :: TextViewClass tv => tv -> TextIter ->
                                   IO Bool
textViewForwardDisplayLineStart tv ti = liftM toBool $
  {#call unsafe text_view_forward_display_line_end#} (toTextView tv) ti

-- @method textViewBackwardDisplayLineStart@ Move the iterator backwards and
-- to the start.
--
-- * See @ref method textViewForwardDisplayLineStart@.
--
textViewBackwardDisplayLineStart :: TextViewClass tv => tv -> TextIter ->
                                    IO Bool
textViewBackwardDisplayLineStart tv ti = liftM toBool $
  {#call unsafe text_view_backward_display_line_start#} (toTextView tv) ti


-- @method textViewMoveVisually@ Move the iterator a number of lines.
--
-- * @ref arg count@ is in display lines. See
--   @ref method textViewForwardDisplayLine@.
--
textViewMoveVisually :: TextViewClass tv => tv -> TextIter -> Int -> IO Bool
textViewMoveVisually tv ti count = liftM toBool $
  {#call unsafe text_view_move_visually#} (toTextView tv) ti 
    (fromIntegral count)


-- @method textViewAddChildAtAnchor@ Add a child widget in the
-- @ref type TextBuffer@ at a given @ref type TextChildAnchor@.
--
textViewAddChildAtAnchor :: (TextViewClass tv , WidgetClass w) => tv -> w ->
                            TextChildAnchor -> IO ()
textViewAddChildAtAnchor tv w anchor = 
  {#call unsafe text_view_add_child_at_anchor#} (toTextView tv) (toWidget w) 
    anchor

-- @method textViewSetEditable@ Toggle whether the text in the
-- @ref type TextView@ is editable or not.
--
textViewSetEditable :: TextViewClass tv => tv -> Bool -> IO ()
textViewSetEditable tv editable =
  {#call text_view_set_editable#} (toTextView tv) (fromBool editable)

-- @method textViewGetEditable@ Retrieve information whether a
-- @ref type TextView@ is editable or not.
--
textViewGetEditable :: TextViewClass tv => tv -> IO Bool
textViewGetEditable tv = liftM toBool $
  {#call unsafe text_view_get_editable#} (toTextView tv)

-- @method textViewSetCursorVisible@ Toggle whether the cursor in the
-- @ref type TextView@ is visible or not.
--
textViewSetCursorVisible :: TextViewClass tv => tv -> Bool -> IO ()
textViewSetCursorVisible tv editable =
  {#call text_view_set_cursor_visible#} (toTextView tv) (fromBool editable)

-- @method textViewGetCursorVisible@ Retrieve information whether the cursor
-- in a @ref type TextView@ is visible or not.
--
textViewGetCursorVisible :: TextViewClass tv => tv -> IO Bool
textViewGetCursorVisible tv = liftM toBool $
  {#call unsafe text_view_get_cursor_visible#} (toTextView tv)

-- @constructor textChildAnchorNew@ Create a new @ref type TextChildAnchor@.
--
-- * Using @ref method textBufferCreateChildAnchor@ is usually simpler then
--   executing this function and @ref method textBufferInsertChildAnchor@.
--
textChildAnchorNew :: IO TextChildAnchor
textChildAnchorNew  = makeNewGObject mkTextChildAnchor 
  {#call unsafe text_child_anchor_new#}


-- @method textChildAnchorGetWidgets@ Retrieve all @ref arg Widget@s at this
-- @ref type TextChildAnchor@.
--
-- * The widgets in the returned list need to be upcasted to what they were.
--
textChildAnchorGetWidgets :: TextChildAnchor -> IO [Widget]
textChildAnchorGetWidgets tca = do
  gList <- {#call text_child_anchor_get_widgets#} tca
  wList <- fromGList gList
  mapM (makeNewObject mkWidget) (map return wList)

-- @method textChildAnchorGetDeleted@ Query if an anchor was deleted.
--
textChildAnchorGetDeleted :: TextChildAnchor -> IO Bool
textChildAnchorGetDeleted tca = liftM toBool $
  {#call unsafe text_child_anchor_get_deleted#} tca

-- 
