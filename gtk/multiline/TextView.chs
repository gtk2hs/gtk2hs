-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: Widget TextView
--
--  Author : Axel Simon
--          
--  Created: 23 February 2002
--
--  Version $Revision: 1.1.1.1 $ from $Date: 2002/03/24 21:56:20 $
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
--- DESCRIPTION ---------------------------------------------------------------
--
--
--- DOCU ----------------------------------------------------------------------
--
-- * Through out we distinguish between buffer cooridinates which are pixels
--   with the origin at the upper left corner of the first character on the
--   first line. Window coordinates are relative to the top left pixel which
--   is visible in the current @TextView. Coordinates from Events are in the
--   latter relation. The conversion can be done with 
--   textViewWindowToBufferCoords.
--
--- TODO ----------------------------------------------------------------------
--
-- * Do GdkRectangle and then the following:
--     gtk_text_view_get_visible_rect
--     gtk_text_view_get_iter_location
--
-- * Everyting after @textChildAnchorGetDeleted
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
  textChildAnchorNew,
  textChildAnchorGetWidgets,
  textChildAnchorGetDeleted
  ) where

import Monad	(liftM)
import Foreign
import CForeign
import Object	(makeNewObject)
import GObject	(makeNewGObject)
{#import Hierarchy#}
{#import Signal#}
{#import TextIter#}
import Enums	(TextWindowType)
import GList	(fromGList)

{# context lib="gtk" prefix="gtk" #}


-- methods

-- Create a new @TextView widget with a default @TextBuffer. (EXPORTED)
--
textViewNew :: IO TextView
textViewNew = makeNewGObject mkTextView $ liftM castPtr 
  {#call unsafe text_view_new#}

-- Create a new @TextView widget with the given @TextBuffer. (EXPORTED)
--
textViewNewWithBuffer :: TextBuffer -> IO TextView
textViewNewWithBuffer tb = makeNewGObject mkTextView $ liftM castPtr $
  {#call unsafe text_view_new_with_buffer#} tb

-- Set the @TextBuffer for a given @TextView widget. (EXPORTED)
--
textViewSetBuffer :: TextViewClass tv => TextBuffer -> tv -> IO ()
textViewSetBuffer tb tv = 
  {#call unsafe text_view_set_buffer#} (toTextView tv) tb

-- Scroll to the position of the supplied @TextMark. (EXPORTED)
--
-- * Just (@xalign, @yalign) gives a goal position of the @TextMark within
--   screen bounds. 0,0 means left, top and 1.0,1.0 means right, bottom.
--
-- * Supply Nothing if the goal is to bring the position into view with
--   the minimum of scrolling.
--
-- * @withinMargin is within [0.0 .. 0.5) and imposes an extra margin at
--   all four sides of the window within which @xalign and @yalign are
--   evaluated.
--
-- * The line distances are calculated in an idle handler. Calling this
--   function ensures that the line heights are indeed evaluated before
--   the scroll is carried out.
--
textViewScrollToMark :: TextViewClass tv => TextMark -> Double -> 
			Maybe (Double, Double) -> tv -> IO ()
textViewScrollToMark tm withinMargin (Just (xalign, yalign)) tv = 
  {#call unsafe text_view_scroll_to_mark#} (toTextView tv) tm 
  (realToFrac withinMargin) 1 (realToFrac xalign) (realToFrac yalign)
textViewScrollToMark tm withinMargin Nothing tv = 
  {#call unsafe text_view_scroll_to_mark#} (toTextView tv) tm 
  (realToFrac withinMargin) 0 (0.0) (0.0)

-- Scroll to the position of the supplied @TextIter. (EXPORTED)
--
-- * The position might not be correct due to the delayed calculation of
--   the line heights.
--
-- * Returns True if the function actually scrolled.
--
textViewScrollToIter tm withinMargin (Just (xalign, yalign)) tv = 
  liftM toBool $ {#call unsafe text_view_scroll_to_iter#} (toTextView tv) 
    tm (realToFrac withinMargin) 1 (realToFrac xalign) 
    (realToFrac yalign)
textViewScrollToIter tm withinMargin Nothing tv = liftM toBool $
  {#call unsafe text_view_scroll_to_iter#} (toTextView tv) tm 
    (realToFrac withinMargin) 0 (0.0) (0.0)

-- Scroll the visible area of the widget so the @TextMark becomes visible.
-- (EXPORTED)
--
-- * This call is equivalent to @textViewScrollToMark tm 0.0 Nothing tv.
--
textViewScrollMarkOnscreen :: TextViewClass tv => TextMark -> tv -> IO ()
textViewScrollMarkOnscreen tm tv = 
  {#call unsafe text_view_scroll_mark_onscreen#} (toTextView tv) tm

-- Move a @TextMark within the buffer until it is in the currently visible
-- area of the widget. (EXPORTED)
--
-- * Returns True if the Mark was moved.
--
textViewMoveMarkOnscreen :: TextViewClass tv => TextMark -> tv -> IO Bool
textViewMoveMarkOnscreen tm tv = liftM toBool $ 
  {#call unsafe text_view_move_mark_onscreen#} (toTextView tv) tm

-- Move the cursor within the buffer until it is in the currently visible
-- area of the widget. (EXPORTED)
--
-- * Returns True if the Mark was moved.
--
textViewPlaceCursorOnscreen :: TextViewClass tv => tv -> IO Bool
textViewPlaceCursorOnscreen tv = liftM toBool $ 
  {#call unsafe text_view_place_cursor_onscreen#} (toTextView tv)

-- Get the currently visible rectangle. (EXPORTED)
--
-- * Use @textViewBufferToWindowCoords to convert into window cooridnates.
--
--textViewGetVisibleRect :: TextViewClass tv => tv -> IO GdkRectangle
--textViewGetVisibleRect tv = alloca $ \rectPtr -> do
--  {#call unsafe text_view_visible_rect#} (toTextView tv) rect
--  peek rectPtr

-- Get a rectangle that roughly contains the character at @TextIter. (EXPORTED)
--
-- * Use @textViewBufferToWindowCoords to convert into window cooridnates.
--
--textViewGetIterLocation :: TextViewClass tv => TextMark -> tv -> IO GdkRectangle
--textViewGetIterLocation tm tv = alloca $ \rectPtr -> do
--  {#call unsafe text_view_iter_location#} (toTextView tv) tm rect
--  peek rectPtr


-- Get the @TextIter at the start of the line containing the coordinate @y.
-- (EXPORTED)
--
-- * @y is in buffer coordinates.
--
-- * Returns the @TextIter and the top of the line.
--
textViewGetLineAtY :: TextViewClass tv => Int -> tv -> IO (TextIter,Int)
textViewGetLineAtY y tv = do
  iter <- makeEmptyTextIter
  lineTop <- liftM fromIntegral $ alloca $ \ltPtr -> do
    {#call unsafe text_view_get_line_at_y#} (toTextView tv) iter 
      (fromIntegral y) ltPtr
    peek ltPtr
  return (iter, lineTop)

-- Get the y coordinate of the top and the height of the line @TextIter is on.
-- (EXPORTED)
--
textViewGetLineYrange :: TextViewClass tv => TextIter -> tv -> IO (Int,Int)
textViewGetLineYrange ti tv = alloca $ \yPtr -> alloca $ \heightPtr -> do
  {#call unsafe text_view_get_line_yrange#} (toTextView tv) ti yPtr heightPtr
  y <- peek yPtr
  height <- peek heightPtr
  return (fromIntegral y, fromIntegral height)

-- Retrieves the @TextIter at buffer coordinates @x and @y. (EXPORTED)
--
textViewGetIterAtLocation :: TextViewClass tv => Int -> Int -> tv -> 
			     IO TextIter
textViewGetIterAtLocation x y tv = do
  iter <- makeEmptyTextIter
  {#call unsafe text_view_get_iter_at_location#} (toTextView tv) iter 
    (fromIntegral x) (fromIntegral y)
  return iter

-- Convert buffer cooridnates into window coordinates. (EXPORTED)
--
textViewBufferToWindowCoords :: TextViewClass tv => TextWindowType -> 
				(Int,Int) -> tv -> IO (Int,Int)
textViewBufferToWindowCoords wt (x,y) tv = 
  alloca $ \xPtr -> alloca $ \yPtr -> do
    {#call unsafe text_view_buffer_to_window_coords#} (toTextView tv) 
      ((fromIntegral.fromEnum) wt) (fromIntegral x) (fromIntegral y) 
      xPtr yPtr
    x' <- peek xPtr
    y' <- peek yPtr
    return (fromIntegral x', fromIntegral y')

-- Convert window cooridnates into buffer coordinates. (EXPORTED)
--
textViewWindowToBufferCoords :: TextViewClass tv => TextWindowType -> 
				(Int,Int) -> tv -> IO (Int,Int)
textViewWindowToBufferCoords wt (x,y) tv = 
  alloca $ \xPtr -> alloca $ \yPtr -> do
    {#call unsafe text_view_window_to_buffer_coords#} (toTextView tv) 
      ((fromIntegral.fromEnum) wt) (fromIntegral x) (fromIntegral y) xPtr yPtr
    x' <- peek xPtr
    y' <- peek yPtr
    return (fromIntegral x', fromIntegral y')


-- Get the underlying @GdkWindow. (EXPORTED)
--
-- * The @TextWindowType determines which window of the @TextWidget we would
--   like to receive.
--
-- * Returns Nothing if there is no @GdkWindow of the specified type.
--
textViewGetWindow :: TextViewClass tv => TextWindowType -> tv -> 
		     IO (Maybe GdkWindow)
textViewGetWindow wt tv = do
  winPtr <- {#call unsafe text_view_get_window#} (toTextView tv) 
    ((fromIntegral.fromEnum) wt)
  if winPtr==nullPtr then return Nothing else liftM Just $
    makeNewGObject mkGdkWindow (return winPtr)

-- Retrieve the type of window the @TextView widget contains. (EXPORTED)
--
-- * Usually used to find out which window an event corresponds to. An emission
--   of an event signal of @TextView yields a @GdkWindow. This function can
--   be used to see if the event actually belongs to the main text window.
--
textViewGetWindowType :: TextViewClass tv => GdkWindow -> tv -> 
			 IO TextWindowType
textViewGetWindowType win tv = liftM (toEnum.fromIntegral) $
  {#call unsafe text_view_get_window_type#} (toTextView tv) win


-- Set the border width of the @TextView widget. (EXPORTED)
--
-- * Sets the width of @TextWindowLeft or @TextWindowRight, or the height of
--   @TextWindowTop or @TextWindowBottom. Automatically destroys the 
--   corresponding window if the size is set to 0 and creates the window if
--   the size is set to non-zero. This function can only used with the four
--   window types mentioned.
--
textViewSetBorderWindowSize :: TextViewClass tv => TextWindowType -> Int -> 
			       tv -> IO ()
textViewSetBorderWindowSize wt size tv = 
  {#call unsafe text_view_set_border_window_size#} (toTextView tv) 
  ((fromIntegral.fromEnum) wt) (fromIntegral size)

-- Retrieve the border width of the specified window. (EXPORTED)
--
-- * See @textViewSetBorderWindowSize.
--
textViewGetBorderWindowSize :: TextViewClass tv => TextWindowType -> tv -> 
			       IO Int
textViewGetBorderWindowSize wt tv = liftM fromIntegral $
  {#call unsafe text_view_get_border_window_size#} (toTextView tv) 
  ((fromIntegral.fromEnum) wt)

-- Move the iterator forwards by one display line. (EXPORTED)
--
-- * Moves the given @TextIter forward by one display (wrapped) line.  A
--   display line is different from a paragraph. Paragraphs are separated by 
--   newlines or other paragraph separator characters. Display lines are 
--   created by line-wrapping a paragraph.  If wrapping is turned off, 
--   display lines and paragraphs will be the same. Display lines are 
--   divided differently for each view, since they depend on the view's width; 
--   paragraphs are the same in all views, since they depend on the contents 
--   of the @TextBuffer.
--
textViewForwardDisplayLine :: TextViewClass tv => TextIter -> tv -> IO Bool
textViewForwardDisplayLine ti tv = liftM toBool $
  {#call unsafe text_view_forward_display_line#} (toTextView tv) ti

-- Move the iterator backwards by one display line. (EXPORTED)
--
-- * See @textViewForwardDisplayLine.
--
textViewBackwardDisplayLine :: TextViewClass tv => TextIter -> tv -> IO Bool
textViewBackwardDisplayLine ti tv = liftM toBool $
  {#call unsafe text_view_backward_display_line#} (toTextView tv) ti
 
-- Move the iterator forwards and to the end. (EXPORTED)
--
-- * Like @textViewForwardDisplayLine but moves to the end of the line as well.
--
textViewForwardDisplayLineEnd :: TextViewClass tv => TextIter -> tv -> IO Bool
textViewForwardDisplayLineEnd ti tv = liftM toBool $
  {#call unsafe text_view_forward_display_line_end#} (toTextView tv) ti

-- Move the iterator backwards and to the end. (EXPORTED)
--
-- * See @textViewForwardDisplayLineEnd.
--
textViewBackwardDisplayLineEnd :: TextViewClass tv => TextIter -> tv -> IO Bool
textViewBackwardDisplayLineEnd ti tv = liftM toBool $
  {#call unsafe text_view_backward_display_line_start#} (toTextView tv) ti

-- Move the iterator forwards and to the start. (EXPORTED)
--
-- * Like @textViewForwardDisplayLine but moves to the start of the line as 
--   well.
--
textViewForwardDisplayLineStart :: TextViewClass tv => TextIter -> tv -> 
				   IO Bool
textViewForwardDisplayLineStart ti tv = liftM toBool $
  {#call unsafe text_view_forward_display_line_end#} (toTextView tv) ti

-- Move the iterator backwards and to the start. (EXPORTED)
--
-- * See @textViewForwardDisplayLineStart.
--
textViewBackwardDisplayLineStart :: TextViewClass tv => TextIter -> tv -> 
				    IO Bool
textViewBackwardDisplayLineStart ti tv = liftM toBool $
  {#call unsafe text_view_backward_display_line_start#} (toTextView tv) ti


-- Move the iterator a number of lines. (EXPORTED)
--
-- * @count is in display lines. See @textViewForwardDisplayLine.
--
textViewMoveVisually :: TextViewClass tv => TextIter -> Int -> tv -> IO Bool
textViewMoveVisually ti count tv = liftM toBool $
  {#call unsafe text_view_move_visually#} (toTextView tv) ti 
    (fromIntegral count)


-- Add a child widget in the @TextBuffer at a given @TextChildAnchor. 
-- (EXPORTED)
--
textViewAddChildAtAnchor :: (TextViewClass tv , WidgetClass w) => w -> 
			    TextChildAnchor -> tv -> IO ()
textViewAddChildAtAnchor w anchor tv = 
  {#call unsafe text_view_add_child_at_anchor#} (toTextView tv) (toWidget w) 
    anchor


-- Create a new @TextChildAnchor. (EXPORTED)
--
-- * Using @textBufferCreateChildAnchor is usually simpler then executing
--   this function and @textBufferInsertChildAnchor.
--
textChildAnchorNew :: IO TextChildAnchor
textChildAnchorNew = makeNewGObject mkTextChildAnchor 
  {#call unsafe text_child_anchor_new#}


-- Retrieve all @Widget@s at this @TextChildAnchor. (EXPORTED)
--
-- * The widgets in the returned list need to be upcasted to what
--   they were.
--
textChildAnchorGetWidgets :: TextChildAnchor -> IO [Widget]
textChildAnchorGetWidgets tca = do
  gList <- {#call text_child_anchor_get_widgets#} tca
  wList <- fromGList gList
  mapM (makeNewObject mkWidget) (map return wList)

-- Query if an anchor was deleted. (EXPORTED)
--
textChildAnchorGetDeleted :: TextChildAnchor -> IO Bool
textChildAnchorGetDeleted tca = liftM toBool $
  {#call unsafe text_child_anchor_get_deleted#} tca

-- 