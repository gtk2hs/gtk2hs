-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Frame
--
--  Author : Axel Simon
--
--  Created: 15 May 2001
--
--  Version $Revision: 1.5 $ from $Date: 2005/03/24 15:21:09 $
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
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- A bin with a decorative frame and optional label
--
module Graphics.UI.Gtk.Ornaments.Frame (
-- * Detail
-- 
-- | The frame widget is a Bin that surrounds its child with a decorative
-- frame and an optional label. If present, the label is drawn in a gap in the
-- top side of the frame. The position of the label can be controlled with
-- 'frameSetLabelAlign'.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Bin'
-- |                           +----Frame
-- |                                 +----'AspectFrame'
-- @

-- * Types
  Frame,
  FrameClass,
  castToFrame,

-- * Constructors
  frameNew,

-- * Methods
  frameSetLabel,
  frameGetLabel,
  frameSetLabelWidget,
  frameGetLabelWidget,
  frameSetLabelAlign,
  frameGetLabelAlign,
  ShadowType(..),
  frameSetShadowType,
  frameGetShadowType,

-- * Properties
  frameLabel,
  frameShadowType
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes		(Attr(..))
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.General.Enums	(ShadowType(..))

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Creates a new 'Frame' without a label.
--
-- * A label can later be set by calling 'frameSetLabel'.
--
frameNew :: IO Frame
frameNew =
  makeNewObject mkFrame $
  liftM (castPtr :: Ptr Widget -> Ptr Frame) $
  {# call unsafe frame_new #}
    nullPtr

--------------------
-- Methods

-- | Sets the text of the label.
--
frameSetLabel :: FrameClass self => self
 -> String -- ^ @label@ - the text to use as the label of the frame
 -> IO ()
frameSetLabel self label =
  withUTFString label $ \labelPtr ->
  {# call frame_set_label #}
    (toFrame self)
    labelPtr

-- | Sets the label widget for the frame. This is the widget that will appear
-- embedded in the top edge of the frame as a title.
--
frameSetLabelWidget :: (FrameClass self, WidgetClass labelWidget) => self
 -> labelWidget
 -> IO ()
frameSetLabelWidget self labelWidget =
  {# call frame_set_label_widget #}
    (toFrame self)
    (toWidget labelWidget)

-- | Retrieves the label widget for the frame. See 'frameSetLabelWidget'.
--
frameGetLabelWidget :: FrameClass self => self
 -> IO (Maybe Widget)
frameGetLabelWidget self = do
  widgetPtr <- {# call frame_get_label_widget #}
    (toFrame self)
  if widgetPtr == nullPtr
    then return Nothing
    else liftM Just $ makeNewObject mkWidget (return widgetPtr)

-- | Sets the horazontal alignment of the frame widget's label. The default value for a
-- newly created frame is 0.0.
--
frameSetLabelAlign :: FrameClass self => self
 -> Float -- ^ @xalign@ - The position of the label along the top edge of the
          -- widget. A value of 0.0 represents left alignment; 1.0 represents
          -- right alignment.
 -> IO ()
frameSetLabelAlign self xalign =
  {# call frame_set_label_align #}
    (toFrame self)
    (realToFrac xalign)
    0.5

-- | Get the label's horazontal alignment.
--
frameGetLabelAlign :: FrameClass self => self
 -> IO Float
frameGetLabelAlign self =
  alloca $ \alignPtr -> do
  {# call unsafe frame_get_label_align #}
    (toFrame self)
    alignPtr
    nullPtr
  align <- peek alignPtr
  return (realToFrac align)

-- | Sets the shadow type of the frame.
--
frameSetShadowType :: FrameClass self => self -> ShadowType -> IO ()
frameSetShadowType self type_ =
  {# call frame_set_shadow_type #}
    (toFrame self)
    ((fromIntegral . fromEnum) type_)

-- | Retrieves the shadow type of the frame. See 'frameSetShadowType'.
--
frameGetShadowType :: FrameClass self => self -> IO ShadowType
frameGetShadowType self =
  liftM (toEnum . fromIntegral) $
  {# call unsafe frame_get_shadow_type #}
    (toFrame self)

-- | If the frame's label widget is a 'Label', returns the text in the label
-- widget.
--
frameGetLabel :: FrameClass self => self
 -> IO String -- ^ returns the text in the label, or if there was no label
              -- widget or the lable widget was not a 'Label' then an 
              -- exception is thrown
frameGetLabel self =
  throwIfNull "frameGetLabel: the title of the frame was not a Label widget."
  ({# call unsafe frame_get_label #}
    (toFrame self))
  >>= peekUTFString

--------------------
-- Properties

-- | Text of the frame's label.
--
frameLabel :: FrameClass self => Attr self String
frameLabel = Attr 
  frameGetLabel
  frameSetLabel

-- | Appearance of the frame border.
--
-- Default value: 'ShadowEtchedIn'
--
frameShadowType :: FrameClass self => Attr self ShadowType
frameShadowType = Attr 
  frameGetShadowType
  frameSetShadowType
