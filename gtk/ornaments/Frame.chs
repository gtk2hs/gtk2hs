-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: Widget Frame
--
--  Author : Axel Simon
--          
--  Created: 15 May 2001
--
--  Version $Revision: 1.1.1.1 $ from $Date: 2002/03/24 21:56:20 $
--
--  Copyright (c) [1999.2001] Manuel Chakravarty, Axel Simon
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
-- * This container adds a frame around its contents. This is useful to
--   logically separate items in a dialog box.
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------

module Frame(
  Frame,
  FrameClass,
  castToFrame,
  frameNew,
  frameSetLabel,
  frameSetLabelWidget,
  frameSetLabelAlign,
  ShadowType(..),
  frameSetShadowType,
  frameGetLabel
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Enums	(ShadowType(..))

{# context lib="gtk" prefix="gtk" #}

-- methods

-- Create a new frame without a label. (EXPORTED)
--
-- * A label can later be set by calling @frameSetLabel.
--
frameNew :: IO Frame
frameNew = makeNewObject mkFrame $
  liftM castPtr $ {#call unsafe frame_new#} nullPtr

-- Replace the label of the frame. (EXPORTED)
--
frameSetLabel :: FrameClass f => String -> f -> IO ()
frameSetLabel label f = withCString label $ \strPtr ->
  {#call frame_set_label#} (toFrame f) strPtr

-- Replace the label with a (label) widget. (EXPORTED)
--
frameSetLabelWidget :: (FrameClass f, WidgetClass w) => w -> f -> IO ()
frameSetLabelWidget w f = 
  {#call frame_set_label_widget#} (toFrame f) (toWidget w)

-- Specify where the label should be placed. (EXPORTED)
--
-- * A value of 0.0 means left justified (the default), a value of 1.0 means
--   right justified.
--
frameSetLabelAlign :: FrameClass f => Float -> f -> IO ()
frameSetLabelAlign align f =
  {#call frame_set_label_align#} (toFrame f) (realToFrac align) 0.0

-- Set the shadow type of the frame. (EXPORTED)
--
frameSetShadowType :: FrameClass f => ShadowType -> f -> IO ()
frameSetShadowType shadow f = 
  {#call frame_set_shadow_type#} (toFrame f) ((fromIntegral.fromEnum) shadow)

-- Retrieve the label of the frame. (EXPORTED)
--
-- * An exception is thrown if a non-Label widget was set. (EXPORTED)
--
frameGetLabel :: FrameClass f => f -> IO String
frameGetLabel f = do
  strPtr <- throwIfNull 
    "frameGetLabel: the title of the frame was not a Label widget." $
    {#call unsafe frame_get_label#} (toFrame f)
  res <- peekCString strPtr
  {#call unsafe g_free#} (castPtr strPtr)
  return res


   
