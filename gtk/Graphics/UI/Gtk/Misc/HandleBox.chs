-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget HandleBox
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.1 $ from $Date: 2005/01/08 15:28:02 $
--
--  Copyright (c) 1999..2002 Axel Simon
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
-- |
--
-- Add a handle to some other widget so that it can be detached and 
-- reattached from the main application.
--
-- * The GtkHandleBox widget allows a portion of a window to be \"torn off\". It
--   is a bin widget which displays its child and a handle that the user can 
--   drag to tear off a separate window (the float window) containing the 
--   child widget. A thin ghost is drawn in the original location of the 
--   handlebox. By dragging the separate window back to its original location,
--   it can be reattached.
--   When reattaching, the ghost and float window, must be aligned along one 
--   of the edges, the snap edge. This either can be specified by the 
--   application programmer explicitely, or GTK+ will pick a reasonable 
--   default based on the handle position.
--   To make detaching and reattaching the handlebox as minimally confusing 
--   as possible to the user, it is important to set the snap edge so that 
--   the snap edge does not move when the handlebox is deattached. For 
--   instance, if the handlebox is packed at the bottom of a 'VBox',
--   then when 
--   the handlebox is detached, the bottom edge of the handlebox's allocation 
--   will remain fixed as the height of the handlebox shrinks, so the snap 
--   edge should be set to 'PosBottom'.
--

module Graphics.UI.Gtk.Misc.HandleBox (
  HandleBox,
  HandleBoxClass,
  castToHandleBox,
  handleBoxNew,
  ShadowType(..),
  handleBoxSetShadowType,
  handleBoxGetShadowType,
  PositionType(..),
  handleBoxSetHandlePosition,
  handleBoxGetHandlePosition,
  handleBoxSetSnapEdge,
  handleBoxGetSnapEdge,
  onChildAttached,
  afterChildAttached,
  onChildDetached,
  afterChildDetached
  ) where

import Monad	(liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.General.Enums	(ShadowType(..), PositionType(..))

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Create a new handle box.
--
handleBoxNew :: IO HandleBox
handleBoxNew  = makeNewObject mkHandleBox $ 
  liftM castPtr {#call unsafe handle_box_new#}

-- | Set the shadow type of the detached box.
--
handleBoxSetShadowType :: HandleBoxClass hb => hb -> ShadowType -> IO ()
handleBoxSetShadowType hb shadow = {#call handle_box_set_shadow_type#}
  (toHandleBox hb) ((fromIntegral.fromEnum) shadow)

-- | Get the shadow type of the detached box.
--
handleBoxGetShadowType :: HandleBoxClass hb => hb -> IO ShadowType
handleBoxGetShadowType hb = liftM (toEnum.fromIntegral) $
  {#call unsafe handle_box_get_shadow_type#} (toHandleBox hb)

-- | Set the position of the handle.
--
handleBoxSetHandlePosition :: HandleBoxClass hb => hb -> PositionType -> IO ()
handleBoxSetHandlePosition hb pos = {#call handle_box_set_handle_position#} 
  (toHandleBox hb) ((fromIntegral.fromEnum) pos)

-- | Get the position of the handle.
--
handleBoxGetHandlePosition :: HandleBoxClass hb => hb -> IO PositionType
handleBoxGetHandlePosition hb = liftM (toEnum.fromIntegral) $
  {#call unsafe handle_box_get_handle_position#} (toHandleBox hb)

-- | Set the snap edge of the HandleBox.
--
-- * The snap edge is the edge of the detached child that must be aligned with
--   the corresponding edge of the \"ghost\" left behind when the child was
--   detached to reattach the torn-off window. Usually, the snap edge should
--   be chosen so that it stays in the same place on the screen when the
--   handlebox is torn off. If the snap edge is not set, then an appropriate
--   value will be guessed from the handle position. If the handle position is
--   'PosRight' or 'PosLeft', then the snap edge will
--   be 'PosTop', otherwise it will be 'PosLeft'.
--
handleBoxSetSnapEdge :: HandleBoxClass hb => hb -> PositionType -> IO ()
handleBoxSetSnapEdge hb pos = {#call handle_box_set_snap_edge#}
  (toHandleBox hb) ((fromIntegral.fromEnum) pos)

-- | Gets the edge used for determining reattachment of the handle box. See
-- 'handleBoxSetSnapEdge'.
--
handleBoxGetSnapEdge :: HandleBoxClass hb => hb -> IO PositionType
handleBoxGetSnapEdge hb = liftM (toEnum.fromIntegral) $
  {#call unsafe handle_box_get_snap_edge#} (toHandleBox hb)

-- signals

-- | Emitted when the contents of the handlebox
-- are reattached to the main window.
--
-- * (INTERNAL) We ignore the given Widget.
--
onChildAttached, afterChildAttached :: HandleBoxClass hb => hb -> IO () ->
                                       IO (ConnectId hb)
onChildAttached = connect_NONE__NONE "child-attached" False
afterChildAttached = connect_NONE__NONE "child-attached" True


-- | Emitted when the 'HandleBox' is
-- detached form the main window.
--
onChildDetached, afterChildDetached :: HandleBoxClass hb => hb -> IO () ->
                                       IO (ConnectId hb)
onChildDetached = connect_NONE__NONE "child-detached" False
afterChildDetached = connect_NONE__NONE "child-detached" True


