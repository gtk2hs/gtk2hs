-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget HandleBox
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
--
--  Version $Revision: 1.5 $ from $Date: 2005/03/16 01:42:46 $
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
-- a widget for detachable window portions
--
module Graphics.UI.Gtk.Misc.HandleBox (
-- * Detail
-- 
-- | The 'HandleBox' widget allows a portion of a window to be \"torn off\".
-- It is a bin widget which displays its child and a handle that the user can
-- drag to tear off a separate window (the float window) containing the child
-- widget. A thin ghost is drawn in the original location of the handlebox. By
-- dragging the separate window back to its original location, it can be
-- reattached.
--
-- When reattaching, the ghost and float window, must be aligned along one
-- of the edges, the snap edge. This either can be specified by the application
-- programmer explicitely, or Gtk+ will pick a reasonable default based on the
-- handle position.
--
-- To make detaching and reattaching the handlebox as minimally confusing as
-- possible to the user, it is important to set the snap edge so that the snap
-- edge does not move when the handlebox is deattached. For instance, if the
-- handlebox is packed at the bottom of a VBox, then when the handlebox is
-- detached, the bottom edge of the handlebox's allocation will remain fixed as
-- the height of the handlebox shrinks, so the snap edge should be set to
-- 'PosBottom'.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Bin'
-- |                           +----HandleBox
-- @

-- * Types
  HandleBox,
  HandleBoxClass,
  castToHandleBox,

-- * Constructors
  handleBoxNew,

-- * Methods
  ShadowType(..),
  handleBoxSetShadowType,
  handleBoxGetShadowType,
  PositionType(..),
  handleBoxSetHandlePosition,
  handleBoxGetHandlePosition,
  handleBoxSetSnapEdge,
  handleBoxGetSnapEdge,

-- * Properties
  handleBoxShadowType,
  handleBoxHandlePosition,
  handleBoxSnapEdge,

-- * Signals
  onChildAttached,
  afterChildAttached,
  onChildDetached,
  afterChildDetached
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.Attributes		(Attr(..))
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.General.Enums	(ShadowType(..), PositionType(..))

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Create a new handle box.
--
handleBoxNew :: IO HandleBox
handleBoxNew =
  makeNewObject mkHandleBox $ liftM castPtr $
  {# call unsafe handle_box_new #}

--------------------
-- Methods

-- | Sets the type of shadow to be drawn around the border of the handle box.
--
handleBoxSetShadowType :: HandleBoxClass self => self -> ShadowType -> IO ()
handleBoxSetShadowType self type_ =
  {# call handle_box_set_shadow_type #}
    (toHandleBox self)
    ((fromIntegral . fromEnum) type_)

-- | Gets the type of shadow drawn around the handle box. See
-- 'handleBoxSetShadowType'.
--
handleBoxGetShadowType :: HandleBoxClass self => self
 -> IO ShadowType -- ^ returns the type of shadow currently drawn around the
                  -- handle box.
handleBoxGetShadowType self =
  liftM (toEnum . fromIntegral) $
  {# call unsafe handle_box_get_shadow_type #}
    (toHandleBox self)

-- | Sets the side of the handlebox where the handle is drawn.
--
handleBoxSetHandlePosition :: HandleBoxClass self => self
 -> PositionType -- ^ @position@ - the side of the handlebox where the handle
                 -- should be drawn.
 -> IO ()
handleBoxSetHandlePosition self position =
  {# call handle_box_set_handle_position #}
    (toHandleBox self)
    ((fromIntegral . fromEnum) position)

-- | Gets the handle position of the handle box. See
-- 'handleBoxSetHandlePosition'.
--
handleBoxGetHandlePosition :: HandleBoxClass self => self
 -> IO PositionType -- ^ returns the current handle position.
handleBoxGetHandlePosition self =
  liftM (toEnum . fromIntegral) $
  {# call unsafe handle_box_get_handle_position #}
    (toHandleBox self)

-- | Sets the snap edge of the HandleBox. The snap edge is the edge of the
-- detached child that must be aligned with the corresponding edge of the
-- \"ghost\" left behind when the child was detached to reattach the torn-off
-- window. Usually, the snap edge should be chosen so that it stays in the same
-- place on the screen when the handlebox is torn off.
--
-- If the snap edge is not set, then an appropriate value will be guessed
-- from the handle position. If the handle position is 'PosRight' or 'PosLeft',
-- then the snap edge will be 'PosTop', otherwise it will be 'PosLeft'.
--
handleBoxSetSnapEdge :: HandleBoxClass self => self
 -> PositionType
 -> IO ()
handleBoxSetSnapEdge self edge =
  {# call handle_box_set_snap_edge #}
    (toHandleBox self)
    ((fromIntegral . fromEnum) edge)

-- | Gets the edge used for determining reattachment of the handle box. See
-- 'handleBoxSetSnapEdge'.
--
handleBoxGetSnapEdge :: HandleBoxClass self => self
 -> IO PositionType
handleBoxGetSnapEdge self =
  liftM (toEnum . fromIntegral) $
  {# call unsafe handle_box_get_snap_edge #}
    (toHandleBox self)

--------------------
-- Properties

-- | Appearance of the shadow that surrounds the container.
--
-- Default value: 'ShadowEtchedOut'
--
handleBoxShadowType :: Attr HandleBox ShadowType
handleBoxShadowType = Attr 
  handleBoxGetShadowType
  handleBoxSetShadowType

-- | Position of the handle relative to the child widget.
--
-- Default value: 'PosLeft'
--
handleBoxHandlePosition :: Attr HandleBox PositionType
handleBoxHandlePosition = Attr 
  handleBoxGetHandlePosition
  handleBoxSetHandlePosition

-- | Side of the handlebox that's lined up with the docking point to dock the
-- handlebox.
--
-- Default value: 'PosTop'
--
handleBoxSnapEdge :: Attr HandleBox PositionType
handleBoxSnapEdge = Attr 
  handleBoxGetSnapEdge
  handleBoxSetSnapEdge

--------------------
-- Signals

-- Note: for these two signales we ignore the given Widget in the handler.

-- | This signal is emitted when the contents of the handlebox are reattached
-- to the main window.
--
onChildAttached, afterChildAttached :: HandleBoxClass self => self
 -> IO ()
 -> IO (ConnectId self)
onChildAttached = connect_NONE__NONE "child-attached" False
afterChildAttached = connect_NONE__NONE "child-attached" True

-- | This signal is emitted when the contents of the handlebox are detached
-- from the main window.
--
onChildDetached, afterChildDetached :: HandleBoxClass self => self
 -> IO ()
 -> IO (ConnectId self)
onChildDetached = connect_NONE__NONE "child-detached" False
afterChildDetached = connect_NONE__NONE "child-detached" True

