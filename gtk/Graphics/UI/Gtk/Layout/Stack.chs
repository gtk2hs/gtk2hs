{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widgets Stack
--
--  Author : Moritz Schulte
--
--  Created: 27 April 2016
--
--  Copyright (C) 2015 Moritz Schulte
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
-- A widget which controls the alignment and size of its child
--
module Graphics.UI.Gtk.Layout.Stack (
-- * Detail
--
-- The 'Stack' widget is a container which only shows one of its
-- children at a time. In contrast to 'Notebook', 'Stack' does not
-- provide a means for users to change the visible child. Instead, the
-- 'StackSwitcher' widget can be used with 'Stack' to provide this
-- functionality.
--
-- Transitions between pages can be animated as slides or fades. This
-- can be controlled with 'stackSetTransitionType'. These
-- animations respect the 'gtk-enable-animations' setting.
--       
-- The GtkStack widget was added in GTK+ 3.10.
--
-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Stack'
-- @

-- * Types
#if GTK_CHECK_VERSION(3,10,0)
         Stack
       , castToStack
       , gTypeStack
       , toStack
       , StackTransitionType(..)
-- * Constructors
       , stackNew

-- * Methods
       , stackAddNamed
       , stackAddTitled
       , stackGetTransitionType
       , stackSetTransitionType
       , stackSetTransitionDuration
       , stackGetTransitionDuration
       , stackGetChildByName
       , stackSetVisibleChild
       , stackSetVisibleChildName
       , stackGetVisibleChildName
       , stackSetVisibleChildFull
       , stackSetHomogeneous
       , stackGetHomogeneous
       , stackSetHhomogeneous
       , stackGetHhomogeneous
       , stackSetVhomogeneous
       , stackGetVhomogeneous
       , stackGetTransitionRunning
       , stackSetInterpolateSize
       , stackGetInterpolateSize

-- * Attributes
       , stackHhomogeneous
       , stackHomogeneous
       , stackInterpolateSize
       , stackTransitionDuration
       , stackTransitionRunning
       , stackTransitionType
       , stackVhomogeneous
       , stackVisibleChild
       , stackVisibleChildName
#endif
) where

#if GTK_CHECK_VERSION(3,10,0)

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
import Graphics.UI.Gtk.General.Enums    (StackTransitionType(..))
{#import Graphics.UI.Gtk.Types#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Creates a new 'Stack' container.
--
stackNew :: IO Stack
stackNew =
  makeNewObject mkStack $
  liftM (castPtr :: Ptr Widget -> Ptr Stack) $
  {# call unsafe stack_new #}

--------------------
-- Methods

-- | Adds a child to stack . The child is identified by the name.
stackAddNamed :: (StackClass self, WidgetClass child, GlibString name) => self
 -> child
 -> name
 -> IO ()
stackAddNamed self child name =
  withUTFString name $ \namePtr ->
    {# call stack_add_named  #}
      (toStack self)
      (toWidget child)
      namePtr

-- | Adds a child to stack. The child is identified by the name. The
-- title will be used by 'StackSwitcher' to represent child in a tab
-- bar, so it should be short.
stackAddTitled :: (StackClass self, WidgetClass child,
                   GlibString name, GlibString title) => self
 -> child
 -> name
 -> title
 -> IO ()
stackAddTitled self child name title =
  withUTFString name $ \namePtr ->
    withUTFString title $ \titlePtr ->
      {# call stack_add_titled  #}
        (toStack self)
        (toWidget child)
        namePtr
        titlePtr

-- | Sets the type of animation that will be used for transitions
-- between pages in stack . Available types include various kinds of
-- fades and slides. The transition type can be changed without
-- problems at runtime, so it is possible to change the animation
-- based on the page that is about to become current.
stackSetTransitionType :: StackClass self => self
 -> StackTransitionType
 -> IO ()
stackSetTransitionType self transitionType =
  {# call unsafe stack_set_transition_type #}
    (toStack self)
    (fromIntegral $ fromEnum transitionType)

-- | Gets the type of animation that will be used for transitions
-- between pages in stack.
stackGetTransitionType :: StackClass self => self
 -> IO StackTransitionType
stackGetTransitionType self =
  liftM (toEnum . fromIntegral) $
  {# call unsafe stack_get_transition_type #}
    (toStack self)

-- | Sets the duration that transitions between pages in stack will
-- take.
stackSetTransitionDuration :: StackClass self => self
 -> Int
 -> IO ()
stackSetTransitionDuration self duration =
  {# call unsafe stack_set_transition_duration #}
    (toStack self)
    (fromIntegral duration)

-- | Returns the amount of time (in milliseconds) that transitions
-- between pages in stack will take.
stackGetTransitionDuration :: StackClass self => self
 -> IO Int
stackGetTransitionDuration self =
  liftM fromIntegral $
  {# call unsafe stack_get_transition_duration #}
    (toStack self)

-- | Finds the child of the GtkStack with the name given as the
-- argument. Returns Nothing if there is no child with this name.
stackGetChildByName :: (StackClass self, GlibString name) => self
 -> name
 -> IO Widget
stackGetChildByName self name =
  withUTFString name $ \namePtr ->
    makeNewObject mkWidget $
    {# call unsafe stack_get_child_by_name #}
      (toStack self)
      namePtr

-- | Gets Just the currently visible child of stack, or Nothing if
-- there are no visible children.
stackGetVisibleChild :: StackClass self => self
 -> IO (Maybe Widget)
stackGetVisibleChild self =
  maybeNull (makeNewObject mkWidget) $
  {# call unsafe stack_get_visible_child #}
    (toStack self)

-- | Makes child the visible child of stack. If child is different
-- from the currently visible child, the transition between the two
-- will be animated with the current transition type of stack. Note
-- that the child widget has to be visible itself (see 'widgetShow')
-- in order to become the visible child of stack.
stackSetVisibleChild :: (StackClass self, WidgetClass child) => self
 -> child
 -> IO ()
stackSetVisibleChild self child =
    {# call unsafe stack_set_visible_child #}
      (toStack self)
      (toWidget child)

-- | Makes the child with the given name visible.  If child is
-- different from the currently visible child, the transition between
-- the two will be animated with the current transition type of stack.
-- Note that the child widget has to be visible itself (see
-- `widgetShow') in order to become the visible child of stack.
stackSetVisibleChildName :: (StackClass self, GlibString name) => self
 -> name
 -> IO ()
stackSetVisibleChildName self name =
  withUTFString name $ \namePtr ->
    {# call unsafe stack_set_visible_child_name #}
      (toStack self)
      namePtr

-- | Returns the name of the currently visible child of stack, or
-- Nothing if there is no visible child.
stackGetVisibleChildName :: (StackClass self, GlibString name) => self
 -> IO (Maybe name)
stackGetVisibleChildName self =
  {# call unsafe stack_get_visible_child_name #}
      (toStack self)
  >>= maybePeekUTFString

-- | Makes the child with the given name visible.  Note that the child
-- widget has to be visible itself (see 'widgetShow') in order to
-- become the visible child of stack .
stackSetVisibleChildFull :: (StackClass self, GlibString name) => self
 -> name
 -> StackTransitionType
 -> IO ()
stackSetVisibleChildFull self name transitionType =
  withUTFString name $ \namePtr ->
    {# call unsafe stack_set_visible_child_full #}
      (toStack self)
      namePtr
      (fromIntegral $ fromEnum transitionType)

-- | Sets the stack to be homogeneous or not. If it is homogeneous,
-- the stack will request the same size for all its children. If it
-- isn't, the stack may change size when a different child becomes
-- visible.
stackSetHomogeneous :: StackClass self => self -> Bool -> IO ()
stackSetHomogeneous self homogeneous =
  {# call stack_set_homogeneous #}
    (toStack self)
    (fromBool homogeneous)

-- | Gets whether stack is homogeneous. See 'stackSetHomogeneous'.
stackGetHomogeneous :: StackClass self => self -> IO Bool
stackGetHomogeneous self =
  liftM toBool $
  {# call stack_get_homogeneous #}
    (toStack self)

-- | Sets the stack to be horizontally homogeneous or not. If it is
-- homogeneous, the stack will request the same width for all its
-- children. If it isn't, the stack may change width when a different
-- child becomes visible.
stackSetHhomogeneous :: StackClass self => self -> Bool -> IO ()
stackSetHhomogeneous self hhomogeneous =
  {# call stack_set_hhomogeneous #}
    (toStack self)
    (fromBool hhomogeneous)

-- | Gets whether stack is horizontally homogeneous. See
-- 'stackSetHhomogeneous'.
stackGetHhomogeneous :: StackClass self => self -> IO Bool
stackGetHhomogeneous self =
  liftM toBool $
  {# call stack_get_hhomogeneous #}
    (toStack self)

-- | Sets the stack to be vertically homogeneous or not. If it is
-- homogeneous, the stack will request the same height for all its
-- children. If it isn't, the stack may change height when a different
-- child becomes visible.
stackSetVhomogeneous :: StackClass self => self -> Bool -> IO ()
stackSetVhomogeneous self vhomogeneous =
  {# call stack_set_vhomogeneous #}
    (toStack self)
    (fromBool vhomogeneous)

-- | Gets whether stack is vertically homogeneous. See
-- 'stackSetVhomogeneous'.
stackGetVhomogeneous :: StackClass self => self -> IO Bool
stackGetVhomogeneous self =
  liftM toBool $
  {# call stack_get_vhomogeneous #}
    (toStack self)

-- | Returns whether the stack is currently in a transition from one
-- page to another.
stackGetTransitionRunning :: StackClass self => self -> IO Bool
stackGetTransitionRunning self =
  liftM toBool $
  {# call stack_get_transition_running #}
    (toStack self)

-- | Returns wether the stack is set up to interpolate between the
-- sizes of children on page switch.
stackGetInterpolateSize :: StackClass self => self -> IO Bool
stackGetInterpolateSize self =
  liftM toBool $
  {# call stack_get_interpolate_size #}
    (toStack self)

-- | Sets whether or not the stack will interpolate its size when
-- changing the visible child. If the 'interpolate-size' property is
-- set to True, stack will interpolate its size between the current
-- one and the one it'll take after changing the visible child,
-- according to the set transition duration.
stackSetInterpolateSize :: StackClass self => self -> Bool -> IO ()
stackSetInterpolateSize self interpolateSize =
  {# call stack_set_interpolate_size #}
    (toStack self)
    (fromBool interpolateSize)

--------------------
-- Attributes

-- | True if the stack allocates the same width for all children.
stackHhomogeneous :: StackClass self => Attr self Bool
stackHhomogeneous = newAttr
  stackGetHhomogeneous
  stackSetHhomogeneous

-- | Homogeneous sizing.
stackHomogeneous :: StackClass self => Attr self Bool
stackHomogeneous = newAttr
  stackGetHomogeneous
  stackSetHomogeneous

-- | Whether or not the size should smoothly change when changing
-- between differently sized children.
stackInterpolateSize :: StackClass self => Attr self Bool
stackInterpolateSize = newAttr
  stackGetInterpolateSize
  stackSetInterpolateSize

-- | The animation duration, in milliseconds.
stackTransitionDuration :: StackClass self => Attr self Int
stackTransitionDuration = newAttr
  stackGetTransitionDuration
  stackSetTransitionDuration

-- | Whether or not the transition is currently running.
stackTransitionRunning :: StackClass self => ReadAttr self Bool
stackTransitionRunning = readAttr
  stackGetTransitionRunning

-- | The type of animation used to transition.
stackTransitionType :: StackClass self => Attr self StackTransitionType
stackTransitionType = newAttr
  stackGetTransitionType
  stackSetTransitionType

-- | True if the stack allocates the same height for all children.
stackVhomogeneous :: StackClass self => Attr self Bool
stackVhomogeneous = newAttr
  stackGetVhomogeneous
  stackSetVhomogeneous

-- | The widget currently visible in the stack.
stackVisibleChild :: StackClass self =>
  ReadWriteAttr self (Maybe Widget) Widget
stackVisibleChild = newAttr
  stackGetVisibleChild
  stackSetVisibleChild

-- | The name of the widget currently visible in the stack.
stackVisibleChildName :: StackClass self => ReadWriteAttr self (Maybe String) String
stackVisibleChildName = newAttr
  stackGetVisibleChildName
  stackSetVisibleChildName

#endif
