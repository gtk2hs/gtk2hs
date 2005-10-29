-- -*-haskell-*-
--  GIMP Toolkit (GTK) Container child Properties
--
--  Author : Duncan Coutts
--
--  Created: 16 April 2005
--
--  Version $Revision: 1.2 $ from $Date: 2005/10/29 23:44:32 $
--
--  Copyright (C) 2005 Duncan Coutts
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
-- #hide

-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Functions for getting and setting container child properties
--
module Graphics.UI.Gtk.Abstract.ContainerChildProperties (
  containerChildGetPropertyBool,
  containerChildSetPropertyBool,

  newAttrFromContainerChildIntProperty,
  newAttrFromContainerChildUIntProperty,
  newAttrFromContainerChildBoolProperty,
  newAttrFromContainerChildEnumProperty,
  newAttrFromContainerChildFlagsProperty,
  newAttrFromContainerChildStringProperty,
  ) where

import Monad (liftM)

import System.Glib.FFI
import System.Glib.Flags
import System.Glib.UTFString
{#import Graphics.UI.Gtk.Types#}
import System.Glib.GType
import qualified System.Glib.GTypeConstants as GType
import System.Glib.GValueTypes
{#import System.Glib.GValue#}		(GValue(GValue), allocaGValue, valueInit)
import System.Glib.Attributes		(Attr, ReadAttr, WriteAttr, ReadWriteAttr,
					newAttr, readAttr, writeAttr)

{# context lib="gtk" prefix="gtk" #}

containerChildSetPropertyInternal ::
 (ContainerClass container, WidgetClass child)
 => GType
 -> (GValue -> a -> IO ())
 -> String
 -> child
 -> container
 -> a
 -> IO ()
containerChildSetPropertyInternal gtype valueSet prop child container val =
  withCString prop $ \propertyNamePtr ->
  allocaGValue $ \gvalue -> do
  valueInit gvalue gtype
  valueSet gvalue val
  {# call container_child_set_property #}
    (toContainer container)
    (toWidget child)
    propertyNamePtr
    gvalue

containerChildGetPropertyInternal ::
 (ContainerClass container, WidgetClass child)
 => GType
 -> (GValue -> IO a)
 -> String
 -> child
 -> container
 -> IO a
containerChildGetPropertyInternal gtype valueGet prop child container =
  withCString prop $ \propertyNamePtr ->
  allocaGValue $ \gvalue -> do
  valueInit gvalue gtype
  {# call container_child_set_property #}
    (toContainer container)
    (toWidget child)
    propertyNamePtr
    gvalue
  valueGet gvalue

-- Versions for specific types:
-- we actually don't use any others than bool at the moment
--

containerChildGetPropertyBool :: (ContainerClass container, WidgetClass child)
 => String -> child -> container -> IO Bool
containerChildGetPropertyBool =
          containerChildGetPropertyInternal GType.bool valueGetBool

containerChildSetPropertyBool :: (ContainerClass container, WidgetClass child)
 => String -> child -> container -> Bool -> IO ()
containerChildSetPropertyBool =
          containerChildSetPropertyInternal GType.bool valueSetBool

-- Convenience functions to make attribute implementations in the other modules
-- shorter and more easily extensible.
--

newAttrFromContainerChildIntProperty ::
 (ContainerClass container, WidgetClass child)
 => String -> child -> Attr container Int
newAttrFromContainerChildIntProperty propName child = newAttr
  (containerChildGetPropertyInternal GType.int valueGetInt propName child)
  (containerChildSetPropertyInternal GType.int valueSetInt propName child)

newAttrFromContainerChildUIntProperty ::
 (ContainerClass container, WidgetClass child)
 => String -> child -> Attr container Int
newAttrFromContainerChildUIntProperty propName child = newAttr
  (containerChildGetPropertyInternal GType.uint
    (\gv -> liftM fromIntegral $ valueGetUInt gv) propName child)
  (containerChildSetPropertyInternal GType.uint
    (\gv v -> valueSetUInt gv (fromIntegral v)) propName child)

newAttrFromContainerChildBoolProperty ::
 (ContainerClass container, WidgetClass child)
 => String -> child -> Attr container Bool
newAttrFromContainerChildBoolProperty propName child = newAttr
  (containerChildGetPropertyInternal GType.bool valueGetBool propName child)
  (containerChildSetPropertyInternal GType.bool valueSetBool propName child)

newAttrFromContainerChildEnumProperty ::
 (ContainerClass container, WidgetClass child, Enum enum)
 => String -> child -> Attr container enum
newAttrFromContainerChildEnumProperty propName child = newAttr
  (containerChildGetPropertyInternal GType.enum valueGetEnum propName child)
  (containerChildSetPropertyInternal GType.enum valueSetEnum propName child)

newAttrFromContainerChildFlagsProperty ::
 (ContainerClass container, WidgetClass child, Flags flag)
 => String -> child -> Attr container [flag]
newAttrFromContainerChildFlagsProperty propName child = newAttr
  (containerChildGetPropertyInternal GType.flags valueGetFlags propName child)
  (containerChildSetPropertyInternal GType.flags valueSetFlags propName child)

newAttrFromContainerChildStringProperty ::
 (ContainerClass container, WidgetClass child)
 => String -> child -> Attr container String
newAttrFromContainerChildStringProperty propName child = newAttr
  (containerChildGetPropertyInternal GType.string valueGetString propName child)
  (containerChildSetPropertyInternal GType.string valueSetString propName child)
