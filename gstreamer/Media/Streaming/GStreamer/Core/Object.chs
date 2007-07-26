-- GIMP Toolkit (GTK) Binding for Haskell: binding to gstreamer   -*-haskell-*-
--
--  Author : Peter Gavin
--  Created: 1-Apr-2007
--
--  Version $Revision$ from $Date$
--
--  Copyright (c) 2007 Peter Gavin
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Library General Public
--  License as published by the Free Software Foundation; either
--  version 2 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Library General Public License for more details.
--
module Media.Streaming.GStreamer.Core.Object (

  Object,
  ObjectClass,
  castToObject,
  toObject,
  fromObject,
  objectSetFlags,
  objectSetName,
  objectGetName,
  objectSetParent,
  objectGetParent,
  objectUnparent,
  objectGetNamePrefix,
  objectSetNamePrefix,
  objectHasAncestor,
  objectWithLock,
  objectWithTrylock,
  objectLock,
  objectTrylock,
  objectUnlock,
  
  onObjectParentSet,
  afterObjectParentSet,
  onObjectParentUnset,
  afterObjectParentUnset,
  
  objectName
  
  ) where

import Control.Exception     ( bracket,
                               bracket_ )
import Control.Monad         ( liftM,
                               when )
import System.Glib.FFI
import System.Glib.GObject
import System.Glib.UTFString ( withUTFString
                             , readUTFString )
import System.Glib.Properties ( newAttrFromMaybeStringProperty )
import System.Glib.Attributes ( Attr )
import System.Glib.Signals
{#import Media.Streaming.GStreamer.Core.Types#}
{#import Media.Streaming.GStreamer.Core.Signals#}

{# context lib = "gstreamer" prefix = "gst" #}

objectSetName :: ObjectClass obj =>
                 obj
              -> Maybe String
              -> IO Bool
objectSetName obj name =
    liftM toBool $ maybeWith withUTFString name $
              {# call object_set_name #} (toObject obj)

objectGetName :: ObjectClass obj =>
                 obj
              -> IO (Maybe String)
objectGetName obj =
    {# call object_get_name #} (toObject obj) >>= maybePeek readUTFString

objectSetParent :: (ObjectClass obj,
                    ObjectClass parent) =>
                   obj
                -> parent
                -> IO Bool
objectSetParent obj parent =
    liftM toBool $ {# call object_set_parent #} (toObject obj) (toObject parent)

objectGetParent :: ObjectClass obj =>
                   obj
                -> IO (Maybe Object)
objectGetParent obj =
    {# call object_get_parent #} (toObject obj) >>=
        maybePeek takeObject

objectUnparent :: ObjectClass obj =>
                  obj
               -> IO ()
objectUnparent obj =
    {# call object_unparent #} $ toObject obj

objectGetNamePrefix :: ObjectClass obj =>
                       obj
                    -> IO (Maybe String)
objectGetNamePrefix obj =
    {# call object_get_name_prefix #} (toObject obj) >>= maybePeek readUTFString

objectSetNamePrefix :: ObjectClass obj =>
                       obj
                    -> Maybe String
                    -> IO ()
objectSetNamePrefix obj namePrefix =
    maybeWith withUTFString namePrefix $ {# call object_set_name_prefix #} (toObject obj)

objectHasAncestor :: (ObjectClass obj, ObjectClass obj') =>
                     obj
                  -> obj'
                  -> IO Bool
objectHasAncestor object ancestor =
    liftM toBool $ {# call object_has_ancestor #} (toObject object) (toObject ancestor)

objectWithLock :: ObjectClass objectT =>
                  objectT
               -> IO a
               -> IO a
objectWithLock object action =
    bracket_ (objectLock object)
             (objectUnlock object)
             action

objectWithTrylock :: ObjectClass objectT =>
                     objectT
                  -> IO a
                  -> IO (Maybe a)
objectWithTrylock object action =
    bracket (objectTrylock object)
             (\locked -> when locked $ objectUnlock object)
             (\locked -> if locked
                            then liftM Just $ action
                            else return Nothing)

objectLock :: ObjectClass objectT =>
              objectT
           -> IO ()
objectLock object =
    withObject (toObject object) cGstObjectLock
foreign import ccall unsafe "_hs_gst_object_lock"
    cGstObjectLock :: Ptr Object
                   -> IO ()

objectTrylock :: ObjectClass objectT =>
                 objectT
              -> IO Bool
objectTrylock object =
    liftM toBool $ withObject (toObject object) cGstObjectTrylock
foreign import ccall unsafe "_hs_gst_object_trylock"
    cGstObjectTrylock :: Ptr Object
                      -> IO {# type gboolean #}

objectUnlock :: ObjectClass objectT =>
                objectT
             -> IO ()
objectUnlock object =
    withObject (toObject object) cGstObjectUnlock
foreign import ccall unsafe "_hs_gst_object_unlock"
    cGstObjectUnlock :: Ptr Object
                     -> IO ()

onObjectParentSet, afterObjectParentSet :: ObjectClass objectT
                                        => objectT
                                        -> (GObject -> IO ())
                                        -> IO (ConnectId objectT)
onObjectParentSet =
    connect_OBJECT__NONE "parent-set" False
afterObjectParentSet =
    connect_OBJECT__NONE "parent-set" True

onObjectParentUnset, afterObjectParentUnset :: ObjectClass objectT
                                            => objectT
                                            -> (GObject -> IO ())
                                            -> IO (ConnectId objectT)
onObjectParentUnset =
    connect_OBJECT__NONE "parent-unset" False
afterObjectParentUnset =
    connect_OBJECT__NONE "parent-unset" True

objectName :: ObjectClass objectT
           => Attr objectT (Maybe String)
objectName = newAttrFromMaybeStringProperty "name"
