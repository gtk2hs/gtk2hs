-- GIMP Toolkit (GTK) Binding for Haskell: binding to gstreamer   -*-haskell-*-
--
--  Author : Peter Gavin
--  Created: 1-Apr-2007
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
module Media.Streaming.GStreamer.Core.Iterator (
  
  Iterator,
  Iteratable,
  IteratorFilter,
  IteratorFoldFunction,
  IteratorResult,
  
  iteratorNext,
  iteratorResync,
  iteratorFilter,
  iteratorFold,
  iteratorForeach,
  iteratorFind
  
  ) where

{#import Media.Streaming.GStreamer.Core.Types#}

import Data.Maybe (fromJust)
import System.Glib.FFI
{#import System.Glib.GValue#}
import Data.IORef

{# context lib = "gstreamer" prefix = "gst" #}

iteratorNext :: Iteratable a
             => Iterator a
             -> IO (IteratorResult, Maybe a)
iteratorNext (Iterator iterator) =
    alloca $ \elemPtr ->
        do result <- {# call iterator_next #} iterator elemPtr
           obj <- peek elemPtr >>= maybePeek peekIteratable
           return (toEnum $ fromIntegral result, obj)

iteratorResync :: Iterator a
               -> IO ()
iteratorResync (Iterator iterator) =
    {# call iterator_resync #} iterator

type CIteratorFilter =  {# type gpointer #}
                     -> {# type gpointer #}
                     -> IO {# type gint #}
marshalIteratorFilter :: Iteratable a
                      => IteratorFilter a
                      -> IO {# type GCompareFunc #}
marshalIteratorFilter iteratorFilter =
    makeIteratorFilter cIteratorFilter
    where cIteratorFilter elementPtr _ =
              do include <- peekIteratable elementPtr >>= iteratorFilter
                 return $ if include then 1 else 0
foreign import ccall "wrapper"
    makeIteratorFilter :: CIteratorFilter
                    -> IO {# type GCompareFunc #}

iteratorFilter :: Iteratable a
               => Iterator a
               -> IteratorFilter a
               -> IO (Iterator a)
iteratorFilter (Iterator iterator) filter =
    do cFilter <- marshalIteratorFilter filter
       {# call iterator_filter #} iterator cFilter nullPtr >>=
           newIterator

{- type IteratorFoldFunction itemT accumT = itemT
                                         -> accumT
                                         -> IO (Maybe accumT) -}
type CIteratorFoldFunction =  {# type gpointer #}
                           -> GValue
                           -> {# type gpointer #}
                           -> IO {# type gboolean #}
marshalIteratorFoldFunction :: Iteratable itemT
                            => IteratorFoldFunction itemT accumT
                            -> IORef accumT
                            -> IO {# type GstIteratorFoldFunction #}
marshalIteratorFoldFunction iteratorFoldFunction accumRef =
    makeIteratorFoldFunction cIteratorFoldFunction
    where cIteratorFoldFunction :: CIteratorFoldFunction
          cIteratorFoldFunction itemPtr _ _ =
              do item <- peekIteratable itemPtr
                 accum <- readIORef accumRef
                 (continue, accum') <- iteratorFoldFunction item accum
                 writeIORef accumRef accum'
                 return $ fromBool continue
foreign import ccall "wrapper"
    makeIteratorFoldFunction :: CIteratorFoldFunction
                             -> IO {# type GstIteratorFoldFunction #}

iteratorFold :: Iteratable itemT
             => Iterator itemT
             -> accumT
             -> IteratorFoldFunction itemT accumT
             -> IO (IteratorResult, accumT)
iteratorFold (Iterator iterator) init func =
    do accumRef <- newIORef init
       func' <- marshalIteratorFoldFunction func accumRef
       result <- {# call iterator_fold #} iterator
                                          func'
                                          (GValue nullPtr)
                                          nullPtr
       freeHaskellFunPtr func'
       accum <- readIORef accumRef
       return (toEnum $ fromIntegral result, accum)

iteratorForeach :: Iteratable itemT
                => Iterator itemT
                -> (itemT -> IO ())
                -> IO IteratorResult
iteratorForeach iterator func =
    do (result, _) <- iteratorFold iterator () $ \item _ ->
                          func item >> return (True, ())
       return result

iteratorFind :: Iteratable itemT
             => Iterator itemT
             -> (itemT -> IO Bool)
             -> IO (IteratorResult, Maybe itemT)
iteratorFind iterator pred =
    iteratorFold iterator Nothing $ \item accum ->
        do found <- pred item
           if found
               then return (False, Just item)
               else return (True, accum)
