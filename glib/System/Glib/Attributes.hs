{-# OPTIONS -fglasgow-exts #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Attributes interface
--
--  Author : Duncan Coutts
--
--  Created: 21 January 2005
--
--  Copyright (C) 2005 Duncan Coutts
--
--  Partially derived from the hs-fltk and wxHaskell projects which
--  are both under LGPL compatible licenses.
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
-- Stability   : experimental
-- Portability : portable
--
-- |
--
module System.Glib.Attributes (
  -- * Attribute types
  Attr,
  ReadAttr,
  WriteAttr,
  ReadWriteAttr(Attr),

  -- * Interface for getting, setting and updating attributes
  Prop(..),
  get,
  set,
  
  -- * Internal attribute constructors
  newAttr,
  readAttr,
  writeAttr,
  ) where

infixr 0 :=,:~,:=>,:~>,::=,::~

-- | An ordinary attribute. Most attributes have the same get and set types.
type Attr o a = ReadWriteAttr o a a

-- | A read-only attribute.
type ReadAttr o a = ReadWriteAttr o a ()

-- | A write-only attribute.
type WriteAttr o b = ReadWriteAttr o () b

-- | A generalised attribute with independent get and set types.
data ReadWriteAttr o a b = Attr !(o -> IO a) !(o -> b -> IO ())


-- | Create a new attribute with a getter and setter function.
newAttr :: (o -> IO a) -> (o -> b -> IO ()) -> ReadWriteAttr o a b
newAttr getter setter = Attr getter setter

-- | Create a new read-only attribute.
readAttr :: (o -> IO a) -> ReadAttr o a
readAttr getter = Attr getter (\_ _ -> return ())

-- | Create a new write-only attribute.
writeAttr :: (o -> b -> IO ()) -> WriteAttr o b
writeAttr setter = Attr (\_ -> return ()) setter


-- | A property of a object @o@ is an attribute that is already associated with
-- a value.
data Prop o
  = forall a b.
      ReadWriteAttr o a b :=              b    -- ^ Assign a value to an
                                               --   attribute.
  | forall a b.
      ReadWriteAttr o a b :~   (  a ->    b)   -- ^ Apply an update function to
                                               --   an attribute.
  | forall a b.
      ReadWriteAttr o a b :=>  (       IO b)   -- ^ Assign the result of an IO
                                               --   action to an attribute.
  | forall a b.
      ReadWriteAttr o a b :~>  (  a -> IO b)   -- ^ Apply a IO update function
                                               --   to an attribute.
  | forall a b.
      ReadWriteAttr o a b ::=  (o      -> b)   -- ^ Assign a value to an
                                               --   attribute with the object as
                                               --   an argument.
  | forall a b.
      ReadWriteAttr o a b ::~  (o -> a -> b)   -- ^ Apply an update function to
                                               --   an attribute with the object
                                               --   as an argument.

-- | Set a number of properties for some object.
set :: w -> [Prop w] -> IO ()
set obj = mapM_ app
 where
   app (Attr getter setter :=  x) = setter obj x
   app (Attr getter setter :~  f) = getter obj >>= \v -> setter obj (f v)
   app (Attr getter setter :=> x) =                x >>= setter obj
   app (Attr getter setter :~> f) = getter obj >>= f >>= setter obj

   app (Attr getter setter ::= f) = setter obj (f obj)
   app (Attr getter setter ::~ f) = getter obj >>= \v -> setter obj (f obj v)

-- | Get an Attr of an object.
get :: w -> ReadWriteAttr w a b -> IO a
get w (Attr getter setter) = getter w
