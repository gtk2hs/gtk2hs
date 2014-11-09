{-# LANGUAGE ExistentialQuantification #-}
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
-- Attributes interface
--
-- Attributes of an object can be get and set. Getting the value of an
-- object's attribute is straingtforward. As an example consider a @button@
-- widget and an attribute called @buttonLabel@.
--
-- > value <- get button buttonLabel
--
-- The syntax for setting or updating an attribute is only slightly more
-- complex. At the simplest level it is just:
--
-- > set button [ buttonLabel := value ]
--
-- However as the list notation would indicate, you can set or update multiple
-- attributes of the same object in one go:
--
-- > set button [ buttonLabel := value, buttonFocusOnClick := False ]
--
-- You are not limited to setting the value of an attribute, you can also
-- apply an update function to an attribute's value. That is the function
-- receives the current value of the attribute and returns the new value.
--
-- > set spinButton [ spinButtonValue :~ (+1) ]
--
-- There are other variants of these operators, (see 'AttrOp'). ':=>' and
-- ':~>' and like ':=' and ':~' but operate in the 'IO' monad rather
-- than being pure. There is also '::=' and '::~' which take the object
-- as an extra parameter.
--
-- Attributes can be read only, write only or both read\/write.
--
module System.Glib.Attributes (
  -- * Attribute types
  Attr,
  ReadAttr,
  WriteAttr,
  ReadWriteAttr,

  -- * Interface for getting, setting and updating attributes
  AttrOp(..),
  get,
  set,

  -- * Internal attribute constructors
  newNamedAttr,
  readNamedAttr,
  writeNamedAttr,
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
data ReadWriteAttr o a b = Attr String !(o -> IO a) !(o -> b -> IO ())

instance Show (ReadWriteAttr o a b) where
  show (Attr str _ _) = str

-- | Create a new attribute with a getter and setter function.
newNamedAttr :: String -> (o -> IO a) -> (o -> b -> IO ()) -> ReadWriteAttr o a b
newNamedAttr prop getter setter = Attr prop getter setter

-- | Create a new read-only attribute.
readNamedAttr :: String -> (o -> IO a) -> ReadAttr o a
readNamedAttr prop getter = Attr prop getter (\_ _ -> return ())

-- | Create a new write-only attribute.
writeNamedAttr :: String -> (o -> b -> IO ()) -> WriteAttr o b
writeNamedAttr prop setter = Attr prop (\_ -> return ()) setter

-- | Create a new attribute with a getter and setter function.
newAttr :: (o -> IO a) -> (o -> b -> IO ()) -> ReadWriteAttr o a b
newAttr getter setter = Attr "unnamed attribute" getter setter

-- | Create a new read-only attribute.
readAttr :: (o -> IO a) -> ReadAttr o a
readAttr getter = Attr "unnamed attribute" getter (\_ _ -> return ())

-- | Create a new write-only attribute.
writeAttr :: (o -> b -> IO ()) -> WriteAttr o b
writeAttr setter = Attr "unnamed attribute" (\_ -> return ()) setter

-- | A set or update operation on an attribute.
data AttrOp o
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
set :: o -> [AttrOp o] -> IO ()
set obj = mapM_ app
 where
   app (Attr _ getter setter :=  x) = setter obj x
   app (Attr _ getter setter :~  f) = getter obj >>= \v -> setter obj (f v)
   app (Attr _ getter setter :=> x) =                x >>= setter obj
   app (Attr _ getter setter :~> f) = getter obj >>= f >>= setter obj

   app (Attr _ getter setter ::= f) = setter obj (f obj)
   app (Attr _ getter setter ::~ f) = getter obj >>= \v -> setter obj (f obj v)

-- | Get an Attr of an object.
get :: o -> ReadWriteAttr o a b -> IO a
get o (Attr _ getter setter) = getter o
