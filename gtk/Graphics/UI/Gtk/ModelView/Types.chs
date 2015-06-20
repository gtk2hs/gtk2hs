{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) CustomStore TreeModel
--
--  Author : Duncan Coutts
--
--  Created: 31 March 2006
--
--  Copyright (C) 2006-2007 Duncan Coutts, Axel Simon
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
-- Common types and classes for the ModelView modules.
--
module Graphics.UI.Gtk.ModelView.Types (
  TypedTreeModel(..),
  TypedTreeModelClass,
  toTypedTreeModel,
  unsafeTreeModelToGeneric,

  TypedTreeModelSort(..),
  unsafeTreeModelSortToGeneric,
  TypedTreeModelFilter(..),
  unsafeTreeModelFilterToGeneric,

  -- TreeIter
  TreeIter(..),
  receiveTreeIter,
  peekTreeIter,
  treeIterSetStamp,

  -- TreePath
  TreePath,
  NativeTreePath(..),
  newTreePath,
  withTreePath,
  maybeWithTreePath,
  peekTreePath,
  fromTreePath,
  stringToTreePath,

  -- Columns
  ColumnAccess(..),
  ColumnId(..),

  -- Storing the model in a ComboBox
  comboQuark,
  ) where

import GHC.Exts (unsafeCoerce#)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GValue         (GValue)
import System.Glib.GObject        (Quark, quarkFromString)
{#import Graphics.UI.Gtk.Types#}        (TreeModel, TreeModelSort, TreeModelFilter,
                                   Pixbuf)
import Data.Char ( isDigit )
import Control.Monad ( liftM )

{# context lib="gtk" prefix="gtk" #}

newtype TypedTreeModel row = TypedTreeModel (ForeignPtr (TypedTreeModel row))

class TypedTreeModelClass model where
  dummy :: model a -> a
  dummy _ = error "not used"
  -- this is to get the right kind for model :: * -> *
  -- TODO: when haddock is fixed we can use an explicit kind annotation

toTypedTreeModel :: TypedTreeModelClass model => model row -> TypedTreeModel row
toTypedTreeModel = unsafeCoerce#

unsafeTreeModelToGeneric :: TreeModel -> model row
unsafeTreeModelToGeneric = unsafeCoerce#

instance TypedTreeModelClass TypedTreeModel

newtype TypedTreeModelSort row = TypedTreeModelSort (ForeignPtr (TypedTreeModelSort row))

unsafeTreeModelSortToGeneric :: TreeModelSort -> TypedTreeModelSort row
unsafeTreeModelSortToGeneric = unsafeCoerce#

instance TypedTreeModelClass TypedTreeModelSort

newtype TypedTreeModelFilter row = TypedTreeModelFilter (ForeignPtr (TypedTreeModelFilter row))

unsafeTreeModelFilterToGeneric :: TreeModelFilter -> TypedTreeModelFilter row
unsafeTreeModelFilterToGeneric = unsafeCoerce#

instance TypedTreeModelClass TypedTreeModelFilter

-- | Tree Iterator: a pointer to an entry in a
-- 'Graphics.UI.Gtk.ModelView.TreeModel'. The constructor of this structure is
-- public for the sake of creating custom tree models. The first value is a
-- time stamp that is handled by the functions that interface with Gtk. The
-- time stamps are used to print warnings if programmers use an iter to a
-- model that has changed meanwhile. The other three fields are used by the
-- custom model implementation to implement an indexing scheme. The precise
-- use of the three words is therefore implementation specific. See also
-- 'TreePath'.
--
data TreeIter = TreeIter {-# UNPACK #-} !CInt !Word32 !Word32 !Word32
              deriving Show

{#pointer *TreeIter as TreeIterPtr -> TreeIter #}

instance Storable TreeIter where
  sizeOf _ = {# sizeof TreeIter #}
  alignment _ = alignment (undefined :: CInt)
  peek ptr = do
    stamp      <- {# get TreeIter->stamp      #} ptr
    user_data  <- {# get TreeIter->user_data  #} ptr
    user_data2 <- {# get TreeIter->user_data2 #} ptr
    user_data3 <- {# get TreeIter->user_data3 #} ptr
    return (TreeIter stamp (ptrToWord user_data)
                           (ptrToWord user_data2)
                           (ptrToWord user_data3))

    where ptrToWord :: Ptr a -> Word32
          ptrToWord ptr = fromIntegral (ptr `minusPtr` nullPtr)

  poke ptr (TreeIter stamp user_data user_data2 user_data3) = do
    {# set TreeIter->stamp      #} ptr stamp
    {# set TreeIter->user_data  #} ptr (wordToPtr user_data)
    {# set TreeIter->user_data2 #} ptr (wordToPtr user_data2)
    {# set TreeIter->user_data3 #} ptr (wordToPtr user_data3)

    where wordToPtr :: Word32 -> Ptr a
          wordToPtr word = nullPtr `plusPtr` fromIntegral word

-- Pass a pointer to a structure large enough to hold a GtkTreeIter
-- structure. If the function returns true, read the tree iter and
-- return it.
receiveTreeIter :: (Ptr TreeIter -> IO CInt) -> IO (Maybe TreeIter)
receiveTreeIter body =
  alloca $ \iterPtr -> do
  result <- body iterPtr
  if toBool result
    then liftM Just (peek iterPtr)
    else return Nothing

-- Note that this function does throw an error if the pointer is NULL rather
-- than returning some random tree iterator.
peekTreeIter :: Ptr TreeIter -> IO TreeIter
peekTreeIter ptr
  | ptr==nullPtr = fail "peekTreeIter: ptr is NULL, tree iterator is invalid"
  | otherwise = peek ptr

-- update the stamp of a tree iter
treeIterSetStamp :: TreeIter -> CInt -> TreeIter
treeIterSetStamp (TreeIter _ a b c) s = (TreeIter s a b c)

-- | TreePath : a list of indices to specify a subtree or node in a
-- 'Graphics.UI.Gtk.ModelView.TreeModel.TreeModel'. The node that correspond
-- to a given 'TreePath' might change if nodes are removed or added and a
-- 'TreePath' may refer to a different or even non-existent node after a
-- modification of the model. In contrast, a 'TreeIter' is a more compact
-- representation of a 'TreePath' which becomes invalid after each
-- modification of the underlying model. An intelligent index that is adjusted
-- with each update of the model to point to the same node (whenever possible)
-- is 'Graphics.UI.Gtk.ModelView.TreeRowReference.TreeRowReference'.
--
type TreePath = [Int]

{#pointer * TreePath as NativeTreePath newtype#}

nativeTreePathFree :: NativeTreePath -> IO ()
nativeTreePathFree =
  {# call unsafe tree_path_free #}

newTreePath :: TreePath -> IO NativeTreePath
newTreePath path = do
  nativePath <- liftM NativeTreePath {# call unsafe tree_path_new #}
  mapM_ ({#call unsafe tree_path_append_index#} nativePath . fromIntegral) path
  return nativePath

withTreePath :: TreePath -> (NativeTreePath -> IO a) -> IO a
withTreePath tp act = do
  nativePath <- newTreePath tp
  res <- act nativePath
  nativeTreePathFree nativePath
  return res

maybeWithTreePath :: Maybe TreePath -> (NativeTreePath -> IO a) -> IO a
maybeWithTreePath mbTp act = maybe (act (NativeTreePath nullPtr)) (`withTreePath` act) mbTp

nativeTreePathGetIndices :: NativeTreePath -> IO [Int]
nativeTreePathGetIndices tp = do
  depth <- liftM fromIntegral $ {# call unsafe tree_path_get_depth #} tp
  arrayPtr <- {# call unsafe tree_path_get_indices #} tp
  if (depth==0 || arrayPtr==nullPtr)
    then return []
    else liftM (map fromIntegral) $ peekArray depth arrayPtr

-- | Convert the given pointer to a tree path.
peekTreePath :: Ptr NativeTreePath -> IO TreePath
peekTreePath tpPtr | tpPtr==nullPtr = return []
                   | otherwise =
  nativeTreePathGetIndices (NativeTreePath tpPtr)

-- | Convert the given pointer to a tree path. Frees the pointer.
fromTreePath :: Ptr NativeTreePath -> IO TreePath
fromTreePath tpPtr | tpPtr==nullPtr = return []
                   | otherwise = do
  path <- nativeTreePathGetIndices (NativeTreePath tpPtr)
  nativeTreePathFree (NativeTreePath tpPtr)
  return path

-- | Convert a comma or colon separated string into a 'TreePath'. Any
-- non-digit characters are assumed to separate indices, thus, the function
-- always is always successful.
stringToTreePath :: DefaultGlibString -> TreePath
stringToTreePath = stringToTreePath' . glibToString
  where
  stringToTreePath' "" = []
  stringToTreePath' path = getNum 0 (dropWhile (not . isDigit) path)
  getNum acc ('0':xs) = getNum (10*acc) xs
  getNum acc ('1':xs) = getNum (10*acc+1) xs
  getNum acc ('2':xs) = getNum (10*acc+2) xs
  getNum acc ('3':xs) = getNum (10*acc+3) xs
  getNum acc ('4':xs) = getNum (10*acc+4) xs
  getNum acc ('5':xs) = getNum (10*acc+5) xs
  getNum acc ('6':xs) = getNum (10*acc+6) xs
  getNum acc ('7':xs) = getNum (10*acc+7) xs
  getNum acc ('8':xs) = getNum (10*acc+8) xs
  getNum acc ('9':xs) = getNum (10*acc+9) xs
  getNum acc xs = acc:stringToTreePath' (dropWhile (not . isDigit) xs)

-- | Accessing a row for a specific value. Used for 'ColumnMap'.
data ColumnAccess row where
  CAInvalid :: ColumnAccess row
  CAInt     :: (row -> Int) -> ColumnAccess row
  CABool    :: (row -> Bool) -> ColumnAccess row
  CAString  :: GlibString string => (row -> string) -> ColumnAccess row
  CAPixbuf  :: (row -> Pixbuf) -> ColumnAccess row

-- | The type of a tree column.
data ColumnId row ty
  = ColumnId (GValue -> IO ty) ((row -> ty) -> ColumnAccess row) Int

-- it shouldn't matter if the following function is actually inlined
{-# NOINLINE comboQuark #-}
comboQuark :: Quark
comboQuark =
  unsafePerformIO $ quarkFromString ("comboBoxHaskellStringModelQuark"::DefaultGlibString)

