{-# OPTIONS -cpp -fglasgow-exts #-}
--
-- (c) The University of Glasgow 2002
--
-- Unboxed mutable Ints

module FastMutInt(
	FastMutInt, newFastMutInt,
	readFastMutInt, writeFastMutInt
  ) where

#define SIZEOF_HSINT  4

import GHC.Base
import GHC.IOBase

data FastMutInt = FastMutInt (MutableByteArray# RealWorld)

newFastMutInt :: IO FastMutInt
newFastMutInt = IO $ \s ->
  case newByteArray# size s of { (# s, arr #) ->
  (# s, FastMutInt arr #) }
  where I# size = SIZEOF_HSINT

readFastMutInt :: FastMutInt -> IO Int
readFastMutInt (FastMutInt arr) = IO $ \s ->
  case readIntArray# arr 0# s of { (# s, i #) ->
  (# s, I# i #) }

writeFastMutInt :: FastMutInt -> Int -> IO ()
writeFastMutInt (FastMutInt arr) (I# i) = IO $ \s ->
  case writeIntArray# arr 0# i s of { s ->
  (# s, () #) }

