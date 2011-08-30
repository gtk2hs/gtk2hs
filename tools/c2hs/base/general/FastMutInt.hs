{-# LANGUAGE CPP, BangPatterns, MagicHash, UnboxedTuples #-}
--
-- (c) The University of Glasgow 2002
--
-- Unboxed mutable Ints

module FastMutInt(
	FastMutInt, newFastMutInt,
	readFastMutInt, writeFastMutInt
  ) where

#define SIZEOF_HSINT  4

import GHC.Exts
# if __GLASGOW_HASKELL__>=612
import GHC.IO     (IO(IO))
#else
import GHC.IOBase (IO(IO))
#endif

data FastMutInt = FastMutInt (MutableByteArray# RealWorld)

newFastMutInt :: IO FastMutInt
newFastMutInt = IO $ \s ->
  case newByteArray# size s of { (# s, arr #) ->
  (# s, FastMutInt arr #) }
  where !(I# size) = SIZEOF_HSINT

readFastMutInt :: FastMutInt -> IO Int
readFastMutInt (FastMutInt arr) = IO $ \s ->
  case readIntArray# arr 0# s of { (# s, i #) ->
  (# s, I# i #) }

writeFastMutInt :: FastMutInt -> Int -> IO ()
writeFastMutInt (FastMutInt arr) (I# i) = IO $ \s ->
  case writeIntArray# arr 0# i s of { s ->
  (# s, () #) }

