{-# LANGUAGE CPP, ScopedTypeVariables #-}
--
-- (c) The University of Glasgow 2002
--
-- Binary I/O library, with special tweaks for GHC
--
-- Based on the nhc98 Binary library, which is copyright
-- (c) Malcolm Wallace and Colin Runciman, University of York, 1998.
-- Under the terms of the license for that software, we must tell you
-- where you can obtain the original version of the Binary library, namely
--     http://www.cs.york.ac.uk/fp/nhc98/

module Binary
  ( {-type-}  Bin,
    {-class-} Binary(..),
    {-type-}  BinHandle,

   openBinIO, openBinIO_,
   openBinMem,
--   closeBin,

   seekBin,
   tellBin,
   castBin,

   writeBinMem,
   readBinMem,

   isEOFBin,

   -- for writing instances:
   putByte,
   getByte,
   putSharedString,
   getSharedString,

   -- lazy Bin I/O
   lazyGet,
   lazyPut,

#if __GLASGOW_HASKELL__<610
   -- GHC only:
   ByteArray(..),
   getByteArray,
   putByteArray,
#endif

   getBinFileWithDict,  -- :: Binary a => FilePath -> IO a
   putBinFileWithDict,  -- :: Binary a => FilePath -> ModuleName -> a -> IO ()

  ) where

#if __GLASGOW_HASKELL__>=604
#include "ghcconfig.h"
#else
#include "config.h"
#endif

import FastMutInt

import Map (Map)
import qualified Map as Map
#if __GLASGOW_HASKELL__>=602
# if __GLASGOW_HASKELL__>=707
import Data.HashTable.Class as HashTable
              (HashTable)
import Data.HashTable.IO as HashTable
              (BasicHashTable, toList, new, insert, lookup)
# else
import Data.HashTable as HashTable
# endif
#endif
import Data.Array.IO
import Data.Array
import Data.Bits
import Data.Int
import Data.Word
import Data.IORef
import Data.Char                ( ord, chr )
import Data.Array.Base          ( unsafeRead, unsafeWrite )
import Control.Monad            ( when, liftM )
import System.IO as IO
import System.IO.Unsafe         ( unsafeInterleaveIO )
import System.IO.Error          ( mkIOError, eofErrorType )
import GHC.Real                 ( Ratio(..) )
import GHC.Exts
# if __GLASGOW_HASKELL__>=612
import GHC.IO     (IO(IO))
#else
import GHC.IOBase (IO(IO))
#endif
import GHC.Word                 ( Word8(..) )
# if __GLASGOW_HASKELL__<602
import GHC.Handle               ( hSetBinaryMode )
# endif
-- for debug
import System.CPUTime           (getCPUTime)
import Numeric                  (showFFloat)

#define SIZEOF_HSINT SIZEOF_VOID_P

type BinArray = IOUArray Int Word8

---------------------------------------------------------------
--              BinHandle
---------------------------------------------------------------

data BinHandle
  = BinMem {            -- binary data stored in an unboxed array
     bh_usr :: UserData,        -- sigh, need parameterized modules :-)
     off_r :: !FastMutInt,              -- the current offset
     sz_r  :: !FastMutInt,              -- size of the array (cached)
     arr_r :: !(IORef BinArray)         -- the array (bounds: (0,size-1))
    }
        -- XXX: should really store a "high water mark" for dumping out
        -- the binary data to a file.

  | BinIO {             -- binary data stored in a file
     bh_usr :: UserData,
     off_r :: !FastMutInt,              -- the current offset (cached)
     hdl   :: !IO.Handle                -- the file handle (must be seekable)
   }
        -- cache the file ptr in BinIO; using hTell is too expensive
        -- to call repeatedly.  If anyone else is modifying this Handle
        -- at the same time, we'll be screwed.

getUserData :: BinHandle -> UserData
getUserData bh = bh_usr bh

setUserData :: BinHandle -> UserData -> BinHandle
setUserData bh us = bh { bh_usr = us }


---------------------------------------------------------------
--              Bin
---------------------------------------------------------------

newtype Bin a = BinPtr Int
  deriving (Eq, Ord, Show, Bounded)

castBin :: Bin a -> Bin b
castBin (BinPtr i) = BinPtr i

---------------------------------------------------------------
--              class Binary
---------------------------------------------------------------

class Binary a where
    put_   :: BinHandle -> a -> IO ()
    put    :: BinHandle -> a -> IO (Bin a)
    get    :: BinHandle -> IO a

    -- define one of put_, put.  Use of put_ is recommended because it
    -- is more likely that tail-calls can kick in, and we rarely need the
    -- position return value.
    put_ bh a = do put bh a; return ()
    put bh a  = do p <- tellBin bh; put_ bh a; return p

putAt  :: Binary a => BinHandle -> Bin a -> a -> IO ()
putAt bh p x = do seekBin bh p; put bh x; return ()

getAt  :: Binary a => BinHandle -> Bin a -> IO a
getAt bh p = do seekBin bh p; get bh

openBinIO_ :: IO.Handle -> IO BinHandle
openBinIO_ h = openBinIO h

openBinIO :: IO.Handle -> IO BinHandle
openBinIO h = do
  r <- newFastMutInt
  writeFastMutInt r 0
  return (BinIO noUserData r h)

openBinMem :: Int -> IO BinHandle
openBinMem size
 | size <= 0 = error "Data.Binary.openBinMem: size must be >= 0"
 | otherwise = do
   arr <- newArray_ (0,size-1)
   arr_r <- newIORef arr
   ix_r <- newFastMutInt
   writeFastMutInt ix_r 0
   sz_r <- newFastMutInt
   writeFastMutInt sz_r size
   return (BinMem noUserData ix_r sz_r arr_r)

tellBin :: BinHandle -> IO (Bin a)
tellBin (BinIO  _ r _)   = do ix <- readFastMutInt r; return (BinPtr ix)
tellBin (BinMem _ r _ _) = do ix <- readFastMutInt r; return (BinPtr ix)

seekBin :: BinHandle -> Bin a -> IO ()
seekBin (BinIO _ ix_r h) (BinPtr p) = do
  writeFastMutInt ix_r p
  hSeek h AbsoluteSeek (fromIntegral p)
seekBin h@(BinMem _ ix_r sz_r a) (BinPtr p) = do
  sz <- readFastMutInt sz_r
  if (p >= sz)
        then do expandBin h p; writeFastMutInt ix_r p
        else writeFastMutInt ix_r p

isEOFBin :: BinHandle -> IO Bool
isEOFBin (BinMem _ ix_r sz_r a) = do
  ix <- readFastMutInt ix_r
  sz <- readFastMutInt sz_r
  return (ix >= sz)
isEOFBin (BinIO _ ix_r h) = hIsEOF h

writeBinMem :: BinHandle -> FilePath -> IO ()
writeBinMem (BinIO _ _ _) _ = error "Data.Binary.writeBinMem: not a memory handle"
writeBinMem (BinMem _ ix_r sz_r arr_r) fn = do
  h <- openFile fn WriteMode
  hSetBinaryMode h True
  arr <- readIORef arr_r
  ix  <- readFastMutInt ix_r
  hPutArray h arr ix
  hClose h

readBinMem :: FilePath -> IO BinHandle
-- Return a BinHandle with a totally undefined State
readBinMem filename = do
  h <- openFile filename ReadMode
  hSetBinaryMode h True
  filesize' <- hFileSize h
  let filesize = fromIntegral filesize'
  arr <- newArray_ (0,filesize-1)
  count <- hGetArray h arr filesize
  when (count /= filesize)
        (error ("Binary.readBinMem: only read " ++ show count ++ " bytes"))
  hClose h
  arr_r <- newIORef arr
  ix_r <- newFastMutInt
  writeFastMutInt ix_r 0
  sz_r <- newFastMutInt
  writeFastMutInt sz_r filesize
  return (BinMem noUserData ix_r sz_r arr_r)

-- expand the size of the array to include a specified offset
expandBin :: BinHandle -> Int -> IO ()
expandBin (BinMem _ ix_r sz_r arr_r) off = do
   sz <- readFastMutInt sz_r
   let sz' = head (dropWhile (<= off) (iterate (* 2) sz))
   arr <- readIORef arr_r
   arr' <- newArray_ (0,sz'-1)
   sequence_ [ unsafeRead arr i >>= unsafeWrite arr' i
             | i <- [ 0 .. sz-1 ] ]
   writeFastMutInt sz_r sz'
   writeIORef arr_r arr'
#ifdef DEBUG
   hPutStrLn stderr ("Binary: expanding to size: " ++ show sz')
#endif
   return ()
expandBin (BinIO _ _ _) _ = return ()
        -- no need to expand a file, we'll assume they expand by themselves.
{-# INLINE expandBin #-}

-- -----------------------------------------------------------------------------
-- Low-level reading/writing of bytes

putWord8 :: BinHandle -> Word8 -> IO ()
putWord8 h@(BinMem _ ix_r sz_r arr_r) w = do
    ix <- readFastMutInt ix_r
    sz <- readFastMutInt sz_r
        -- double the size of the array if it overflows
    if (ix >= sz)
        then do expandBin h ix
                putWord8 h w
        else do arr <- readIORef arr_r
                unsafeWrite arr ix w
                writeFastMutInt ix_r (ix+1)
                return ()
putWord8 (BinIO _ ix_r h) w = do
    ix <- readFastMutInt ix_r
    hPutChar h (chr (fromIntegral w))   -- XXX not really correct
    writeFastMutInt ix_r (ix+1)
    return ()

getWord8 :: BinHandle -> IO Word8
getWord8 (BinMem _ ix_r sz_r arr_r) = do
    ix <- readFastMutInt ix_r
    sz <- readFastMutInt sz_r
    when (ix >= sz)  $
        ioError (mkIOError eofErrorType "Data.Binary.getWord8" Nothing Nothing)
    arr <- readIORef arr_r
    w <- unsafeRead arr ix
    writeFastMutInt ix_r (ix+1)
    return w
getWord8 (BinIO _ ix_r h) = do
    ix <- readFastMutInt ix_r
    c <- hGetChar h
    writeFastMutInt ix_r (ix+1)
    return $! (fromIntegral (ord c))    -- XXX not really correct

putByte :: BinHandle -> Word8 -> IO ()
putByte bh w = put_ bh w

getByte :: BinHandle -> IO Word8
getByte = getWord8

-- -----------------------------------------------------------------------------
-- Primitive Word writes

instance Binary Word8 where
  put_ = putWord8
  get  = getWord8

instance Binary Word16 where
  put_ h w = do -- XXX too slow.. inline putWord8?
    putByte h (fromIntegral (w `shiftR` 8))
    putByte h (fromIntegral (w .&. 0xff))
  get h = do
    w1 <- getWord8 h
    w2 <- getWord8 h
    return $! ((fromIntegral w1 `shiftL` 8) .|. fromIntegral w2)


instance Binary Word32 where
  put_ h w = do
    putByte h (fromIntegral (w `shiftR` 24))
    putByte h (fromIntegral ((w `shiftR` 16) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR` 8)  .&. 0xff))
    putByte h (fromIntegral (w .&. 0xff))
  get h = do
    w1 <- getWord8 h
    w2 <- getWord8 h
    w3 <- getWord8 h
    w4 <- getWord8 h
    return $! ((fromIntegral w1 `shiftL` 24) .|.
               (fromIntegral w2 `shiftL` 16) .|.
               (fromIntegral w3 `shiftL`  8) .|.
               (fromIntegral w4))


instance Binary Word64 where
  put_ h w = do
    putByte h (fromIntegral (w `shiftR` 56))
    putByte h (fromIntegral ((w `shiftR` 48) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR` 40) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR` 32) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR` 24) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR` 16) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR`  8) .&. 0xff))
    putByte h (fromIntegral (w .&. 0xff))
  get h = do
    w1 <- getWord8 h
    w2 <- getWord8 h
    w3 <- getWord8 h
    w4 <- getWord8 h
    w5 <- getWord8 h
    w6 <- getWord8 h
    w7 <- getWord8 h
    w8 <- getWord8 h
    return $! ((fromIntegral w1 `shiftL` 56) .|.
               (fromIntegral w2 `shiftL` 48) .|.
               (fromIntegral w3 `shiftL` 40) .|.
               (fromIntegral w4 `shiftL` 32) .|.
               (fromIntegral w5 `shiftL` 24) .|.
               (fromIntegral w6 `shiftL` 16) .|.
               (fromIntegral w7 `shiftL`  8) .|.
               (fromIntegral w8))

-- -----------------------------------------------------------------------------
-- Primitive Int writes

instance Binary Int8 where
  put_ h w = put_ h (fromIntegral w :: Word8)
  get h    = do w <- get h; return $! (fromIntegral (w::Word8))

instance Binary Int16 where
  put_ h w = put_ h (fromIntegral w :: Word16)
  get h    = do w <- get h; return $! (fromIntegral (w::Word16))

instance Binary Int32 where
  put_ h w = put_ h (fromIntegral w :: Word32)
  get h    = do w <- get h; return $! (fromIntegral (w::Word32))

instance Binary Int64 where
  put_ h w = put_ h (fromIntegral w :: Word64)
  get h    = do w <- get h; return $! (fromIntegral (w::Word64))

-- -----------------------------------------------------------------------------
-- Instances for standard types

instance Binary () where
    put_ bh () = return ()
    get  _     = return ()
--    getF bh p  = case getBitsF bh 0 p of (_,b) -> ((),b)

instance Binary Bool where
    put_ bh b = putByte bh (fromIntegral (fromEnum b))
    get  bh   = do x <- getWord8 bh; return $! (toEnum (fromIntegral x))
--    getF bh p = case getBitsF bh 1 p of (x,b) -> (toEnum x,b)

instance Binary Char where
    put_  bh c = put_ bh (fromIntegral (ord c) :: Word8)
    get  bh   = do x <- get bh; return $! (chr (fromIntegral (x :: Word8)))
--    getF bh p = case getBitsF bh 8 p of (x,b) -> (toEnum x,b)

instance Binary Int where
#if SIZEOF_HSINT == 4
    put_ bh i = put_ bh (fromIntegral i :: Int32)
    get  bh = do
        x <- get bh
        return $! (fromIntegral (x :: Int32))
#elif SIZEOF_HSINT == 8
    put_ bh i = put_ bh (fromIntegral i :: Int64)
    get  bh = do
        x <- get bh
        return $! (fromIntegral (x :: Int64))
#else
#error "unsupported sizeof(HsInt)"
#endif
--    getF bh   = getBitsF bh 32

instance Binary a => Binary [a] where
    put_ bh list = do put_ bh (length list)
                      mapM_ (put_ bh) list
    get bh = do len <- get bh
                let getMany :: Int -> IO [a]
                    getMany 0 = return []
                    getMany n = do x <- get bh
                                   xs <- getMany (n-1)
                                   return (x:xs)
                getMany len

instance (Binary a, Binary b) => Binary (a,b) where
    put_ bh (a,b) = do put_ bh a; put_ bh b
    get bh        = do a <- get bh
                       b <- get bh
                       return (a,b)

instance (Binary a, Binary b, Binary c) => Binary (a,b,c) where
    put_ bh (a,b,c) = do put_ bh a; put_ bh b; put_ bh c
    get bh          = do a <- get bh
                         b <- get bh
                         c <- get bh
                         return (a,b,c)

instance (Binary a, Binary b, Binary c, Binary d) => Binary (a,b,c,d) where
    put_ bh (a,b,c,d) = do put_ bh a; put_ bh b; put_ bh c; put_ bh d
    get bh          = do a <- get bh
                         b <- get bh
                         c <- get bh
                         d <- get bh
                         return (a,b,c,d)

instance Binary a => Binary (Maybe a) where
    put_ bh Nothing  = putByte bh 0
    put_ bh (Just a) = do putByte bh 1; put_ bh a
    get bh           = do h <- getWord8 bh
                          case h of
                            0 -> return Nothing
                            _ -> do x <- get bh; return (Just x)

instance (Binary a, Binary b) => Binary (Either a b) where
    put_ bh (Left  a) = do putByte bh 0; put_ bh a
    put_ bh (Right b) = do putByte bh 1; put_ bh b
    get bh            = do h <- getWord8 bh
                           case h of
                             0 -> do a <- get bh ; return (Left a)
                             _ -> do b <- get bh ; return (Right b)

instance (Binary a, Binary i, Ix i) => Binary (Array i a) where
  put_ bh arr = do put_ bh (Data.Array.bounds arr)
                   put_ bh (Data.Array.elems arr)
  get bh = do bounds <- get bh
              elems <- get bh
              return $ listArray bounds elems

instance (Binary key, Ord key, Binary elem) => Binary (Map key elem) where
--    put_ bh fm = put_ bh (Map.toList fm)
--    get bh = do list <- get bh
--                return (Map.fromList list)

    put_ bh fm = do let list = Map.toList fm
                    put_ bh (length list)
                    mapM_ (\(key, val) -> do put_ bh key
                                             lazyPut bh val) list
    get bh = do len <- get bh
                let getMany :: Int -> IO [(key,elem)]
                    getMany 0 = return []
                    getMany n = do key <- get bh
                                   val <- lazyGet bh
                                   xs <- getMany (n-1)
                                   return ((key,val):xs)
--                printElapsedTime "before get Map"
                list <- getMany len
--                printElapsedTime "after get Map"
                return (Map.fromList list)

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__<610
instance Binary Integer where
    put_ bh (S# i#) = do putByte bh 0; put_ bh (I# i#)
    put_ bh (J# s# a#) = do
        p <- putByte bh 1;
        put_ bh (I# s#)
        let sz# = sizeofByteArray# a#  -- in *bytes*
        put_ bh (I# sz#)  -- in *bytes*
        putByteArray bh a# sz#

    get bh = do
        b <- getByte bh
        case b of
          0 -> do (I# i#) <- get bh
                  return (S# i#)
          _ -> do (I# s#) <- get bh
                  sz <- get bh
                  (BA a#) <- getByteArray bh sz
                  return (J# s# a#)

putByteArray :: BinHandle -> ByteArray# -> Int# -> IO ()
putByteArray bh a s# = loop 0#
  where loop n#
           | n# ==# s# = return ()
           | otherwise = do
                putByte bh (indexByteArray a n#)
                loop (n# +# 1#)

getByteArray :: BinHandle -> Int -> IO ByteArray
getByteArray bh (I# sz) = do
  (MBA arr) <- newByteArray sz
  let loop n
           | n ==# sz = return ()
           | otherwise = do
                w <- getByte bh
                writeByteArray arr n w
                loop (n +# 1#)
  loop 0#
  freezeByteArray arr


data ByteArray = BA ByteArray#
data MBA = MBA (MutableByteArray# RealWorld)

newByteArray :: Int# -> IO MBA
newByteArray sz = IO $ \s ->
  case newByteArray# sz s of { (# s, arr #) ->
  (# s, MBA arr #) }

freezeByteArray :: MutableByteArray# RealWorld -> IO ByteArray
freezeByteArray arr = IO $ \s ->
  case unsafeFreezeByteArray# arr s of { (# s, arr #) ->
  (# s, BA arr #) }

writeByteArray :: MutableByteArray# RealWorld -> Int# -> Word8 -> IO ()

#if __GLASGOW_HASKELL__ < 503
writeByteArray arr i w8 = IO $ \s ->
  case word8ToWord w8 of { W# w# ->
  case writeCharArray# arr i (chr# (word2Int# w#)) s  of { s ->
  (# s , () #) }}
#else
writeByteArray arr i (W8# w) = IO $ \s ->
  case writeWord8Array# arr i w s of { s ->
  (# s, () #) }
#endif

#if __GLASGOW_HASKELL__ < 503
indexByteArray a# n# = fromIntegral (I# (ord# (indexCharArray# a# n#)))
#else
indexByteArray a# n# = W8# (indexWord8Array# a# n#)
#endif

instance (Integral a, Binary a) => Binary (Ratio a) where
    put_ bh (a :% b) = do put_ bh a; put_ bh b
    get bh = do a <- get bh; b <- get bh; return (a :% b)

#else

instance Binary Integer where
    put_ h n = do
      put h ((fromIntegral $ signum n) :: Int8)
      when (n /= 0) $ do
        let n' = abs n
            nBytes = byteSize n'
        put h (fromIntegral nBytes :: Word64)
        mapM_ (putByte h) [ fromIntegral ((n' `shiftR` (b * 8)) .&. 0xff)
                          | b <- [ nBytes-1, nBytes-2 .. 0 ] ]
      where byteSize n =
                let f b = if (1 `shiftL` (b * 8)) > n
                             then b
                             else f (b + 1)
                in f 0
    get h = do
      sign :: Int8 <- get h
      if sign == 0
         then return 0
         else do
           nBytes :: Word64 <- get h
           n <- accumBytes nBytes 0
           return $ fromIntegral sign * n
      where accumBytes nBytes acc | nBytes == 0 = return acc
                                  | otherwise = do
                b <- getByte h
                accumBytes (nBytes - 1) ((acc `shiftL` 8) .|. fromIntegral b)
#endif

#endif

instance Binary (Bin a) where
  put_ bh (BinPtr i) = put_ bh i
  get bh = do i <- get bh; return (BinPtr i)

-- -----------------------------------------------------------------------------
-- Lazy reading/writing

lazyPut :: Binary a => BinHandle -> a -> IO ()
lazyPut bh a = do
        -- output the obj with a ptr to skip over it:
    pre_a <- tellBin bh
    put_ bh pre_a       -- save a slot for the ptr
    put_ bh a           -- dump the object
    q <- tellBin bh     -- q = ptr to after object
    putAt bh pre_a q    -- fill in slot before a with ptr to q
    seekBin bh q        -- finally carry on writing at q

lazyGet :: Binary a => BinHandle -> IO a
lazyGet bh = do
    p <- get bh         -- a BinPtr
    p_a <- tellBin bh
    a <- unsafeInterleaveIO (getAt bh p_a)
    seekBin bh p -- skip over the object for now
    return a

-- --------------------------------------------------------------
--      Main wrappers: getBinFileWithDict, putBinFileWithDict
--
--      This layer is built on top of the stuff above,
--      and should not know anything about BinHandles
-- --------------------------------------------------------------

initBinMemSize       = (1024*1024) :: Int
binaryInterfaceMagic = 0x1face :: Word32

getBinFileWithDict :: Binary a => FilePath -> IO a
getBinFileWithDict file_path = do
  bh <- Binary.readBinMem file_path

        -- Read the magic number to check that this really is a GHC .hi file
        -- (This magic number does not change when we change
        --  GHC interface file format)
  magic <- get bh
  when (magic /= binaryInterfaceMagic) $
        error "magic number mismatch: old/corrupt interface file?"

        -- Read the dictionary
        -- The next word in the file is a pointer to where the dictionary is
        -- (probably at the end of the file)
  dict_p <- Binary.get bh       -- Get the dictionary ptr
  data_p <- tellBin bh          -- Remember where we are now
  seekBin bh dict_p
  dict <- getDictionary bh
  seekBin bh data_p             -- Back to where we were before

        -- Initialise the user-data field of bh
  let bh' = setUserData bh (initReadState dict)
        
        -- At last, get the thing
  get bh'

putBinFileWithDict :: Binary a => FilePath -> a -> IO ()
putBinFileWithDict file_path the_thing = do
--  hnd <- openBinaryFile file_path WriteMode
--  bh <- openBinIO hnd
  bh <- openBinMem initBinMemSize
  put_ bh binaryInterfaceMagic

        -- Remember where the dictionary pointer will go
  dict_p_p <- tellBin bh
  put_ bh dict_p_p      -- Placeholder for ptr to dictionary

        -- Make some initial state
  usr_state <- newWriteState

        -- Put the main thing,
  put_ (setUserData bh usr_state) the_thing

        -- Get the final-state
  j <- readIORef  (ud_next usr_state)
#if __GLASGOW_HASKELL__>=602
  fm <- HashTable.toList (ud_map  usr_state)
#else
  fm <- liftM Map.toList $ readIORef (ud_map  usr_state)
#endif
  dict_p <- tellBin bh  -- This is where the dictionary will start

        -- Write the dictionary pointer at the fornt of the file
  putAt bh dict_p_p dict_p      -- Fill in the placeholder
  seekBin bh dict_p             -- Seek back to the end of the file

        -- Write the dictionary itself
  putDictionary bh j (constructDictionary j fm)

        -- And send the result to the file
  writeBinMem bh file_path
--  hClose hnd

-- -----------------------------------------------------------------------------
-- UserData
-- -----------------------------------------------------------------------------

data UserData =
   UserData {   -- This field is used only when reading
              ud_dict :: Dictionary,

                -- The next two fields are only used when writing
              ud_next :: IORef Int,     -- The next index to use
#if __GLASGOW_HASKELL__>=602
# if __GLASGOW_HASKELL__>=707
              ud_map  :: BasicHashTable String Int -- The index of each string
# else
              ud_map  :: HashTable String Int -- The index of each string
# endif
#else
              ud_map  :: IORef (Map String Int)
#endif
        }

noUserData = error "Binary.UserData: no user data"

initReadState :: Dictionary -> UserData
initReadState dict = UserData{ ud_dict = dict,
                               ud_next = undef "next",
                               ud_map  = undef "map" }

newWriteState :: IO UserData
newWriteState = do
  j_r <- newIORef 0
#if __GLASGOW_HASKELL__>=602
# if __GLASGOW_HASKELL__>=707
  out_r <- HashTable.new
# else
  out_r <- HashTable.new (==) HashTable.hashString
# endif
#else
  out_r <- newIORef Map.empty
#endif
  return (UserData { ud_dict = error "dict",
                     ud_next = j_r,
                     ud_map  = out_r })


undef s = error ("Binary.UserData: no " ++ s)

---------------------------------------------------------
--              The Dictionary
---------------------------------------------------------

type Dictionary = Array Int String      -- The dictionary
                                        -- Should be 0-indexed

putDictionary :: BinHandle -> Int -> Dictionary -> IO ()
putDictionary bh sz dict = do
  put_ bh sz
  mapM_ (put_ bh) (elems dict)

getDictionary :: BinHandle -> IO Dictionary
getDictionary bh = do
  sz <- get bh
  elems <- sequence (take sz (repeat (get bh)))
  return (listArray (0,sz-1) elems)

constructDictionary :: Int -> [(String,Int)] -> Dictionary
constructDictionary j fm = array (0,j-1) (map (\(x,y) -> (y,x)) fm)

---------------------------------------------------------
--              Reading and writing memoised Strings
---------------------------------------------------------

putSharedString :: BinHandle -> String -> IO ()
putSharedString bh str =
  case getUserData bh of
    UserData { ud_next = j_r, ud_map = out_r, ud_dict = dict} -> do
#if __GLASGOW_HASKELL__>=602
      entry <- HashTable.lookup out_r str
#else
      fm <- readIORef out_r
      let entry = Map.lookup str fm
#endif
      case entry of
        Just j  -> put_ bh j
        Nothing -> do
                     j <- readIORef j_r
                     put_ bh j
                     writeIORef j_r (j+1)
#if __GLASGOW_HASKELL__>=602
                     HashTable.insert out_r str j
#else
                     modifyIORef out_r (\fm -> Map.insert str j fm)
#endif

getSharedString :: BinHandle -> IO String
getSharedString bh = do
        j <- get bh
        return $! (ud_dict (getUserData bh) ! j)

{-
---------------------------------------------------------
--              Reading and writing FastStrings
---------------------------------------------------------

putFS bh (FastString id l ba) = do
  put_ bh (I# l)
  putByteArray bh ba l
putFS bh s = error ("Binary.put_(FastString): " ++ unpackFS s)
        -- Note: the length of the FastString is *not* the same as
        -- the size of the ByteArray: the latter is rounded up to a
        -- multiple of the word size.

{- -- possible faster version, not quite there yet:
getFS bh@BinMem{} = do
  (I# l) <- get bh
  arr <- readIORef (arr_r bh)
  off <- readFastMutInt (off_r bh)
  return $! (mkFastSubStringBA# arr off l)
-}
getFS bh = do
  (I# l) <- get bh
  (BA ba) <- getByteArray bh (I# l)
  return $! (mkFastSubStringBA# ba 0# l)

instance Binary FastString where
  put_ bh f@(FastString id l ba) =
    case getUserData bh of {
        UserData { ud_next = j_r, ud_map = out_r, ud_dict = dict} -> do
    out <- readIORef out_r
    let uniq = getUnique f
    case lookupUFM out uniq of
        Just (j,f)  -> put_ bh j
        Nothing -> do
           j <- readIORef j_r
           put_ bh j
           writeIORef j_r (j+1)
           writeIORef out_r (addToUFM out uniq (j,f))
    }
  put_ bh s = error ("Binary.put_(FastString): " ++ show (unpackFS s))

  get bh = do
        j <- get bh
        return $! (ud_dict (getUserData bh) ! j)
-}

printElapsedTime :: String -> IO ()
printElapsedTime msg = do
  time <- getCPUTime
  hPutStr stderr $ "elapsed time: " ++ Numeric.showFFloat (Just 2) ((fromIntegral time) / 10^12) " (" ++ msg ++ ")\n"

