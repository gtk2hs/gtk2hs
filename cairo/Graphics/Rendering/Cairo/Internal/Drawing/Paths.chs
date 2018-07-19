-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Cairo.Internal.Drawing.Paths
-- Copyright   :  (c) Paolo Martini 2005
-- License     :  BSD-style (see cairo/COPYRIGHT)
--
-- Maintainer  :  p.martini@neuralnoise.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creating paths and manipulating path data.
-----------------------------------------------------------------------------

module Graphics.Rendering.Cairo.Internal.Drawing.Paths where

{#import Graphics.Rendering.Cairo.Types#}

import Foreign
import Foreign.C
import Foreign.Marshal.Alloc (mallocBytes,finalizerFree)

import Graphics.Rendering.Cairo.Internal.Utilities (CairoString(..))

{#context lib="cairo" prefix="cairo"#}

{#pointer *path_t as CPath newtype#}
unPath :: CPath -> Ptr CPath
unPath (CPath p) = p


{#fun get_current_point as getCurrentPoint { unCairo `Cairo', alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv* } -> `()'#}
{#fun new_path          as newPath         { unCairo `Cairo' } -> `()'#}
{#fun close_path        as closePath       { unCairo `Cairo' } -> `()'#}
{#fun arc               as arc             { unCairo `Cairo', `Double', `Double', `Double', `Double', `Double' } -> `()'#}
{#fun arc_negative      as arcNegative     { unCairo `Cairo', `Double', `Double', `Double', `Double', `Double' } -> `()'#}
{#fun curve_to          as curveTo         { unCairo `Cairo', `Double', `Double', `Double', `Double', `Double', `Double' } -> `()'#}
{#fun line_to           as lineTo          { unCairo `Cairo', `Double', `Double' } -> `()'#}
{#fun move_to           as moveTo          { unCairo `Cairo', `Double', `Double' } -> `()'#}
{#fun rectangle         as rectangle       { unCairo `Cairo', `Double', `Double', `Double', `Double' } -> `()'#}
textPath :: CairoString string => Cairo -> string -> IO ()
textPath c string =
    withUTFString string $ \string' ->
    {# call text_path #}
        c string'
{#fun rel_curve_to      as relCurveTo      { unCairo `Cairo', `Double', `Double', `Double', `Double', `Double', `Double' } -> `()'#}
{#fun rel_line_to       as relLineTo       { unCairo `Cairo', `Double', `Double' } -> `()'#}
{#fun rel_move_to       as relMoveTo       { unCairo `Cairo', `Double', `Double' } -> `()'#}
{#fun copy_path         as copyPathC       { unCairo `Cairo' } -> `CPath' CPath #}
{#fun copy_path_flat    as copyPathFlatC   { unCairo `Cairo' } -> `CPath' CPath #}
{#fun append_path       as appendPathC     { unCairo `Cairo', unPath `CPath' } -> `()' #}
{#fun path_destroy      as pathDestroy     { unPath `CPath' } -> `()' #}


{#enum path_data_type_t as PathDataRecordType {underscoreToCase} deriving(Eq,Show)#}

data PathDataRecord
 = PathHeaderRecord PathDataRecordType Int
 | PathPointRecord Double Double
 deriving (Eq,Show)


copyPath :: Cairo -> IO [PathElement]
copyPath ctx = do
   p <- copyPathC ctx
   xs <- pathToList p
   pathDestroy p
   return xs


copyPathFlat :: Cairo -> IO [PathElement]
copyPathFlat ctx = do
   p <- copyPathFlatC ctx
   xs <- pathToList p
   pathDestroy p
   return xs


appendPath :: Cairo -> [PathElement] -> IO ()
appendPath ctx es = do
   path <- mkPathPtr es
   appendPathC ctx path
   deallocPath path


pathToList :: CPath -> IO [PathElement]
pathToList p  =  pathToList' <$> pathToList'' p



pathToList' :: [PathDataRecord] -> [PathElement]
pathToList' [] = []
pathToList' ((PathHeaderRecord htype hlen):rs)
   | hlen >= 1 = let (mine,rest) = splitAt (hlen-1) rs
                 in  (consElem htype mine) : pathToList' rest
   | otherwise = error "invalid path data (invalid header length)"
pathToList' _ = error "invalid path data (expected header record)"



pathToList'' :: CPath -> IO [PathDataRecord]
pathToList'' (CPath p) = do
      numdata <- {#get path_t->num_data #} p
      dptr    <- {#get path_t->data#} p
      getPathData 0 (cIntConv numdata) (castPtr dptr)

  where  size = {#sizeof path_data_t#}
         getPathData :: Int -> Int -> Ptr PathDataRecord -> IO [PathDataRecord]
         getPathData currpos numdata dptr
            | currpos < numdata = do
               let dptr' = dptr `plusPtr` (size*currpos)
               h@(PathHeaderRecord _ hlen) <- peekHeader dptr'
               ds <- peekPoints dptr' hlen
               rest <- getPathData (currpos+hlen) numdata dptr
               return$ h:(ds++rest)
            | otherwise = return []

         peekHeader :: Ptr PathDataRecord -> IO PathDataRecord
         peekHeader p = do
            -- the more intuitive statement
            --     htype <- {#get path_data_t->header.type #} p
            -- generates an error
            -- "CHS module contains errors: The phrase `type' is not allowed here."
            htype <- peekByteOff p 0 :: IO CInt
            hlen <- {#get path_data_t->header.length #} p
            return$ PathHeaderRecord (cToEnum htype) (cIntConv hlen)

         peekPoint :: Ptr PathDataRecord -> IO PathDataRecord
         peekPoint p = do
            x <- {#get path_data_t->point.x #} p
            y <- {#get path_data_t->point.y #} p
            return$ PathPointRecord (cFloatConv x) (cFloatConv y)

         peekPoints :: Ptr PathDataRecord -> Int -> IO [PathDataRecord]
         peekPoints p n = mapM (\i -> peekPoint (p `plusPtr` (size*i))) [1..(n-1)]



getPts = \(PathPointRecord x y) -> (x,y)


pokeRecord :: Ptr PathDataRecord -> PathDataRecord -> IO ()
pokeRecord ptr (PathHeaderRecord htype hlen) = do
   pokeByteOff ptr 0 (cFromEnum htype :: CInt)  -- the member named 'type' of the header is misunderstood by c2hs (see above)
   {#set path_data_t->header.length #} ptr (cIntConv hlen)

pokeRecord ptr (PathPointRecord x y) = do
   {#set path_data_t->point.x #} ptr (cFloatConv x)
   {#set path_data_t->point.y #} ptr (cFloatConv y)





consElem :: PathDataRecordType -> [PathDataRecord] -> PathElement
consElem PathMoveTo ps
   | length ps < 1   = error "invalid path data (not enough points)"
   | otherwise       = uncurry MoveTo $ getPts (ps!!0)
consElem PathLineTo ps
   | length ps < 1   = error "invalid path data (not enough points)"
   | otherwise       = uncurry LineTo $ getPts (ps!!0)
consElem PathCurveTo ps
   | length ps < 3   = error "invalid path data (not enough points)"
   | otherwise       = let ps' = map getPts (take 3 ps)
                       in uncurry (uncurry (uncurry CurveTo (ps'!!0)) (ps'!!1)) (ps'!!2)
consElem PathClosePath ps = ClosePath


consRecs :: PathElement -> [PathDataRecord]
consRecs (MoveTo x y) =
   [ PathHeaderRecord PathMoveTo 2, PathPointRecord x y]
consRecs (LineTo x y) =
   [ PathHeaderRecord PathLineTo 2, PathPointRecord x y]
consRecs (CurveTo x₀ y₀ x₁ y₁ x₂ y₂) =
   [ PathHeaderRecord PathCurveTo 4
   , PathPointRecord x₀ y₀
   , PathPointRecord x₁ y₁
   , PathPointRecord x₂ y₂
   ]
consRecs ClosePath = [PathHeaderRecord PathClosePath 1]



mkPathPtr :: [PathElement] -> IO CPath
mkPathPtr es = do
   (dptr,numdata) <- mkDataPtr es
   ptr <- mallocBytes {#sizeof path_t#}
   {#set path_t->status #} ptr (cFromEnum StatusSuccess)
   {#set path_t->data #} ptr (castPtr dptr)
   {#set path_t->num_data #} ptr (cIntConv numdata)
   return (CPath ptr)



mkDataPtr :: [PathElement] -> IO (Ptr PathDataRecord, Int)
mkDataPtr es = do
   let rs = concatMap consRecs es
       len  = length rs
       size = {#sizeof path_data_t#}
   dptr <- mallocBytes (len*size) :: IO (Ptr PathDataRecord)
   mapM_ (\(r,i) -> pokeRecord (dptr `plusPtr` (i*size)) r) (zip rs [0..])
   return (dptr,len)


deallocPath :: CPath -> IO ()
deallocPath (CPath ptr) = do
   dptr <- {#get path_t->data#} ptr
   free dptr
   free ptr

