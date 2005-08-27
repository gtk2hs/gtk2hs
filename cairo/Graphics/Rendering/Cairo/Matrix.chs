-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Cairo.Matrix
-- Copyright   :  (c) Paolo Martini 2005
-- License     :  BSD-style (see cairo/COPYRIGHT)
--
-- Maintainer  :  p.martini@neuralnoise.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Matrix math
-----------------------------------------------------------------------------

module Graphics.Rendering.Cairo.Matrix (
    Matrix(Matrix)
  , MatrixPtr
  , identity
  , translate
  , scale
  , rotate
  , transformDistance
  , transformPoint
  , scalarMultiply
  , adjoint
  , invert
  ) where

import Foreign hiding (rotate)
import CForeign

data Matrix = Matrix { xx :: Double, yx :: Double,
                       xy :: Double, yy :: Double,
                       x0 :: Double, y0 :: Double }
  deriving (Show, Eq)

{#pointer *cairo_matrix_t as MatrixPtr -> Matrix#}

instance Storable Matrix where
  sizeOf _ = {#sizeof cairo_matrix_t#}
  alignment _ = alignment (undefined :: CDouble)
  peek p = do
    xx <- {#get cairo_matrix_t->xx#} p
    yx <- {#get cairo_matrix_t->yx#} p
    xy <- {#get cairo_matrix_t->xy#} p
    yy <- {#get cairo_matrix_t->yy#} p
    x0 <- {#get cairo_matrix_t->x0#} p
    y0 <- {#get cairo_matrix_t->y0#} p
    return $ Matrix (realToFrac xx) (realToFrac yx)
                    (realToFrac xy) (realToFrac yy)
                    (realToFrac x0) (realToFrac y0)

  poke p (Matrix xx yx xy yy x0 y0) = do
    {#set cairo_matrix_t->xx#} p (realToFrac xx)
    {#set cairo_matrix_t->yx#} p (realToFrac yx)
    {#set cairo_matrix_t->xy#} p (realToFrac xy)
    {#set cairo_matrix_t->yy#} p (realToFrac yy)
    {#set cairo_matrix_t->x0#} p (realToFrac x0)
    {#set cairo_matrix_t->y0#} p (realToFrac y0)
    return ()

instance Num Matrix where
  (*) (Matrix xx yx xy yy x0 y0) (Matrix xx' yx' xy' yy' x0' y0') =
    Matrix (xx * xx' + yx * yx')
           (xx * yx' + yx * yy')
           (xy * xx' + yy * xy')
           (xy * yx' + yy * yy')
           (x0 * xx' + y0 * xy' + x0')
           (x0 * yx' + y0 * yy' + y0')

identity :: Matrix
identity = Matrix 1 0 0 1 0 0

translate :: Double -> Double -> Matrix -> Matrix
translate tx ty m = m * (Matrix 1 0 0 1 tx ty)

scale :: Double -> Double -> Matrix -> Matrix
scale sx sy m = m * (Matrix sx 0 0 sy 0 0)

rotate :: Double -> Matrix -> Matrix
rotate r m = m * (Matrix c s (-s) c 0 0)
  where s = sin r
        c = cos r

transformDistance :: Matrix -> (Double,Double) -> (Double,Double)
transformDistance (Matrix xx xy yx yy _ _) (dx,dy) = newX `seq` newY `seq` (newX,newY)
  where newX = xx * dx + xy * dy
        newY = yx * dx + yy * dy

transformPoint :: Matrix -> (Double,Double) -> (Double,Double)
transformPoint (Matrix xx xy yx yy x0 y0) (dx,dy) = newX `seq` newY `seq` (newX,newY)
  where newX = xx * dx + xy * dy + x0
        newY = yx * dx + yy * dy + y0

scalarMultiply :: Double -> Matrix -> Matrix
scalarMultiply scalar (Matrix xx yx xy yy x0 y0) =
  Matrix (xx * scalar)
         (yx * scalar)
         (xy * scalar)
         (yy * scalar)
         (x0 * scalar)
         (y0 * scalar)

adjoint :: Matrix -> Matrix
adjoint (Matrix a b c d tx ty) = Matrix d (-b) (-c) a (c*ty - d*tx) (b*tx - a*ty)

invert :: Matrix -> Matrix
invert m@(Matrix xx yx xy yy _ _) = scalarMultiply det $ adjoint m
  where det = xx*yx - xy*yy
