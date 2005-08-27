-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Cairo.PS
-- Copyright   :  (c) Paolo Martini 2005
-- License     :  BSD-style (see cairo/COPYRIGHT)
--
-- Maintainer  :  p.martini@neuralnoise.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Rendering PS documents.
-----------------------------------------------------------------------------

module Graphics.Rendering.Cairo.PS where

import Graphics.Rendering.Cairo.Types
import qualified Graphics.Rendering.Cairo.Internal.Surfaces.PS as PS
import Graphics.Rendering.Cairo.Internal.Surfaces.Surface (surfaceDestroy)

withPSSurface :: FilePath -> Double -> Double -> (Surface -> IO a) -> IO a
withPSSurface filename width height = do
  surface <- PS.psSurfaceCreate filename width height
  ret <- f surface
  surfaceDestroy surface
  return ret

psSurfaceSetDPI :: Surface -> Double -> Double -> IO a
psSurfaceSetDPI surface x y = PS.psSurfaceSetDPI surface x y
