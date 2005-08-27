-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Cairo.PDF
-- Copyright   :  (c) Paolo Martini 2005
-- License     :  BSD-style (see cairo/COPYRIGHT)
--
-- Maintainer  :  p.martini@neuralnoise.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Rendering PDF documents.
-----------------------------------------------------------------------------

module Graphics.Rendering.Cairo.PDF where

import Graphics.Rendering.Cairo.Types
import qualified Graphics.Rendering.Cairo.Internal.Surfaces.PDF as PDF
import Graphics.Rendering.Cairo.Internal.Surfaces.Surface (surfaceDestroy)

withPDFSurface :: FilePath -> Double -> Double -> (Surface -> IO a) -> IO a
withPDFSurface filename width height = do
  surface <- PDF.pdfSurfaceCreate filename width height
  ret <- f surface
  surfaceDestroy surface
  return ret

pdfSurfaceSetDPI :: Surface -> Double -> Double -> IO a
pdfSurfaceSetDPI surface x y = PDF.pdfSurfaceSetDPI surface x y
