-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Cairo.Internal.Surfaces.PNG
-- Copyright   :  (c) Paolo Martini 2005
-- License     :  BSD-style (see doc/COPYRIGHT)
--
-- Maintainer  :  p.martini@neuralnoise.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Reading and writing PNG images.
-----------------------------------------------------------------------------

module Graphics.Rendering.Cairo.Internal.Surfaces.PNG where

{#import Graphics.Rendering.Cairo.Types#}

import Foreign
import CForeign

import Monad (liftM)

{#context lib="cairo" prefix="cairo"#}

imageSurfaceCreateFromPNG :: FilePath -> IO Surface
imageSurfaceCreateFromPNG filename =
  liftM Surface $
  withCString filename $ \filenamePtr ->
  {#call unsafe image_surface_create_from_png#} filenamePtr

surfaceWriteToPNG :: Surface -> FilePath -> IO Status
surfaceWriteToPNG surface filename = withCString filename $ \f ->
  throwErrnoIf (/= StatusSuccess) "surfaceWriteToPNG" $
    {#call unsafe surface_write_to_png#} surface f >>= return . cToEnum
