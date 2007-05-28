-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Cairo.Internal.Surfaces.Image
-- Copyright   :  (c) Paolo Martini 2005
-- License     :  BSD-style (see doc/COPYRIGHT)
--
-- Maintainer  :  p.martini@neuralnoise.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Rendering to memory buffers.
-----------------------------------------------------------------------------

module Graphics.Rendering.Cairo.Internal.Surfaces.Image where

{#import Graphics.Rendering.Cairo.Types#}

import Foreign
import Foreign.C

{#context lib="cairo" prefix="cairo"#}

{#fun image_surface_create     as imageSurfaceCreate    { cFromEnum `Format', `Int', `Int' } -> `Surface' mkSurface*#}
{#fun image_surface_get_width  as imageSurfaceGetWidth  { withSurface* `Surface' } -> `Int'#}
{#fun image_surface_get_height as imageSurfaceGetHeight { withSurface* `Surface' } -> `Int'#}

