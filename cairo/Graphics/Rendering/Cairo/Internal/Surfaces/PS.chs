-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Cairo.Internal.Surfaces.PS
-- Copyright   :  (c) Paolo Martini 2005
-- License     :  BSD-style (see doc/COPYRIGHT)
--
-- Maintainer  :  p.martini@neuralnoise.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Rendering PS documents.
-----------------------------------------------------------------------------

module Graphics.Rendering.Cairo.Internal.Surfaces.PS where

{#import Graphics.Rendering.Cairo.Types#}

import CForeign

{#fun cairo_ps_surface_create  as psSurfaceCreate { `FilePath', `Double', `Double' } -> `Surface' Surface#}
{#fun cairo_ps_surface_set_dpi as psSurfaceSetDPI { `Surface', `Double', `Double' } -> `()'#}
