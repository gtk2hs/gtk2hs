-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Cairo.Internal.Surfaces.PDF
-- Copyright   :  (c) Paolo Martini 2005
-- License     :  BSD-style (see doc/COPYRIGHT)
--
-- Maintainer  :  p.martini@neuralnoise.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Rendering PDF documents.
-----------------------------------------------------------------------------

module Graphics.Rendering.Cairo.Internal.Surfaces.PDF where

{#import Graphics.Rendering.Cairo.Types#}

import Foreign
import CForeign

{#fun cairo_pdf_surface_create  as pdfSurfaceCreate { `FilePath', `Double', `Double' } -> `Surface' Surface#}
{#fun cairo_pdf_surface_set_dpi as pdfSurfaceSetDPI { `Surface', `Double', `Double' } -> `()'#}
