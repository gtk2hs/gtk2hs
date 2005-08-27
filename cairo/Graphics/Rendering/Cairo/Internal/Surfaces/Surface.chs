-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Cairo.Internal.Surfaces.Surface
-- Copyright   :  (c) Paolo Martini 2005
-- License     :  BSD-style (see doc/COPYRIGHT)
--
-- Maintainer  :  p.martini@neuralnoise.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Base class for surfaces.
-----------------------------------------------------------------------------

module Graphics.Rendering.Cairo.Internal.Surfaces.Surface where

{#import Graphics.Rendering.Cairo.Types#}

import Foreign
import CForeign

{#context lib="cairo" prefix="cairo"#}

{#fun surface_create_similar       as surfaceCreateSimilar      { unSurface `Surface', cFromEnum `Content', `Int', `Int' } -> `Surface' Surface#}
{#fun surface_destroy              as surfaceDestroy            { unSurface `Surface' } -> `()'#}
{#fun surface_finish               as surfaceFinish             { unSurface `Surface' } -> `()'#}
{#fun surface_flush                as surfaceFlush              { unSurface `Surface' } -> `()'#}
{#fun surface_get_font_options     as surfaceGetFontOptions     { unSurface `Surface', withFontOptions* `FontOptions'} -> `()'#}
{#fun surface_mark_dirty           as surfaceMarkDirty          { unSurface `Surface' } -> `()'#}
{#fun surface_mark_dirty_rectangle as surfaceMarkDirtyRectangle { unSurface `Surface', `Int', `Int', `Int', `Int' } -> `()'#}
{#fun surface_reference            as surfaceReference          { unSurface `Surface' } -> `()'#}
{#fun surface_set_device_offset    as surfaceSetDeviceOffset    { unSurface `Surface', `Double', `Double' } -> `()'#}
{#fun surface_status               as surfaceStatus             { unSurface `Surface' } -> `Status' cToEnum#}

