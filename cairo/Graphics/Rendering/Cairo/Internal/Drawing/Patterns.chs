-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Cairo.Internal.Drawing.Patterns
-- Copyright   :  (c) Paolo Martini 2005
-- License     :  BSD-style (see cairo/COPYRIGHT)
--
-- Maintainer  :  p.martini@neuralnoise.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Gradients and filtered sources.
-----------------------------------------------------------------------------

module Graphics.Rendering.Cairo.Internal.Drawing.Patterns where

{#import Graphics.Rendering.Cairo.Types#}
{#import Graphics.Rendering.Cairo.Internal.Drawing.Paths #} ( CPath(..), pathDestroy, pathToList )


import Foreign
import Foreign.C

{#context lib="cairo" prefix="cairo"#}

{#fun pattern_create_rgb            as patternCreateRGB           { `Double', `Double', `Double' } -> `Pattern' mkPattern* #}
{#fun pattern_create_rgba           as patternCreateRGBA          { `Double', `Double', `Double', `Double' } -> `Pattern' mkPattern* #}
{#fun pattern_get_rgba              as patternGetRGBA             { withPattern* `Pattern', alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv* } -> `Status' cToEnum #}

{#fun pattern_create_linear         as patternCreateLinear        { `Double', `Double', `Double', `Double' } -> `Pattern' mkPattern* #}
{#fun pattern_create_radial         as patternCreateRadial        { `Double', `Double', `Double', `Double', `Double', `Double' } -> `Pattern' mkPattern* #}
{#fun pattern_add_color_stop_rgb    as patternAddColorStopRGB     { withPattern* `Pattern', `Double', `Double', `Double', `Double' } -> `()' #}
{#fun pattern_add_color_stop_rgba   as patternAddColorStopRGBA    { withPattern* `Pattern', `Double', `Double', `Double', `Double', `Double' } -> `()'#}
{#fun pattern_get_color_stop_count  as patternGetColorStopCount   { withPattern* `Pattern', alloca- `Int' peekIntConv* } -> `Status' cToEnum #}
{#fun pattern_get_color_stop_rgba   as patternGetColorStopRGBA    { withPattern* `Pattern', `Int', alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv*} -> `Status' cToEnum #}
{#fun pattern_get_linear_points     as patternGetLinearPoints     { withPattern* `Pattern', alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv* } -> `Status' cToEnum #}
{#fun pattern_get_radial_circles    as patternGetRadialCircles    { withPattern* `Pattern', alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv* } -> `Status' cToEnum #}

{#fun pattern_create_for_surface    as patternCreateForSurface    { withSurface* `Surface' } -> `Pattern' mkPattern* #}

{#fun pattern_status                as patternStatus              { withPattern* `Pattern' } -> `Status' cToEnum#}
{#fun pattern_get_type              as patternGetType             { withPattern* `Pattern' } -> `PatternType' cToEnum #}

{#fun pattern_set_extend            as patternSetExtend           { withPattern* `Pattern', cFromEnum `Extend' } -> `()'#}
{#fun pattern_get_extend            as patternGetExtend           { withPattern* `Pattern' } -> `Extend' cToEnum#}
{#fun pattern_set_filter            as patternSetFilter           { withPattern* `Pattern', cFromEnum `Filter' } -> `()'#}
{#fun pattern_get_filter            as patternGetFilter           { withPattern* `Pattern' } -> `Filter' cToEnum#}
{#fun pattern_set_matrix            as patternSetMatrix           { withPattern* `Pattern', `Matrix' } -> `()'#}
{#fun pattern_get_matrix            as patternGetMatrix           { withPattern* `Pattern', alloca- `Matrix' peek*} -> `()'#}


-- support for mesh patterns / tensor product patches
#if CAIRO_CHECK_VERSION(1,12,0)
{#fun pattern_create_mesh                 as patternCreateMesh             { } -> `Pattern' mkPattern* #}
{#fun mesh_pattern_begin_patch            as meshPatternBeginPatch         { withPattern* `Pattern' } -> `()' #}
{#fun mesh_pattern_end_patch              as meshPatternEndPatch           { withPattern* `Pattern' } -> `()' #}
{#fun mesh_pattern_move_to                as meshPatternMoveTo             { withPattern* `Pattern', `Double', `Double' } -> `()' #}
{#fun mesh_pattern_line_to                as meshPatternLineTo             { withPattern* `Pattern', `Double', `Double' } -> `()' #}
{#fun mesh_pattern_curve_to               as meshPatternCurveTo            { withPattern* `Pattern', `Double', `Double', `Double', `Double', `Double', `Double' } -> `()' #}
{#fun mesh_pattern_set_control_point      as meshPatternSetControlPoint    { withPattern* `Pattern', fromIntegral `Int', `Double', `Double' } -> `()' #}
{#fun mesh_pattern_set_corner_color_rgb   as meshPatternSetCornerColorRGB  { withPattern* `Pattern', fromIntegral `Int', `Double', `Double', `Double' } -> `()' #}
{#fun mesh_pattern_set_corner_color_rgba  as meshPatternSetCornerColorRGBA { withPattern* `Pattern', fromIntegral `Int', `Double', `Double', `Double', `Double'} -> `()' #}
{#fun mesh_pattern_get_patch_count        as meshPatternGetPatchCount      { withPattern* `Pattern', alloca- `Int' peekIntConv* } -> `Status' cToEnum #}
{#fun mesh_pattern_get_path               as meshPatternGetPathC           { withPattern* `Pattern', fromIntegral `Int' } -> `CPath' CPath #}
{#fun mesh_pattern_get_control_point      as meshPatternGetControlPoint    { withPattern* `Pattern', fromIntegral `Int', fromIntegral `Int', alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv* } -> `Status' cToEnum #}
{#fun mesh_pattern_get_corner_color_rgba  as meshPatternGetCornerColorRGBA { withPattern* `Pattern', fromIntegral `Int', fromIntegral `Int', alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv* } -> `Status' cToEnum #}

meshPatternGetPath :: Pattern -> Int -> IO [PathElement]
meshPatternGetPath pat n = do
   path <- meshPatternGetPathC pat n
   xs   <- pathToList path
   pathDestroy path
   return xs

convertPathElement :: Pattern -> PathElement -> IO ()
convertPathElement pat (MoveTo x y)                = meshPatternMoveTo  pat x y
convertPathElement pat (LineTo x y)                = meshPatternLineTo  pat x y
convertPathElement pat (CurveTo x1 y1 x2 y2 x3 y3) = meshPatternCurveTo pat x1 y1 x2 y2 x3 y3
convertPathElement  _  ClosePath                   = return ()

#endif

