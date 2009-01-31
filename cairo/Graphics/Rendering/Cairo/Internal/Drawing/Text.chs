-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Cairo.Internal.Drawing.Text
-- Copyright   :  (c) Paolo Martini 2005
-- License     :  BSD-style (see cairo/COPYRIGHT)
--
-- Maintainer  :  p.martini@neuralnoise.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Rendering text.
-----------------------------------------------------------------------------

module Graphics.Rendering.Cairo.Internal.Drawing.Text where

{#import Graphics.Rendering.Cairo.Types#}

import System.Glib.UTFString (withUTFString)

import Foreign
import Foreign.C

{#context lib="cairo" prefix="cairo"#}

{#fun select_font_face as selectFontFace { unCairo `Cairo', `String', cFromEnum `FontSlant', cFromEnum `FontWeight' } -> `()'#}
{#fun set_font_size    as setFontSize    { unCairo `Cairo', `Double' } -> `()'#}
{#fun set_font_matrix  as setFontMatrix  { unCairo `Cairo', `Matrix' } -> `()'#}
{#fun get_font_matrix  as getFontMatrix  { unCairo `Cairo', alloca- `Matrix' peek*} -> `()'#}
{#fun set_font_options as setFontOptions { unCairo `Cairo',  withFontOptions* `FontOptions' } -> `()'#}
{#fun show_text        as showText       { unCairo `Cairo', withUTFString* `String' } -> `()'#}
{#fun font_extents     as fontExtents    { unCairo `Cairo', alloca- `FontExtents' peek* } -> `()'#}
{#fun text_extents     as textExtents    { unCairo `Cairo', withUTFString* `String', alloca- `TextExtents' peek* } -> `()'#}
