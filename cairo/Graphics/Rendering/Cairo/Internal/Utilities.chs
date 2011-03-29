-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Cairo.Internal.Utilities
-- Copyright   :  (c) Paolo Martini 2005
-- License     :  BSD-style (see cairo/COPYRIGHT)
--
-- Maintainer  :  p.martini@neuralnoise.com
-- Stability   :  experimental
-- Portability :  portable
--
-- http://cairographics.org/manual/Support.html
-----------------------------------------------------------------------------

module Graphics.Rendering.Cairo.Internal.Utilities where

{#import Graphics.Rendering.Cairo.Types#}

import Foreign
import Foreign.C

import Data.Char (ord, chr)

{#context lib="cairo" prefix="cairo"#}

{#fun status_to_string    as statusToString { cFromEnum `Status' } -> `String'#}
{#fun pure version        as version        {} -> `Int'#}
{#fun pure version_string as versionString  {} -> `String'#}

-- These functions taken from System/Glib/UTFString.hs
-- Copyright (c) 1999..2002 Axel Simon

-- Define withUTFString to emit UTF-8.
--
withUTFString :: String -> (CString -> IO a) -> IO a
withUTFString hsStr = withCString (toUTF hsStr)
 where
    -- Convert Unicode characters to UTF-8.
    --
    toUTF :: String -> String
    toUTF [] = []
    toUTF (x:xs) | ord x<=0x007F = x:toUTF xs
                 | ord x<=0x07FF = chr (0xC0 .|. ((ord x `shift` (-6)) .&. 0x1F)):
                                   chr (0x80 .|. (ord x .&. 0x3F)):
                                   toUTF xs
                 | otherwise     = chr (0xE0 .|. ((ord x `shift` (-12)) .&. 0x0F)):
                                   chr (0x80 .|. ((ord x `shift` (-6)) .&. 0x3F)):
                                   chr (0x80 .|. (ord x .&. 0x3F)):
                                   toUTF xs
