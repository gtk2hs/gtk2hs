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
import System.IO.Unsafe (unsafePerformIO)

import Codec.Binary.UTF8.String
import Data.Char (ord, chr)

{#context lib="cairo" prefix="cairo"#}

{#fun status_to_string    as statusToString { cFromEnum `Status' } -> `String'#}
{#fun pure version        as version        {} -> `Int'#}
{#fun pure version_string as versionString  {} -> `String'#}

withUTFString :: String -> (CString -> IO a) -> IO a
withUTFString = withCAString . encodeString
