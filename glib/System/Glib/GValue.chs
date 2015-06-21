-- -*-haskell-*-
--  GIMP Toolkit (GTK) GValue
--
--  Author : Axel Simon
--
--  Created: 1 June 2001
--
--  Copyright (c) 1999..2002 Axel Simon
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- This module implements only the necessities for the GTK binding.
--
-- * Everything here is only used by "Graphics.UI.Gtk.TreeList.TreeModel" and
-- friends.
--
module System.Glib.GValue (
  GValue(GValue),
  valueInit,
  valueGetType,
  allocaGValue
  ) where

import System.Glib.FFI
import System.Glib.GType        (GType)

{# context lib="glib" prefix="g" #}

{# pointer *GValue newtype #}

-- | Clear a GValue.
--
valueInit :: GValue -> GType -> IO ()
valueInit gv gt = do
  -- The g_type field of the value must be zero or g_value_init will fail.
  {# call unsafe value_init #} gv gt
  return ()

-- | Get the type of the value stored in the GValue
--
valueGetType :: GValue -> IO GType
valueGetType (GValue gvPtr) = {# get GValue->g_type #} gvPtr

-- | Temporarily allocate a GValue.
--
allocaGValue :: (GValue -> IO b) -> IO b
allocaGValue body =
  -- c2hs is broken in that it can't handle arrays of compound arrays in the
  -- sizeof hook
  allocaBytes ({# sizeof GType #}+ 2* {# sizeof guint64 #}) $ \gvPtr -> do
  -- The g_type field of the value must be zero or g_value_init will fail.
  {# set GValue->g_type #} gvPtr (0 :: GType)
  result <- body (GValue gvPtr)
  {#call unsafe value_unset#} (GValue gvPtr)
  return result
