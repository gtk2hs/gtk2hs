{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- -*-haskell-*-
-- -------------------- automatically generated file - do not edit ----------
--  Object hierarchy for the GIMP Toolkit (GTK) Binding for Haskell
--
--  Author : Hamish Mackenzie
--
--  Copyright (C) 2001-2005 Axel Simon
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
-- #hide

-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
module Graphics.UI.Gtk.Embedding.Types (

#if (defined(HAVE_PLUG_AND_SOCKET) && (!defined(WIN32) || GTK_CHECK_VERSION(2,8,0))) || defined(GDK_WINDOWING_X11)
  Socket(Socket), SocketClass,
  toSocket, 
  mkSocket, unSocket,
  castToSocket, gTypeSocket,
  Plug(Plug), PlugClass,
  toPlug, 
  mkPlug, unPlug,
  castToPlug, gTypePlug,
#endif
  ) where

#if (defined(HAVE_PLUG_AND_SOCKET) && (!defined(WIN32) || GTK_CHECK_VERSION(2,8,0))) || defined(GDK_WINDOWING_X11)
import Foreign.ForeignPtr (ForeignPtr, castForeignPtr)
-- TODO work around cpphs https://ghc.haskell.org/trac/ghc/ticket/13553
#if __GLASGOW_HASKELL__ >= 707 || __GLASGOW_HASKELL__ == 0
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
#else
import Foreign.ForeignPtr (unsafeForeignPtrToPtr)
#endif
import Foreign.C.Types    (CULong(..), CUInt(..), CULLong(..))
import System.Glib.GType  (GType, typeInstanceIsA)
{#import System.Glib.GObject#}
import Graphics.UI.Gtk.General.Threading
{#import Graphics.UI.Gtk.Types#}

{# context lib="gtk" prefix="gtk" #}

-- The usage of foreignPtrToPtr should be safe as the evaluation will only be
-- forced if the object is used afterwards
--
castTo :: (GObjectClass obj, GObjectClass obj') => GType -> String
                                                -> (obj -> obj')
castTo gtype objTypeName obj =
  case toGObject obj of
    gobj@(GObject objFPtr)
      | typeInstanceIsA ((unsafeForeignPtrToPtr.castForeignPtr) objFPtr) gtype
                  -> unsafeCastGObject gobj
      | otherwise -> error $ "Cannot cast object to " ++ objTypeName


-- ****************************************************************** Socket

{#pointer *GtkSocket as Socket foreign newtype #} deriving (Eq,Ord)

mkSocket = (Socket, objectUnrefFromMainloop)
unSocket (Socket o) = o

class ContainerClass o => SocketClass o
toSocket :: SocketClass o => o -> Socket
toSocket = unsafeCastGObject . toGObject

instance SocketClass Socket
instance ContainerClass Socket
instance WidgetClass Socket
#if GTK_MAJOR_VERSION < 3
instance ObjectClass Socket
#endif
instance GObjectClass Socket where
  toGObject = GObject . castForeignPtr . unSocket
  unsafeCastGObject = Socket . castForeignPtr . unGObject

castToSocket :: GObjectClass obj => obj -> Socket
castToSocket = castTo gTypeSocket "Socket"

gTypeSocket :: GType
gTypeSocket =
  {# call fun unsafe gtk_socket_get_type #}

-- ****************************************************************** Plug

{#pointer *GtkPlug as Plug foreign newtype #} deriving (Eq,Ord)

mkPlug = (Plug, objectUnrefFromMainloop)
unPlug (Plug o) = o

class WindowClass o => PlugClass o
toPlug :: PlugClass o => o -> Plug
toPlug = unsafeCastGObject . toGObject

instance PlugClass Plug
instance WindowClass Plug
instance BinClass Plug
instance ContainerClass Plug
instance WidgetClass Plug
#if GTK_MAJOR_VERSION < 3
instance ObjectClass Plug
#endif
instance GObjectClass Plug where
  toGObject = GObject . castForeignPtr . unPlug
  unsafeCastGObject = Plug . castForeignPtr . unGObject

castToPlug :: GObjectClass obj => obj -> Plug
castToPlug = castTo gTypePlug "Plug"

gTypePlug :: GType
gTypePlug =
  {# call fun unsafe plug_get_type #}
#endif
