-- -*-haskell-*-
--  GIMP Toolkit (GTK) GType constants
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
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
module System.Glib.GTypeConstants (
  invalid,
  none,
  uint, int,
  uint64, int64,
  uchar, char,
  bool,
  enum, flags,
  pointer,
  float, double,
  string,
  object,
  boxed
  ) where

import System.Glib.GType        (GType)

#include<glib-object.h>

invalid, none, uint, int, uint64, int64, uchar, char, bool, enum, flags,
 pointer, float, double, string, object, boxed :: GType

invalid = #const G_TYPE_INVALID
none    = #const G_TYPE_NONE
uint    = #const G_TYPE_UINT
int     = #const G_TYPE_INT
uint64  = #const G_TYPE_UINT64
int64   = #const G_TYPE_INT64
uchar   = #const G_TYPE_UCHAR
char    = #const G_TYPE_CHAR
bool    = #const G_TYPE_BOOLEAN
enum    = #const G_TYPE_ENUM
flags   = #const G_TYPE_FLAGS
pointer = #const G_TYPE_POINTER
float   = #const G_TYPE_FLOAT
double  = #const G_TYPE_DOUBLE
string  = #const G_TYPE_STRING
object  = #const G_TYPE_OBJECT
boxed   = #const G_TYPE_BOXED
