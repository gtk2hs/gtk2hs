/*  GIMP Toolkit (GTK) Binding for Haskell: binding to gtk
 *
 *  Author : Peter Gavin
 *  Created: 25-Jan-2009
 *
 *  Copyright (c) 2009 Peter Gavin
 *
 *  This library is free software: you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public License
 *  as published by the Free Software Foundation, either version 3 of
 *  the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *  
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this program.  If not, see
 *  <http://www.gnu.org/licenses/>.
 *  
 *  GStreamer, the C library which this Haskell library depends on, is
 *  available under LGPL Version 2. The documentation included with
 *  this library is based on the original GStreamer documentation.
 */
#ifndef __HSGTK_H__
#define __HSGTK_H__

#include <gtk/gtk.h>
#include <pango/pango.h>
#include <gtk2hs-config.h>

#if ((_PANGO_MAJOR_VERSION < 1) ||                                      \
     ((_PANGO_MAJOR_VERSION == 1) && (_PANGO_MINOR_VERSION < 16)))

#define PANGO_VERSION_MAJOR _PANGO_MAJOR_VERSION
#define PANGO_VERSION_MINOR _PANGO_MINOR_VERSION
#define PANGO_VERSION_MICRO _PANGO_MICRO_VERSION

/* Encode a Pango version as an integer */
#define PANGO_VERSION_ENCODE(major, minor, micro) (     \
          ((major) * 10000)                             \
        + ((minor) *   100)                             \
        + ((micro) *     1))

/* Encoded version of Pango at compile-time */
#define PANGO_VERSION PANGO_VERSION_ENCODE(     \
        PANGO_VERSION_MAJOR,                    \
        PANGO_VERSION_MINOR,                    \
        PANGO_VERSION_MICRO)

/* Check that compile-time Pango is as new as required */
#define PANGO_VERSION_CHECK(major,minor,micro)    \
        (PANGO_VERSION >= PANGO_VERSION_ENCODE(major,minor,micro))
#endif

#endif
