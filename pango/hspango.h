/* Impedance matching file to include pango/pango.h and at the same time
   ensuring that the pango version macros are defined even for versions
   before 1.6.0. We basically define the version check macros if they
   haven't been defined yet. These macros use the values of the macros
   PANGO_VERSION_... that are defined by pango/pango.h after version 1.6
   and must be defined by the cabal file for earlier versions.
*/

#include<pango/pango.h>
#include<pango/pangocairo.h>

#ifndef PANGO_VERSION_MINOR
#include "hspangoversion.h"
#endif

/* This is copied from pango/pango-utils.h */

/* Pango version checking */

/* Encode a Pango version as an integer */
#ifndef PANGO_VERSION_ENCODE
#define PANGO_VERSION_ENCODE(major, minor, micro) (     \
          ((major) * 10000)                             \
        + ((minor) *   100)                             \
        + ((micro) *     1))
#endif

/* Encoded version of Pango at compile-time */
#ifndef PANGO_VERSION
#define PANGO_VERSION PANGO_VERSION_ENCODE(     \
        PANGO_VERSION_MAJOR,                    \
        PANGO_VERSION_MINOR,                    \
        PANGO_VERSION_MICRO)
#endif

/* Check that compile-time Pango is as new as required */
#ifndef PANGO_VERSION_CHECK
#define PANGO_VERSION_CHECK(major,minor,micro)    \
        (PANGO_VERSION >= PANGO_VERSION_ENCODE(major,minor,micro))
#endif

