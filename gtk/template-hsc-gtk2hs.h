#ifndef _TEMPLATE_HSC_GTK2HS_H_
#define _TEMPLATE_HSC_GTK2HS_H_

#include <glib.h>

#define hsc_gtk2hs_type(t)                        \
  if ((t)(int)(t)1.4 == (t)1.4)                   \
    printf ("%s%" G_GSIZE_FORMAT,                 \
      (t)(-1) < (t)0 ? "Int" : "Word",            \
            sizeof (t) * 8);                      \
  else                                            \
    printf ("%s",                                 \
      sizeof (t) >  sizeof (double) ? "LDouble" : \
      sizeof (t) == sizeof (double) ? "Double"  : \
            "Float");

#endif
