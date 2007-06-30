/* Annoyingly, g_thread_supported is actuall a macro so we need this silly
 * little wrapper functions. Grrr. */

#include <glib/gthread.h>

int gtk2hs_thread_supported () {
  return g_thread_supported();
}
