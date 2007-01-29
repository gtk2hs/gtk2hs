#include <glib/gthread.h>

void gtk2hs_thread_init() {
  if (!g_thread_supported ())
    g_thread_init (NULL);
}
