/* We would use the g_thread_supported macro here, but unfortunately on
 * windows GHCi's dynamic linker cannot cope with references to global
 * variables imported from dlls.
 *
 * So instead of asking glib if we (or indeed) anyone else has initialised
 * the glib gthread system, we keep track of it ourselves. We still have to
 * do it in C land so the state survives :reload in GHCi. So there is the
 * danger in a mixed language program, of someone else initialising the
 * glib thread system and us not being aware of it. :-(
 */

#include <glib/gthread.h>

void gtk2hs_threads_initialise () {
  static int threads_initialised = 0;

  if (!threads_initialised) {
    threads_initialised = 1;
    g_thread_init(NULL);
  }
}
