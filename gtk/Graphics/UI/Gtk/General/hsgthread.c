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
#include <gdk/gdk.h>

static GStaticRecMutex gtk2hs_mutex;

/* Lock the Gtk2Hs lock. */
void gtk2hs_lock() {
  g_static_rec_mutex_lock(&gtk2hs_mutex);
}

/* Unlock the Gtk2Hs lock. */
void gtk2hs_unlock() {
  g_static_rec_mutex_unlock(&gtk2hs_mutex);
}

/* Initialize the threads system of Gdk and Gtk. Furthermore, install the
Gtk2Hs specific lock and unlock functions. */
void gtk2hs_threads_initialise () {
  static int threads_initialised = 0;

  if (!threads_initialised) {
    threads_initialised = 1;
    gdk_threads_set_lock_functions((GCallback) &gtk2hs_lock,
                                   (GCallback) &gtk2hs_unlock );
    g_thread_init(NULL);
    gdk_threads_init();
    g_static_rec_mutex_init(&gtk2hs_mutex);
  }
}

/* Free an object within the Gtk2Hs lock. */
void gtk2hs_g_object_unref_locked(gpointer object) {
  g_static_rec_mutex_lock(&gtk2hs_mutex);
  g_object_unref(object);
  g_static_rec_mutex_unlock(&gtk2hs_mutex);
}
