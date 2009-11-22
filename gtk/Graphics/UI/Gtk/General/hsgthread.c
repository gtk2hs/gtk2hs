/* We would use the g_thread_supported macro here, but unfortunately on
 * windows GHCi's dynamic linker cannot cope with references to global
 * variables imported from dlls.
 *
 * So instead of asking glib if we (or indeed) anyone else has initialised
 * the glib gthread system, we keep track of it ourselves. We still have to
 * do it in C land so the state survives :reload in GHCi. So there is the
 * danger in a mixed language program, of someone else initialising the
 * glib thread system and us not being aware of it. :-(
 *
 * Besides the interaction with ghci, we provide a variant of g_object_unref
 * that is used in all objects of Gtk+ and those libraries that build on Gtk+.
 * This variant enqueues the object to be finalized and adds an idle handler
 * into the main loop of Gtk+ that will call the actual finalizers on the
 * enqueued objects. The aim is to ensure that finalizers for objects that
 * may hold Xlib or Win32 resources are only run from the thread that runs the
 * main Gtk+ loop. If this is not ensured then bad things happen at least on
 * Win32 since that API is making use of thread-local storage that is not
 * present if the finalizers, that are run by the GC in a different thread,
 * call back into Win32 without this thread-local storage.
 */

#include <glib.h>
#include <glib/gthread.h>
#include <gdk/gdk.h>

#undef DEBUG

static GStaticMutex gtk2hs_finalizer_mutex;
static GSource* gtk2hs_finalizer_source;
static guint gtk2hs_finalizer_id;
static GArray* gtk2hs_finalizers;

gboolean gtk2hs_run_finalizers(gpointer data);

/* Initialize the threads system of Gdk and Gtk. */
void gtk2hs_threads_initialise () {
  static int threads_initialised = 0;

  if (!threads_initialised) {
    threads_initialised = 1;
    g_thread_init(NULL);
    gdk_threads_init();
  }
}

/* Free an object within the Gtk2Hs lock. */
void gtk2hs_g_object_unref_from_mainloop(gpointer object) {
  g_static_mutex_lock(&gtk2hs_finalizer_mutex);

#ifdef DEBUG
  printf("adding finalizer!\n");
#endif

  /* Ensure that the idle handler is still installed and that
     the array of objects that are to be finalized exists. */
  if (gtk2hs_finalizer_id==0) {

    if (gtk2hs_finalizers == NULL)
      gtk2hs_finalizers = g_array_new(0, 0, sizeof(gpointer));

    if (gtk2hs_finalizer_source != NULL) {
      g_source_destroy(gtk2hs_finalizer_source);
      g_source_unref(gtk2hs_finalizer_source);
    };

    gtk2hs_finalizer_source = g_idle_source_new();
    g_source_set_callback(gtk2hs_finalizer_source, &gtk2hs_run_finalizers, 0, 0);
    gtk2hs_finalizer_id = g_source_attach(gtk2hs_finalizer_source, NULL);

  };

  /* Add the object to the list. */
  g_array_append_val(gtk2hs_finalizers, object);

  g_static_mutex_unlock(&gtk2hs_finalizer_mutex);
}

/* Run the finalizers that have been accumulated. */
gboolean gtk2hs_run_finalizers(gpointer data) {
  gint index;
  g_assert(gtk2hs_finalizers!=NULL);

  g_static_mutex_lock(&gtk2hs_finalizer_mutex);

#ifdef DEBUG
  printf("running %i finalizers!\n", gtk2hs_finalizers->len);
#endif

  for (index = 0; index < gtk2hs_finalizers->len; index++)
    g_object_unref(g_array_index (gtk2hs_finalizers, GObject*, index));

  g_array_set_size(gtk2hs_finalizers, 0);

  gtk2hs_finalizer_id = 0;

  g_static_mutex_unlock(&gtk2hs_finalizer_mutex);

  return FALSE;
}

