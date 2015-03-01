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
 *
 * Also g_static_mutex_lock and g_static_mutex_unlock cause linking problems
 * in ghci on Windows 7 (namely: HSgtk-0.10.5.o: unknown symbol
 * `__imp__g_threads_got_initialized'), so we use a Win32 critical section
 * instead.
 */

#define DEFINED_LPTYPELIB
#define DEFINDE_LPTYPEINFO
#define DEFINED_LPTYPECOMP
#define DEFINE_LPCREATETYPEINFO
#define DEFINED_LPDISPATCH

#include <glib.h>
#include <gdk/gdk.h>
#include "hsgthread.h"

#if defined( WIN32 )
#include <windows.h>
#include <stdlib.h>
#include <fcntl.h>
#endif

#undef DEBUG

static int threads_initialised = 0;
#if defined( WIN32 )
static CRITICAL_SECTION gtk2hs_finalizer_mutex;
#else
static GStaticMutex gtk2hs_finalizer_mutex;
#endif
static GSource* gtk2hs_finalizer_source;
static guint gtk2hs_finalizer_id;
static GArray* gtk2hs_finalizers;

gboolean gtk2hs_run_finalizers(gpointer data);

#if defined( WIN32 ) && GLIB_CHECK_VERSION(2,32,0)
static GRecMutex recursive_mutex;

void imp_rec_lock() {
    g_rec_mutex_lock(&recursive_mutex);
}

void imp_rec_unlock() {
    g_rec_mutex_unlock(&recursive_mutex);
}
#endif

/* Initialize the default _fmode on WIN32. */
void gtk2hs_initialise (void) {
#if defined( WIN32 ) && defined( GTK2HS_SET_FMODE_BINARY )
	/* Some Windows GTK binraries (current Fedora MinGW ones) do */
	/* not open files in binary mode.  This is a work around.    */
    HANDLE handle = LoadLibrary("MSVCRT.dll");
    if(!handle) { 
        fprintf(stderr, "Warning: failed to load MSVCRT.dll, ");
        fprintf(stderr, "binary mode was not set!\n");
        return;
    }
    
    int *_fmode_ptr = GetProcAddress(handle, "_fmode");
    if(!_fmode_ptr) {
        fprintf(stderr, "Warning: failed to load address of _fmode from MSVCRT.dll, ");
        fprintf(stderr, "binary mode was not set!\n");
        return;
    }
    *_fmode_ptr = _O_BINARY;
#endif
}

/* Initialize the threads system of Gdk and Gtk. */
void gtk2hs_threads_initialise (void) {

#ifdef DEBUG
  printf("gtk2hs_threads_initialise: threads_initialised=%i, g_thread_get_initialized=%i\n",
		threads_initialised, g_thread_get_initialized());
#endif

  if (!threads_initialised) {
    threads_initialised = 1;
#if defined( WIN32 )
    InitializeCriticalSection(&gtk2hs_finalizer_mutex);
#else
    g_static_mutex_init(&gtk2hs_finalizer_mutex);
#endif

#if defined( WIN32 ) && GLIB_CHECK_VERSION(2,32,0)
    g_rec_mutex_init(&recursive_mutex);
    
    gdk_threads_set_lock_functions(imp_rec_lock, imp_rec_unlock);
#endif
    g_thread_init(NULL);
    gdk_threads_init();

    /* from here onwards, the Gdk lock is held */
    gdk_threads_enter();

  }
}

/* Free an object within the Gtk2Hs lock. */
void gtk2hs_g_object_unref_from_mainloop(gpointer object) {

  int mutex_locked = 0;
  if (threads_initialised) {
#ifdef DEBUG
      printf("acquiring lock to add a %s object at %lx\n",
             g_type_name(G_OBJECT_TYPE(object)), (unsigned long) object);
      printf("value of lock function is %lx\n",
             (unsigned long) g_thread_functions_for_glib_use.mutex_lock);
#endif
#if defined( WIN32 )
    EnterCriticalSection(&gtk2hs_finalizer_mutex);
#else
    g_static_mutex_lock(&gtk2hs_finalizer_mutex);
#endif
    mutex_locked = 1;
  }

#ifdef DEBUG
  if (mutex_locked) printf("within mutex: ");
  printf("adding finalizer to a %s object!\n", g_type_name(G_OBJECT_TYPE(object)));
#endif

  /* Ensure that the idle handler is still installed and that
     the array of objects that are to be finalized exists. */
  if (gtk2hs_finalizer_id==0) {

    if (gtk2hs_finalizers == NULL)
      gtk2hs_finalizers = g_array_new(0, 0, sizeof(gpointer));
#ifdef DEBUG
    printf("creating finalizer list.\n");
#endif

    if (gtk2hs_finalizer_source != NULL) {
#ifdef DEBUG
      printf("re-initializing finalizer source.\n");
#endif
      g_source_destroy(gtk2hs_finalizer_source);
      g_source_unref(gtk2hs_finalizer_source);
    };

    gtk2hs_finalizer_source = g_idle_source_new();
    g_source_set_callback(gtk2hs_finalizer_source, &gtk2hs_run_finalizers, 0, 0);
    gtk2hs_finalizer_id = g_source_attach(gtk2hs_finalizer_source, NULL);

  };

  /* Add the object to the list. */
  g_array_append_val(gtk2hs_finalizers, object);

  if (mutex_locked) {
#ifdef DEBUG
    printf("releasing lock to add a %s object at %lx\n",
           g_type_name(G_OBJECT_TYPE(object)), (unsigned long) object);
#endif
#if defined( WIN32 )
    LeaveCriticalSection(&gtk2hs_finalizer_mutex);
#else
    g_static_mutex_unlock(&gtk2hs_finalizer_mutex);
#endif
  }
}

/* Run the finalizers that have been accumulated. */
gboolean gtk2hs_run_finalizers(gpointer data) {
  gint index;
  g_assert(gtk2hs_finalizers!=NULL);

  gdk_threads_enter();
	
  int mutex_locked = 0;
  if (threads_initialised) {
#ifdef DEBUG
    printf("acquiring lock to kill objects\n");
#endif
#if defined( WIN32 )
    EnterCriticalSection(&gtk2hs_finalizer_mutex);
#else
    g_static_mutex_lock(&gtk2hs_finalizer_mutex);
#endif
    mutex_locked = 1;
  }

#ifdef DEBUG
  printf("running %i finalizers!\n", gtk2hs_finalizers->len);
#endif

  for (index = 0; index < gtk2hs_finalizers->len; index++)
    g_object_unref(g_array_index (gtk2hs_finalizers, GObject*, index));

  g_array_set_size(gtk2hs_finalizers, 0);

  gtk2hs_finalizer_id = 0;

  if (mutex_locked) {
#ifdef DEBUG
    printf("releasing lock to kill objects\n");
#endif
#if defined( WIN32 )
    LeaveCriticalSection(&gtk2hs_finalizer_mutex);
#else
    g_static_mutex_unlock(&gtk2hs_finalizer_mutex);
#endif
  }

  gdk_threads_leave();

  return FALSE;
}

