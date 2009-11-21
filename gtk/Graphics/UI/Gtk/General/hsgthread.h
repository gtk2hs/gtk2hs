#include <glib.h>

#ifndef HSGTHREAD_H
#define HSGTHREAD_H

/* Lock the Gtk2Hs lock. */
void gtk2hs_lock(void);

/* Unlock the Gtk2Hs lock. */
void gtk2hs_unlock(void);

/* Initialize the threads system of Gdk and Gtk. Furthermore, install the
Gtk2Hs specific lock and unlock functions. */
int gtk2hs_threads_initialise (void);

/* Free an object within the Gtk2Hs lock. */
void gtk2hs_g_object_unref_locked(gpointer object);

#endif HSGTHREAD_H

