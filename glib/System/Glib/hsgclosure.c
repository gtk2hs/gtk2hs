/*  GIMP Toolkit (GTK) HsGClosure implementation
 *
 *  Author : Duncan Coutts
 *
 *  Created: 22 March 2005
 *
 *  Version $Revision: 1.1 $ from $Date: 2005/04/06 20:20:16 $
 *
 *  Copyright (C) 2005 Duncan Coutts
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 */

/* GHC's semi-public Rts API */
#include <Rts.h>

#include "hsgclosure.h"

#ifdef DEBUG
#include <stdio.h>
#define WHEN_DEBUG(a) a
#else
#define WHEN_DEBUG(a)
#endif

/* HsGClosure is a _private_ structure */
typedef struct _HsGClosure HsGClosure;
struct _HsGClosure {
    GClosure closure;
    HsStablePtr callback;
};

/* TODO: check if we should be using invalidate or finalise */
static void
hsg_closure_invalidate(gpointer data, GClosure *closure) {
    HsGClosure *hc = (HsGClosure *)closure;
    WHEN_DEBUG(fprintf(stderr, "hsg_closure_invalidate: enter, callback=%p\n", hc->callback));

    /* I think invalidate can be called more than once in the case of cycles
     * so be safe and allow that */
    if (hc->callback)
        hs_free_stable_ptr(hc->callback);

    hc->callback = NULL;
    WHEN_DEBUG(fprintf(stderr, "hsg_closure_invalidate: leave\n"));
}

/* forward defs */
static HaskellObj hsg_value_as_haskellobj(const GValue *value);
static void hsg_value_from_haskellobj(GValue *value, HaskellObj obj);

extern StgClosure * GHCziStable_deRefStablePtr_closure;

static void
hsg_closure_marshal(GClosure *closure,
                    GValue *return_value,
                    guint n_param_values,
                    const GValue *param_values,
                    gpointer invocation_hint,
                    gpointer marshal_data)
{

    HsGClosure *hc = (HsGClosure *)closure;
    HaskellObj call, ret;
    SchedulerStatus rc;
    guint i;
    
    WHEN_DEBUG(fprintf(stderr, "hsg_closure_marshal: about to run callback=%p, n_param_values=%d\n", hc->callback, n_param_values));
    rts_lock();
    
    call = (StgClosure *)deRefStablePtr(hc->callback);
   
    /* construct the function call */
    for (i = 0; i < n_param_values; i++) {
#ifdef DEBUG
        gchar * value_str_rep = g_strdup_value_contents(&param_values[i]);
        fprintf(stderr, "hsg_closure_marshal: param_values[%d]=%s :: %s\n", i, value_str_rep, g_type_name(G_VALUE_TYPE(&param_values[i])));
        g_free(value_str_rep);
#endif
        call = rts_apply(call, hsg_value_as_haskellobj(&param_values[i]));
    }
    
    WHEN_DEBUG(fprintf(stderr, "hsg_closure_marshal: about to rts_evalIO\n"));
    
    /* perform the call */
    rc=rts_evalIO(rts_apply((HaskellObj)runIO_closure, call),&ret);
    
    WHEN_DEBUG(fprintf(stderr, "hsg_closure_marshal: about to rts_checkSchedStatus\n"));
    
    /* barf if anything went wrong */
    /* TODO: pass a sensible value for call site so we get better error messages */
    /* or perhaps we can propogate any error? */
    rts_checkSchedStatus("", rc);
    
    if (return_value) {
        WHEN_DEBUG(fprintf(stderr, "hsg_closure_marshal: type of return_value=%s\n", g_type_name(G_VALUE_TYPE(return_value))));
        hsg_value_from_haskellobj(return_value, ret);
    }
    
    rts_unlock();
    WHEN_DEBUG(fprintf(stderr, "hsg_closure_marshal: done running callback\n"));
}

GClosure *
hsg_closure_new(HsStablePtr callback)
{
    GClosure *closure;
    
    WHEN_DEBUG(fprintf(stderr, "hsg_closure_new: enter, callback=%p\n", callback));
    closure = g_closure_new_simple(sizeof(HsGClosure), NULL);
    /* TODO: check if we should be using invalidate or finalise notifier */
    g_closure_add_invalidate_notifier(closure, NULL, hsg_closure_invalidate);
    g_closure_set_marshal(closure, hsg_closure_marshal);

    ((HsGClosure *)closure)->callback = callback;
    
    WHEN_DEBUG(fprintf(stderr, "hsg_closure_new: leave\n"));
    
    return closure;
}

/* GValue <-> HaskellObj marshaling functions */

static HaskellObj
hsg_value_as_haskellobj(const GValue *value) {
    switch (G_TYPE_FUNDAMENTAL(G_VALUE_TYPE(value))) {
    case G_TYPE_INTERFACE:
        if (g_type_is_a(G_VALUE_TYPE(value), G_TYPE_OBJECT))
            return rts_mkPtr(g_value_get_object(value));
        else
            break;
    case G_TYPE_CHAR:
        return rts_mkChar(g_value_get_char(value));
    case G_TYPE_UCHAR:
        return rts_mkChar(g_value_get_uchar(value));
    case G_TYPE_BOOLEAN:
        return rts_mkBool(g_value_get_boolean(value));
    case G_TYPE_INT:
        return rts_mkInt(g_value_get_int(value));
    case G_TYPE_UINT:
        return rts_mkWord(g_value_get_uint(value));
    case G_TYPE_LONG:
        return rts_mkInt(g_value_get_long(value));
    case G_TYPE_ULONG:
        return rts_mkWord(g_value_get_ulong(value));
/*    case G_TYPE_INT64:
        return rts_mkInt64(g_value_get_int64(value));
    case G_TYPE_UINT64:
        return rts_mkWord64(g_value_get_uint64(value));   */
    case G_TYPE_ENUM:
        return rts_mkInt(g_value_get_enum(value));
    case G_TYPE_FLAGS:
        return rts_mkWord(g_value_get_enum(value));
    case G_TYPE_FLOAT:
        return rts_mkFloat(g_value_get_float(value));
    case G_TYPE_DOUBLE:
        return rts_mkDouble(g_value_get_double(value));
    case G_TYPE_STRING:
        return rts_mkPtr((char *)g_value_get_string(value)); /* CHECKME: is the string freed? */
    case G_TYPE_POINTER:
        return rts_mkPtr(g_value_get_pointer(value));
    case G_TYPE_BOXED:
        return rts_mkPtr(g_value_get_boxed(value));
    case G_TYPE_PARAM:
        break; /* TODO */
        /* return g_value_get_param(value); */
    case G_TYPE_OBJECT:
        return rts_mkPtr(g_value_get_object(value));
    default:
        break;
    }
    WHEN_DEBUG(fprintf(stderr, "hsg_value_as_haskellobj: unable to handle GValue: %s\n", g_strdup_value_contents(value)));
    exit(1);
}

void
hsg_value_from_haskellobj(GValue *value, HaskellObj obj) {

    switch (G_TYPE_FUNDAMENTAL(G_VALUE_TYPE(value))) {
    case G_TYPE_INTERFACE:
        /* we only handle interface types that have a GObject prereq */
        if (g_type_is_a(G_VALUE_TYPE(value), G_TYPE_OBJECT)) {
            g_value_set_object(value, rts_getPtr(obj));
        } else {
            break;
        }
        return;
    case G_TYPE_CHAR:
        g_value_set_char(value, rts_getChar(obj));
        return;
    case G_TYPE_UCHAR:
        g_value_set_char(value, rts_getChar(obj));
        return;
    case G_TYPE_BOOLEAN:
        g_value_set_boolean(value, rts_getBool(obj));
        return;
    case G_TYPE_INT:
        g_value_set_int(value, rts_getInt(obj));
        return;
    case G_TYPE_UINT:
        g_value_set_uint(value, rts_getWord(obj));
        return;
    case G_TYPE_LONG:
        g_value_set_long(value, rts_getInt(obj));
        return;
    case G_TYPE_ULONG:
        g_value_set_ulong(value, rts_getWord(obj));
        return;
/*    case G_TYPE_INT64:
        g_value_set_int64(value, rts_getInt64(obj));
        return;
    case G_TYPE_UINT64:
        g_value_set_uint64(value, rts_getWord64(obj));
        return;                                         */
    case G_TYPE_ENUM:
        g_value_set_enum(value, rts_getInt(obj));
        return;
    case G_TYPE_FLAGS:
        g_value_set_flags(value, rts_getInt(obj));
        return;
    case G_TYPE_FLOAT:
        g_value_set_float(value, rts_getFloat(obj));
        return;
    case G_TYPE_DOUBLE:
        g_value_set_double(value, rts_getDouble(obj));
        return;
    case G_TYPE_STRING:
        g_value_set_string(value, rts_getPtr(obj));
        return;
    case G_TYPE_POINTER:
        g_value_set_pointer(value, rts_getPtr(obj));
        return;
    case G_TYPE_BOXED: {
        /* g_value_set_boxed(value, obj); */
        break; /* TODO */
    }
    case G_TYPE_PARAM:
        /* g_value_set_param(value, (obj)); */
        break; /* TODO */
    case G_TYPE_OBJECT:
        g_value_set_object(value, rts_getPtr(obj));
        return;
    default:
        break; 
    }
    /* FIXME: improve error handling here */
    WHEN_DEBUG(fprintf(stderr, "hsg_value_from_haskellobj: unable to handle GValue with type %s\n", g_type_name(G_VALUE_TYPE((value)))));
    exit(1);
}
