#ifndef __HSGSTREAMER_H__
#define __HSGSTREAMER_H__

#include <gst/gst.h>

#include <gst/base/gstadapter.h>
#include <gst/base/gstbasesink.h>
#include <gst/base/gstbasesrc.h>
#include <gst/base/gstbasetransform.h>
#include <gst/base/gstcollectpads.h>
#include <gst/base/gstdataqueue.h>
#include <gst/base/gstpushsrc.h>
#include <gst/base/gsttypefindhelper.h>
#include <gst/controller/gstcontroller.h>

void     _hs_gst_object_lock    (GstObject* obj);
gboolean _hs_gst_object_trylock (GstObject* obj);
void     _hs_gst_object_unlock  (GstObject* obj);
void _hs_gst_object_take_ownership (gpointer obj);
GstMessageType _hs_gst_message_get_message_type (GstMessage *message);
void _hs_gst_structure_make_immutable (GstStructure *structure);
gsize _hs_gst_segment_sizeof (void) G_GNUC_CONST;

#endif
