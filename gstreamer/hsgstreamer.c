#include "hsgstreamer.h"

guint _hs_gst_object_flags (GstObject* obj)
{ return GST_OBJECT_FLAGS (obj); }
void _hs_gst_object_flag_set (GstObject* obj, guint flags)
{ GST_OBJECT_FLAG_SET (obj, flags); }
void _hs_gst_object_flag_unset (GstObject* obj, guint flags)
{ GST_OBJECT_FLAG_UNSET (obj, flags); }

void _hs_gst_object_lock (GstObject* obj)
{ GST_OBJECT_LOCK (obj); }
gboolean _hs_gst_object_trylock (GstObject* obj)
{ return GST_OBJECT_TRYLOCK (obj); }
void _hs_gst_object_unlock (GstObject* obj)
{ return GST_OBJECT_UNLOCK (obj); }

void _hs_gst_object_take_ownership (gpointer obj)
{
  GST_OBJECT_LOCK (obj);
  
  if (GST_OBJECT_IS_FLOATING (obj)) {
    GST_OBJECT_FLAG_UNSET (obj, GST_OBJECT_FLOATING);
  } else {
    gst_object_ref (obj);
  }
  
  GST_OBJECT_UNLOCK (obj);
}

GstMessageType _hs_gst_message_get_message_type (GstMessage *message)
{ return GST_MESSAGE_TYPE (message); }

void _hs_gst_structure_make_immutable (GstStructure *structure)
{
  static gint refcount = 2;
  gst_structure_set_parent_refcount (structure, &refcount);
}

gsize _hs_gst_segment_sizeof (void)
{ return sizeof (GstSegment); }

GstPad *_hs_gst_base_src_get_pad (GstBaseSrc *base_src)
{ return GST_BASE_SRC_PAD (base_src); }
GstPad *_hs_gst_base_sink_get_pad (GstBaseSink *base_sink)
{ return GST_BASE_SINK_PAD (base_sink); }

GstPad *_hs_gst_base_transform_get_sink_pad (GstBaseTransform *base_transform)
{ return GST_BASE_TRANSFORM_SINK_PAD (base_transform); }
GstPad *_hs_gst_base_transform_get_src_pad (GstBaseTransform *base_transform)
{ return GST_BASE_TRANSFORM_SRC_PAD (base_transform); }
