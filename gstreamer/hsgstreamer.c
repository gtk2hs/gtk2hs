/*  GIMP Toolkit (GTK) Binding for Haskell: binding to gstreamer
 *
 *  Author : Peter Gavin
 *  Created: 1-Apr-2007
 *
 *  Copyright (c) 2007 Peter Gavin
 *
 *  This library is free software: you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public License
 *  as published by the Free Software Foundation, either version 3 of
 *  the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *  
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this program.  If not, see
 *  <http://www.gnu.org/licenses/>.
 *  
 *  GStreamer, the C library which this Haskell library depends on, is
 *  available under LGPL Version 2. The documentation included with
 *  this library is based on the original GStreamer documentation.
 */
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

void _hs_gst_object_unfloat (gpointer obj)
{
  GST_OBJECT_LOCK (obj);
  
  GST_OBJECT_FLAG_UNSET (obj, GST_OBJECT_FLOATING);
  
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

guint _hs_gst_mini_object_flags (GstMiniObject* obj)
{ return GST_MINI_OBJECT_FLAGS (obj); }
void _hs_gst_mini_object_flag_set (GstMiniObject* obj, guint flags)
{ GST_MINI_OBJECT_FLAG_SET (obj, flags); }
void _hs_gst_mini_object_flag_unset (GstMiniObject* obj, guint flags)
{ GST_MINI_OBJECT_FLAG_UNSET (obj, flags); }
void _hs_gst_mini_object_make_read_only (GstMiniObject *obj)
{ GST_MINI_OBJECT_FLAG_SET (obj, GST_MINI_OBJECT_FLAG_READONLY); }

GstQueryType _hs_gst_query_type (GstQuery *query)
{ return GST_QUERY_TYPE (query); }
GstEventType _hs_gst_event_type (GstEvent *event)
{ return GST_EVENT_TYPE (event); }

GstClockTime _hs_gst_frames_to_clock_time (gint64 frames, double rate)
{ return GST_FRAMES_TO_CLOCK_TIME (frames, rate); }
gint64       _hs_gst_clock_time_to_frames (GstClockTime clock_time, double rate)
{ return GST_CLOCK_TIME_TO_FRAMES (clock_time, rate); }
