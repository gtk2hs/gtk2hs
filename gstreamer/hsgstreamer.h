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
#include <gst/net/gstnet.h>
#include <gst/dataprotocol/dataprotocol.h>
#include <gst/audio/audio.h>

guint    _hs_gst_object_flags (GstObject* obj);
void     _hs_gst_object_flag_set (GstObject* obj, guint flags);
void     _hs_gst_object_flag_unset (GstObject* obj, guint flags);
void     _hs_gst_object_lock    (GstObject* obj);
gboolean _hs_gst_object_trylock (GstObject* obj);
void     _hs_gst_object_unlock  (GstObject* obj);
void _hs_gst_object_unfloat (gpointer obj);
GstMessageType _hs_gst_message_get_message_type (GstMessage *message);
void _hs_gst_structure_make_immutable (GstStructure *structure);
gsize _hs_gst_segment_sizeof (void) G_GNUC_CONST;

GstPad *_hs_gst_base_src_get_pad (GstBaseSrc *base_src);
GstPad *_hs_gst_base_sink_get_pad (GstBaseSink *base_sink);
GstPad *_hs_gst_base_transform_get_sink_pad (GstBaseTransform *base_transform);
GstPad *_hs_gst_base_transform_get_src_pad (GstBaseTransform *base_transform);

guint    _hs_gst_mini_object_flags (GstMiniObject* obj);
void     _hs_gst_mini_object_flag_set (GstMiniObject* obj, guint flags);
void     _hs_gst_mini_object_flag_unset (GstMiniObject* obj, guint flags);
void     _hs_gst_mini_object_make_read_only (GstMiniObject *obj);

GstQueryType _hs_gst_query_type (GstQuery *query);
GstEventType _hs_gst_event_type (GstEvent *event);

GstClockTime _hs_gst_frames_to_clock_time (gint64 frames, double rate);
gint64       _hs_gst_clock_time_to_frames (GstClockTime clock_time, double rate);

#endif
