/*  GIMP Toolkit (GTK) HSGClosure interface
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

#include <glib-object.h>

GClosure * hsg_closure_new(HsStablePtr callback);
