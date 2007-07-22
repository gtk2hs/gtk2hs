/* GIMP Toolkit (GTK) Binding for Haskell: binding to libgnomevfs -*-c-*-
 *
 * Author : Peter Gavin
 * Created: 1-Apr-2007
 *
 * Copyright (c) 2007 Peter Gavin
 *
 * This library is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation, either version 3 of
 * the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program.  If not, see
 * <http://www.gnu.org/licenses/>.
 * 
 * GnomeVFS, the C library which this Haskell library depends on, is
 * available under LGPL Version 2. The documentation included with
 * this library is based on the original GnomeVFS documentation,
 * Copyright (c) 2001 Seth Nickell <snickell@stanford.edu>. The
 * documentation is covered by the GNU Free Documentation License,
 * version 1.2.
 */

#include "hsfileinfo.h"

GnomeVFSFileType
_hs_gnome_vfs_file_info_get_type (const GnomeVFSFileInfo *file_info)
{ return file_info->type; }

GnomeVFSInodeNumber
_hs_gnome_vfs_file_info_get_inode (const GnomeVFSFileInfo *file_info)
{ return file_info->inode; }
GnomeVFSFileSize
_hs_gnome_vfs_file_info_get_size (const GnomeVFSFileInfo *file_info)
{ return file_info->size; }
GnomeVFSFileSize
_hs_gnome_vfs_file_info_get_block_count (const GnomeVFSFileInfo *file_info)
{ return file_info->block_count; }

void
_hs_gnome_vfs_file_info_set_type (GnomeVFSFileInfo *file_info,
				  GnomeVFSFileType type)
{ file_info->type = type; }
void
_hs_gnome_vfs_file_info_set_inode (GnomeVFSFileInfo *file_info,
				   GnomeVFSInodeNumber inode)
{ file_info->inode = inode; }
void
_hs_gnome_vfs_file_info_set_size (GnomeVFSFileInfo *file_info,
				  GnomeVFSFileSize size)
{ file_info->size = size; }
void
_hs_gnome_vfs_file_info_set_block_count (GnomeVFSFileInfo *file_info,
					 GnomeVFSFileSize block_count)
{ file_info->block_count = block_count; }
