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

#include <libgnomevfs/gnome-vfs-file-info.h>

/* This is a stub to work around C2HS tripping over "type" as the
   field name. */
GnomeVFSFileType
_hs_gnome_vfs_file_info_get_type (const GnomeVFSFileInfo *file_info);

/* C2HS doesn't seem to realize GnomeVFSFileSize should be long long */
GnomeVFSInodeNumber
_hs_gnome_vfs_file_info_get_inode (const GnomeVFSFileInfo *file_info);
GnomeVFSFileSize
_hs_gnome_vfs_file_info_get_size (const GnomeVFSFileInfo *file_info);
GnomeVFSFileSize
_hs_gnome_vfs_file_info_get_block_count (const GnomeVFSFileInfo *file_info);

void
_hs_gnome_vfs_file_info_set_type (GnomeVFSFileInfo *file_info,
				  GnomeVFSFileType type);

void
_hs_gnome_vfs_file_info_set_inode (GnomeVFSFileInfo *file_info,
				   GnomeVFSInodeNumber inode);
void
_hs_gnome_vfs_file_info_set_size (GnomeVFSFileInfo *file_info,
				  GnomeVFSFileSize size);
void
_hs_gnome_vfs_file_info_set_block_count (GnomeVFSFileInfo *file_info,
					 GnomeVFSFileSize block_count);
