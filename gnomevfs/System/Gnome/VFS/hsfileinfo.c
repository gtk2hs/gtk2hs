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
