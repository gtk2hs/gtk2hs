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
