{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}
--  GIMP Toolkit (GTK) Binding for Haskell: binding to libgnomevfs -*-haskell-*-
--
--  Author : Peter Gavin
--  Created: 21-Jun-2008
--
--  Copyright (c) 2008 Peter Gavin
--
--  This library is free software: you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public License
--  as published by the Free Software Foundation, either version 3 of
--  the License, or (at your option) any later version.
--  
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--  
--  You should have received a copy of the GNU Lesser General Public
--  License along with this program.  If not, see
--  <http://www.gnu.org/licenses/>.
--  
--  GnomeVFS, the C library which this Haskell library depends on, is
--  available under LGPL Version 2. The documentation included with
--  this library is based on the original GnomeVFS documentation,
--  Copyright (c) 2001 Seth Nickell <snickell@stanford.edu>. The
--  documentation is covered by the GNU Free Documentation License,
--  version 1.2.

-- #hide

#include <libgnomevfs/gnome-vfs-file-info.h>

-- | Maintainer  : gtk2hs-devel@lists.sourceforge.net
--   Stability   : alpha
--   Portability : portable (depends on GHC)
module System.Gnome.VFS.Constants (

  FilePermissions (..),
  permUserAll,
  permGroupAll,
  permOtherAll

  ) where

import System.Glib.Flags

-- | UNIX-like permissions for a file.
data FilePermissions 
#ifndef WIN32
    = PermSUID
    | PermSGID
    | PermSticky
#else
    = PermSticky
#endif
    | PermUserRead
    | PermUserWrite
    | PermUserExec
    | PermGroupRead
    | PermGroupWrite
    | PermGroupExec
    | PermOtherRead
    | PermOtherWrite
    | PermOtherExec
    | PermAccessReadable
    | PermAccessWritable
    | PermAccessExecutable
      deriving (Eq, Ord, Bounded, Show, Read)
instance Flags FilePermissions
permUserAll, permGroupAll, permOtherAll :: [FilePermissions]
permUserAll  = [ PermUserRead, PermUserWrite, PermUserExec ]
permGroupAll = [ PermGroupRead, PermGroupWrite, PermGroupExec ]
permOtherAll = [ PermOtherRead, PermOtherWrite, PermOtherExec ]

instance Enum FilePermissions where
#ifndef WIN32
    fromEnum PermSUID = #{const GNOME_VFS_PERM_SUID}
    fromEnum PermSGID = #{const GNOME_VFS_PERM_SGID}
#endif
    fromEnum PermSticky = #{const GNOME_VFS_PERM_STICKY}
    fromEnum PermUserRead = #{const GNOME_VFS_PERM_USER_READ}
    fromEnum PermUserWrite = #{const GNOME_VFS_PERM_USER_WRITE}
    fromEnum PermUserExec = #{const GNOME_VFS_PERM_USER_EXEC}
    fromEnum PermGroupRead = #{const GNOME_VFS_PERM_GROUP_READ}
    fromEnum PermGroupWrite = #{const GNOME_VFS_PERM_GROUP_WRITE}
    fromEnum PermGroupExec = #{const GNOME_VFS_PERM_GROUP_EXEC}
    fromEnum PermOtherRead = #{const GNOME_VFS_PERM_OTHER_READ}
    fromEnum PermOtherWrite = #{const GNOME_VFS_PERM_OTHER_WRITE}
    fromEnum PermOtherExec = #{const GNOME_VFS_PERM_OTHER_EXEC}
    fromEnum PermAccessReadable = #{const GNOME_VFS_PERM_ACCESS_READABLE}
    fromEnum PermAccessWritable = #{const GNOME_VFS_PERM_ACCESS_WRITABLE}
    fromEnum PermAccessExecutable = #{const GNOME_VFS_PERM_ACCESS_EXECUTABLE}
    
#ifndef WIN32
    toEnum #{const GNOME_VFS_PERM_SUID} = PermSUID
    toEnum #{const GNOME_VFS_PERM_SGID} = PermSGID
#endif
    toEnum #{const GNOME_VFS_PERM_STICKY} = PermSticky
    toEnum #{const GNOME_VFS_PERM_USER_READ} = PermUserRead
    toEnum #{const GNOME_VFS_PERM_USER_WRITE} = PermUserWrite
    toEnum #{const GNOME_VFS_PERM_USER_EXEC} = PermUserExec
    toEnum #{const GNOME_VFS_PERM_GROUP_READ} = PermGroupRead
    toEnum #{const GNOME_VFS_PERM_GROUP_WRITE} = PermGroupWrite
    toEnum #{const GNOME_VFS_PERM_GROUP_EXEC} = PermGroupExec
    toEnum #{const GNOME_VFS_PERM_OTHER_READ} = PermOtherRead
    toEnum #{const GNOME_VFS_PERM_OTHER_WRITE} = PermOtherWrite
    toEnum #{const GNOME_VFS_PERM_OTHER_EXEC} = PermOtherExec
    toEnum #{const GNOME_VFS_PERM_ACCESS_READABLE} = PermAccessReadable
    toEnum #{const GNOME_VFS_PERM_ACCESS_WRITABLE} = PermAccessWritable
    toEnum #{const GNOME_VFS_PERM_ACCESS_EXECUTABLE} = PermAccessExecutable
