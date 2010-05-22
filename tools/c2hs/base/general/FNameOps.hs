--  Compiler Toolkit: operations on file names
--
--  Author : Manuel M. T. Chakravarty
--  Created: 15 November 98
--
--  Version $Revision: 1.1.1.1 $ from $Date: 2004/11/13 16:42:47 $
--
--  Copyright (c) [1998..1999] Manuel M. T. Chakravarty
--
--  This file is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This file is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--- DESCRIPTION ---------------------------------------------------------------
--
--  Typical operations needed when manipulating file names.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--- TODO ----------------------------------------------------------------------
--

module FNameOps (basename, dirname, stripDirname, suffix, stripSuffix, addPath,
                 splitSearchPath)
where

import System.FilePath

-- strip directory and suffix (EXPORTED)
--
--   eg, ../lib/libc.so -> libc
--
basename :: FilePath -> FilePath
basename  = takeBaseName   

-- strip basename and suffix (EXPORTED)
--
--   eg, ../lib/libc.so -> ../lib/
--
dirname       :: FilePath -> FilePath
dirname = takeDirectory

-- remove dirname (EXPORTED)
--
--   eg, ../lib/libc.so -> libc.so
--
stripDirname       :: FilePath -> FilePath
stripDirname = takeFileName

-- get suffix (EXPORTED)
--
--   eg, ../lib/libc.so -> .so
--
suffix       :: FilePath -> String
suffix = takeExtension

-- remove suffix (EXPORTED)
--
--   eg, ../lib/libc.so -> ../lib/libc
--
stripSuffix       :: FilePath -> FilePath
stripSuffix = dropExtension

-- prepend a path to a file name (EXPORTED)
--
--   eg, ../lib/, libc.so -> ../lib/libc.so
--       ../lib , libc.so -> ../lib/libc.so
--
addPath           :: FilePath -> FilePath -> FilePath
addPath = (</>)


