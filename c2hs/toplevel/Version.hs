module Version (version, copyright, disclaimer)
where

-- version number is major.minor.patchlvl; don't change the format of the
-- `versnum' line as it is `grep'ed for by a Makefile
--
idstr      = "$Id: Version.hs,v 1.2 2002/10/01 15:17:07 as49 Exp $"
name       = "C->Haskell Compiler"
versnum    = "0.10.17"
versnick   = "\"Altocumulus Stratiformis Perlucidus Undulatus\""
date	   = "07 Sep 2002"
version    = name ++ ", version " ++ versnum ++ " " ++ versnick ++ ", " ++ date
copyright  = "Copyright (c) [1999..2002] Manuel M T Chakravarty"
disclaimer = "This software is distributed under the \
	     \terms of the GNU Public Licence.\n\
	     \NO WARRANTY WHATSOEVER IS PROVIDED. \
	     \See the details in the documentation."
