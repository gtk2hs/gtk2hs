--  The HiPar Toolkit: generates unique names
--
--  Author : Manuel M T Chakravarty
--  Created: 3 April 98
--
--  Version $Revision: 1.2 $ from $Date: 2004/11/13 17:26:50 $
--
--  Copyright (C) [1998..2003] Manuel M T Chakravarty
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
--  Generates unqiue names according to a method of L. Augustsson, M. Rittri
--  & D. Synek ``Functional pearl: On generating unique names'', Journal of
--  Functional Programming 4(1), pp 117-123, 1994.
--
--  WARNING: DON'T tinker with the implementation!  It uses UNSAFE low-level 
--	     operations!
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--  * This module provides an ordering relation on names (e.g., for using
--    `Maps'), but no assumption maybe made on the order in which names
--    are generated from the name space.  Furthermore, names are instances of
--    `Ix' to allow to use them as indicies.
--
--  * A supply should be used *at most* once to *either* split it or extract a 
--    stream of names.  A supply used repeatedly will always generate the same
--    set of names (otherwise, the whole thing wouldn't be referential
--    transparent).  
--
--  * If you ignored the warning below, looked at the implementation, and lost
--    faith, consider that laziness means call-by-need *and* sharing, and that
--    sharing is realized by updating evaluated thunks.
--
--  * ATTENTION: No clever CSE or unnecessary argument elimination may be
--    applied to the function `names'!
--
--- TODO
--

module UNames (NameSupply, Name,
	       rootSupply, splitSupply, names,
	       saveRootNameSupply, restoreRootNameSupply)
where

import Monad  (when)
import Ix
import SysDep (IORef, readIORef, writeIORef,
               unsafeNewIntRef, unsafeReadAndIncIntRef)
import Binary (Binary(..))


-- Name supply definition (EXPORTED ABSTRACTLY)
--
newtype NameSupply = NameSupply (IORef Int)

-- Name (EXPORTED ABSTRACTLY)
--
newtype Name = Name Int
--             deriving (Show, Eq, Ord, Ix)
-- FIXME: nhc98, v1.08 can't derive Ix
             deriving (Eq, Ord)
instance Ix Name where
  range   (Name from, Name to)            = map Name (range (from, to))
  index   (Name from, Name to) (Name idx) = index   (from, to) idx
  inRange (Name from, Name to) (Name idx) = inRange (from, to) idx

-- we want to show the number only, to be useful for generating unqiue
-- printable names
--
instance Show Name where
  show (Name i) = show i


--	  	      *** DON'T TOUCH THE FOLLOWING *** 
--  and if you believe in the lambda calculus better also don't look at it
--          ! here lives the daemon of unordered destructive updates !

-- The initial supply (EXPORTED)
--
rootSupply :: NameSupply
{-# NOINLINE rootSupply #-}
rootSupply  = NameSupply (unsafeNewIntRef 1)

-- Split a name supply into a stream of supplies (EXPORTED)
--
splitSupply   :: NameSupply -> [NameSupply]
splitSupply s  = repeat s

-- Given a name supply, yield a stream of names (EXPORTED)
--
names                :: NameSupply -> [Name]
--
--  The recursion of `theNames' where `s' is passed as an argument is crucial, 
--  as it forces the creation of a new closure for `unsafeReadAndIncIntRef s'
--  in each recursion step.  Sharing a single closure or building a cyclic
--  graph for a nullary `theNames' would always result in the same name!  If
--  the compiler ever gets clever enough to optimize this, we have to prevent
--  it from doing so.
--
names (NameSupply s)  = 
  theNames s
  where
    theNames s = Name (unsafeReadAndIncIntRef s) : theNames s

-- Actions for saving and restoring the state of the whole program. (EXPORTED)
-- The rules for these functions are as follows:
--   you must not use the root name supply after saving it
--   you must not use the root namue supply before restoring it
-- Otherwise bad things will happen, your unique Ids will no longer be unique
saveRootNameSupply :: IO Name
saveRootNameSupply =
  case rootSupply of
    NameSupply ref -> do
      val <- readIORef ref
      writeIORef ref (error "UName: root name supply used after saving")
      return (Name val)

restoreRootNameSupply :: Name -> IO ()
restoreRootNameSupply (Name val) = 
  case rootSupply of
    NameSupply ref -> do
      prev <- readIORef ref
      when (prev /= 1) (error "UName: root name supply used before restoring")
      writeIORef ref val


{-! for Name derive : GhcBinary !-}
{-* Generated by DrIFT : Look, but Don't Touch. *-}
instance Binary Name where
    put_ bh (Name aa) = do
            put_ bh aa
    get bh = do
    aa <- get bh
    return (Name aa)
