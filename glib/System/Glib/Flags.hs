-- -*-haskell-*-
--  class of flag types
--
--  Author : Duncan Coutts
--
--  Created: 21 January 2005
--
--  Copyright (C) 2001-2005 Duncan Coutts, Axel Simon
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable
--
-- This module defines a type class for flags that are marshaled as bitflags.
--
module System.Glib.Flags (
  Flags,
  fromFlags,
  toFlags
  ) where

import Data.Bits ((.|.), (.&.), testBit, shiftL, shiftR)
import Data.Maybe (catMaybes)

class  (Enum a, Bounded a) => Flags a

fromFlags :: Flags a => [a] -> Int
fromFlags is = orNum 0 is
  where orNum n []     = n
        orNum n (i:is) = orNum (n .|. fromEnum i) is

-- * Note that this function ignores bits set in the passed
--   'Int' that do not correspond to a flag.
toFlags :: Flags a => Int -> [a]
toFlags n = catMaybes [ if n .&. fromEnum flag == fromEnum flag
                           then Just flag
                           else Nothing
                      | flag <- [minBound .. maxBound] ]

-------------------------
-- QuickCheck test code

{-
import Test.QuickCheck
import List (sort, nub)
-- to run these tests you must copy EventMask and its Enum instance here
-- and make it an instance of Ord, Eq and Show.

prop_ToFlagsFromFlags :: Int -> Property
prop_ToFlagsFromFlags n =
  (n >= 1 && n <= 21)
  ==>
  collect n $
  let flag :: [EventMask]
      flag = toFlags (2^n)
   in 2^n == fromFlags flag

prop_FromFlagsToFlags :: [EventMask] -> Bool
prop_FromFlagsToFlags flags =
  (nub . sort) flags == toFlags (fromFlags flags)

instance Arbitrary EventMask where
  arbitrary     = sized $ \_ -> do x <- choose (1,21 :: Int)
                                   return (toEnum $ 2^x)
-}
