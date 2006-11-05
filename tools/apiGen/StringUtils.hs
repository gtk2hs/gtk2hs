module StringUtils where

import Prelude hiding (lines, words)
import Data.Char (toLower, toUpper)
import Data.List (unfoldr)

-------------------------------------------------------------------------------
-- ShowS functions
-------------------------------------------------------------------------------
ss :: String -> ShowS
ss = showString

sc :: Char -> ShowS
sc = showChar

nl :: ShowS
nl = sc '\n'

indent :: Int -> ShowS
indent c = ss ("\n"++replicate (2*c) ' ')

comment :: ShowS
comment = ss "-- "

cat :: [ShowS] -> ShowS
cat = foldl (.) id

lines :: [ShowS] -> ShowS
lines []     = id
lines [x]    = x
lines (x:xs) = x. sc '\n'. lines xs

sepBy :: String -> [String] -> ShowS
sepBy _ []     = id
sepBy _ [x]    = ss x
sepBy s (x:xs) = ss x. ss s. sepBy s xs

sepBy' :: String -> [ShowS] -> ShowS
sepBy' _ []     = id
sepBy' _ [x]    = x
sepBy' s (x:xs) = x. ss s. sepBy' s xs

templateSubstitute :: String -> (String -> ShowS) -> ShowS
templateSubstitute template varSubst = doSubst template
  where doSubst [] = id
        doSubst ('\\':'@':cs) = sc '@' . doSubst cs
        doSubst ('@':cs) = let (var,_:cs') = span ('@'/=) cs
                            in varSubst var . doSubst cs'
        doSubst (c:cs) = sc c . doSubst cs
