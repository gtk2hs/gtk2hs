module StringUtils where

import Prelude hiding (lines)
import Char   (toLower, toUpper, isSpace, isAlpha, isAlphaNum, isUpper)
import List   (unfoldr)

-------------------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------------------

ss = showString
sc = showChar

nl = sc '\n'

indent :: Int -> ShowS
indent c = ss ("\n"++replicate (2*c) ' ')

comment :: ShowS
comment = ss "-- "

lowerCaseFirstChar :: String -> String
lowerCaseFirstChar (c:cs) = toLower c : cs

upperCaseFirstChar :: String -> String
upperCaseFirstChar (c:cs) = toUpper c : cs

cat :: [ShowS] -> ShowS
cat = foldl (.) id

lines :: [ShowS] -> ShowS
lines []     = id
lines [x]    = x
lines (x:xs) = x. sc '\n'. lines xs

sepBy :: String -> [String] -> ShowS
sepBy s []     = id
sepBy s [x]    = ss x
sepBy s (x:xs) = ss x. ss s. sepBy s xs

sepBy' :: String -> [ShowS] -> ShowS
sepBy' s []     = id
sepBy' s [x]    = x
sepBy' s (x:xs) = x. ss s. sepBy' s xs

templateSubstitute :: String -> (String -> ShowS) -> ShowS
templateSubstitute template varSubst = doSubst template
  where doSubst [] = id
        doSubst ('\\':'@':cs) = sc '@' . doSubst cs
        doSubst ('@':cs) = let (var,_:cs') = span ('@'/=) cs
                            in varSubst var . doSubst cs'
        doSubst (c:cs) = sc c . doSubst cs

splitBy :: Char -> String -> [String]
splitBy sep str =
  case span (sep/=) str of
    (remainder,[]) -> [remainder]
    (word,_:remainder) -> word : splitBy sep remainder

-- wraps a list of words to lines of words
wrapText :: Int -> Int -> [String] -> [[String]]
wrapText initialCol width = wrap initialCol []
  
  where wrap :: Int -> [String] -> [String] -> [[String]]
        wrap 0   []   (word:words) |       length word + 1 > width = wrap (length word) [word] words
        wrap col line (word:words) | col + length word + 1 > width = reverse line : wrap 0 [] (word:words)
        wrap col line (word:words) = wrap (col + length word + 1) (word:line) words
        wrap _ []   [] = []
        wrap _ line [] = [reverse line]

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn sep = 
  unfoldr (\s -> case break (sep==) s of
                   ([],_) -> Nothing
                   (w,_:r) -> Just (w,r)
                   (w,[]) -> Just (w,[]))

-- mergeBy cmp xs ys = (only_in_xs, in_both, only_in_ys)
mergeBy :: (a -> b -> Ordering) -> [a] -> [b] -> ([a], [(a, b)], [b])
mergeBy cmp = merge [] [] []
  where merge l m r []     ys     = (reverse l, reverse m, reverse (ys++r))
        merge l m r xs     []     = (reverse (xs++l), reverse m, reverse r)
        merge l m r (x:xs) (y:ys) = 
          case x `cmp` y of
            GT -> merge    l         m  (y:r) (x:xs)    ys
            EQ -> merge    l  ((x,y):m)    r     xs     ys
            LT -> merge (x:l)        m     r     xs  (y:ys)
