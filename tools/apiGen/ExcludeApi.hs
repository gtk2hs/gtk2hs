module ExcludeApi (
  parseFilterFile,
  matcher
  ) where

import Char  (isSpace)
import Maybe (catMaybes, isJust)
import List  (isPrefixOf, intersperse)
import System (getArgs)
import Text.Regex

data FilterSpec = Exclude String
                | NotExclude String     -- override Exclude but not AlwaysExclude
                | AlwaysExclude String

parseFilterFile :: String -> [FilterSpec]
parseFilterFile = catMaybes . map parseLine . lines
  where parseLine []      = Nothing
        parseLine ('#':_) = Nothing
        parseLine line
          | "exclude " `isPrefixOf` line        = Just $ Exclude       $ trim $ drop 8  line
          | "do not exclude " `isPrefixOf` line = Just $ NotExclude    $ trim $ drop 15 line
          | "always exclude " `isPrefixOf` line = Just $ AlwaysExclude $ trim $ drop 15 line
        parseLine line = error $ "cannot parse line: " ++ line
        trim = takeWhile (not . isSpace) . dropWhile isSpace

matcher :: [FilterSpec] -> (String -> Bool)
matcher spec line = match line
  where excludeRegexFragments       = [ regex | Exclude       regex <- spec ]
        noExcludeRegexFragments     = [ regex | NotExclude    regex <- spec ]
        alwaysExcludeRegexFragments = [ regex | AlwaysExclude regex <- spec ]
  
        excludeRegex       = mkRegex $ concat $ intersperse "|" excludeRegexFragments
        noExcludeRegex     = mkRegex $ concat $ intersperse "|" noExcludeRegexFragments
        alwaysExcludeRegex = mkRegex $ concat $ intersperse "|" alwaysExcludeRegexFragments
        
        matchExclude       line = isJust (matchRegex excludeRegex line) && not (null excludeRegexFragments)
        matchNotExclude    line = isJust (matchRegex noExcludeRegex line) && not (null noExcludeRegexFragments)
        matchAlwaysExclude line = isJust (matchRegex alwaysExcludeRegex line) && not (null alwaysExcludeRegexFragments)
        
        match line = not $ (matchExclude line && (not $ matchNotExclude line))
                        || matchAlwaysExclude line
