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
matcher spec = match
  where excludeRegex       = mkRegex $ concat $ intersperse "|" [ regex | Exclude       regex <- spec ]
        noExcludeRegex     = mkRegex $ concat $ intersperse "|" [ regex | NotExclude    regex <- spec ]
        alwaysExcludeRegex = mkRegex $ concat $ intersperse "|" [ regex | AlwaysExclude regex <- spec ]
        match line = not $ ((isJust $ matchRegex excludeRegex line)
                        && (not $ isJust $ matchRegex noExcludeRegex line))
                        || (isJust $ matchRegex alwaysExcludeRegex line)
