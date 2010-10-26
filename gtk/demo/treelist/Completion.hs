-- Demo to show off entry completion.

import Graphics.UI.Gtk
import Data.Char
import Data.List

data ColorDesc = ColorDesc {
 cdColor :: Color,
 cdName :: String
 } deriving Show

compareCol :: ColumnId ColorDesc String
compareCol = makeColumnIdString 0

invertColor (Color r g b) = Color (32767+r) (32767+g) (32767+b)
--Color (65535-r) (65535-g) (65535-b)

parseColor s = ColorDesc c (dropWhile isSpace (upperToSpace name))
  where
  s1 = dropWhile isSpace s
  (s2,s3) = span isDigit s1
  s4 = dropWhile isSpace s3
  (s5,s6) = span isDigit s4
  s7 = dropWhile isSpace s6
  (s8,s9) = span isDigit s7
  n1 = read ('0':s2)
  n2 = read ('0':s5)
  n3 = read ('0':s8)
  c = Color (n1*256+n1) (n2*256+n2) (n3*256+n3)
  name = dropWhile isSpace s9
  upperToSpace [] = []
  upperToSpace (x:xs) | isUpper x = ' ':toLower x:upperToSpace xs
                      | otherwise = x:upperToSpace xs

main =
    do
      initGUI
      window <- windowNew

      contents <- readFile "rgb.txt"
      let killDups [] = []
          killDups [x] = [x]
          killDups (x:y:xs) | cdName x==cdName y = killDups (y:xs)
                            | otherwise = x:killDups (y:xs)
          cols = killDups $ map parseColor (drop 1 (lines contents))
      store <- listStoreNew cols
      customStoreSetColumn store compareCol cdName

      entry <- entryNew
      completion <- entryCompletionNew
      entrySetCompletion entry completion

      set completion [entryCompletionModel := Just store]
      cell <- cellRendererTextNew
      set cell [cellTextBackgroundSet := True,
                cellTextForegroundSet := True]
      cellLayoutPackStart completion cell True
      cellLayoutSetAttributes completion cell store
        (\cd -> [cellText := cdName cd,
                 cellTextBackgroundColor := cdColor cd,
                 cellTextForegroundColor := invertColor (cdColor cd)])
      entryCompletionSetMatchFunc completion (matchFunc store)
      on completion matchSelected $ \model iter -> do
        color <- treeModelGetValue model iter compareCol
        entrySetText entry color
        return True
      set window [containerChild := entry]
      widgetShowAll window
      onDestroy window mainQuit
      mainGUI

matchFunc :: ListStore ColorDesc -> String -> TreeIter -> IO Bool
matchFunc model str iter = do
  --putStrLn ("iter is "++show iter)
  tp <- treeModelGetPath model iter
  r <- case tp of
         (i:_) -> do row <- listStoreGetValue model i
                     return $ any (isPrefixOf (map toLower str))
                                  (words (map toLower (cdName row)))
         otherwise -> return False
  return r
