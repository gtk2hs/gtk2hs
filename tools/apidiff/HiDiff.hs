
module Main where

import Data.Char as Char hiding (isSymbol)
import Data.List as List (nub, isPrefixOf, sortBy)
import Data.Maybe as Maybe (catMaybes)
import Control.Monad (unless)
import System.Environment (getArgs)

main = do
  [name, original, modified] <- getArgs
  diff name original modified

diff :: String -> FilePath -> FilePath -> IO ()
diff moduleName originalFile modifiedFile = do
  original <- readFile originalFile
  modified <- readFile modifiedFile
  let (added, removed, changed) =
        classify (extractFunctionsFromHi original)
                 (extractFunctionsFromHi modified)

--  putStrLn "Functions added:"
--  mapM_ putStrLn added

  unless (null removed && null changed) $
    putStrLn ("module " ++ moduleName)

  unless (null removed) $ do
    putStrLn " functions removed:"
    mapM_ (putStrLn . ("  "++)) removed
    putStrLn ""

  unless (null changed) $ do
    putStrLn " functions changed:"
    mapM_ putStrLn [ "  " ++ name ++ " changed from"
                          ++ "\n     :: " ++ originalType
                          ++ "\n  to :: " ++ modifiedType
                   | (name, originalType, modifiedType) <- changed ]
    putStrLn ""


classify :: [(String, String)] -> [(String, String)] -> ([String], [String], [(String, String, String)])
classify originalFuns modifiedFuns =
  ( [ originalName | (originalName, originalType) <- added ]
  , [ modifiedName | (modifiedName, modifiedType) <- removed
    , not $ "lvl" `isPrefixOf` modifiedName
    , not $ "gtk_" `isPrefixOf` modifiedName ]
  , [ (originalName, originalType, modifiedType)
    | ((originalName, originalType), (modifiedName, modifiedType)) <- changed
    ,    canonicaliseQuantifiedVars originalType
      /= canonicaliseQuantifiedVars modifiedType] )
  where (removed, changed, added) =
          mergeBy (comparing fst)
                  (sortBy (comparing fst) originalFuns)
                  (sortBy (comparing fst) modifiedFuns)


canonicaliseQuantifiedVars :: String -> String
canonicaliseQuantifiedVars ty =
  unwords [ case lookup w varMapping of
              Nothing -> w
              Just w' -> w'
          | w <- words ty ]
  where quantifiedVars = [ w | w@(c:_) <- words ty, Char.isLower c ]
        varMapping = zip (List.nub quantifiedVars) (map (\c -> [c]) ['a'..'z'])


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

comparing :: (Ord a) => (b -> a) -> b -> b -> Ordering
comparing p x y = compare (p x) (p y)


-- returns a list of function names with their types
extractFunctionsFromHi :: String -> [(String, String)]
extractFunctionsFromHi = catMaybes . map (eval . lexer) . init . parse


ignore = ["module", "__interface", "__export", "package", "orphans",
          ";", "$", "import", ":", "infixr", "infixl", "infix", "("]

eval :: [String] -> Maybe (String, String)
eval (x:_) | x `elem` ignore = Nothing


eval ("instance":ls) = Nothing
eval ("type":ls) = Nothing
eval ("data":ls) = Nothing
-- functions
eval ls@(name:"::":type_) = Just (name, respace (filterBraces type_))
eval xs = Nothing

filterBraces ("{":"}":xs) = filterBraces xs
filterBraces (x:xs) = x : filterBraces xs
filterBraces [] = []

respace :: [String] -> String
respace x = f (filter (/= ";") x)
    where
        f [] = ""
        f [x] = x
        f (x1:x2:xs) = if shouldspace x1 x2
                       then x1 ++ " " ++ f (x2:xs)
                       else x1 ++        f (x2:xs)

lBrack = "({["
rBrack = ")}]"

isRight [x] = x `elem` rBrack
isRight _ = False

isLeft [x] = x `elem` lBrack
isLeft _ = False


shouldspace l r = not $
    isRight r || isLeft l || r == ","


splitTerms :: [String] -> [[String]]
splitTerms xs@(x:_) | isLeft x = left : splitTerms (drop (length left) xs)
    where
        left = readBrack 0 xs
        
        readBrack 1 (x:xs) | isRight x = [x]
        readBrack n (x:xs) | isRight x = x : readBrack (n-1) xs
                           | isLeft  x = x : readBrack (n+1) xs
                           | otherwise = x : readBrack   n   xs

splitTerms (x:xs) = [x] : splitTerms xs
splitTerms [] = []
        
        

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x [] = []
splitOn x as = takeWhile (/= x) as : splitOn x (safeTail (dropWhile (/= x) as))


safeTail (x:xs) = xs
safeTail [] = []



parse :: String -> [String]
parse xs = rejoin (lines xs)
    where
        rejoin (x1:x2@(x2h:x2t):xs) | isSpace x2h = rejoin ((x1 ++ " " ++ dropWhile isSpace x2) : xs)
        rejoin (x:xs) = x : rejoin xs
        rejoin [] = []


lexer :: String -> [String]
lexer = demodule . lexRaw

demodule :: [String] -> [String]
demodule ("(":".":xs) = "(":".": demodule xs
demodule (x:".":xs) = demodule xs
demodule (x:xs) = x : demodule xs
demodule [] = []


-- Chunks taken from NHC's lexer, with modifications
lexRaw :: String -> [String]

lexRaw "" = []
lexRaw (x:xs) | isSpace x = lexRaw xs

lexRaw (x:xs) | x == '\'' || x == '\"' = f [x] xs
    where
        f done [] = [reverse done]
        f done ('\\':x:xs) = f (x:'\\':done) xs
        f done (a:xs) | a == x = reverse (a:done) : lexRaw xs
                      | otherwise = f (a:done) xs

lexRaw ('{':'-':x) = f x
    where
        f ('-':'}':x) = lexRaw x
        f (x:xs) = f xs
        f [] = []
        
lexRaw ('[':']':xs) = "[]" : lexRaw xs
lexRaw ('(':')':xs) = "()" : lexRaw xs

lexRaw ('(':x:xs) | isSymbol x && b == ')' = a : lexRaw bs
    where (a, b:bs) = span isSymbol (x:xs)

lexRaw (x:xs) | x `elem` ",;()[]{}`" = [x] : lexRaw xs
              | isDigit x = lexRaw xs -- drop digits, not needed -- continue isDigit
              | isSymbol x = continue isSymbol
              | isIdFirst x = continue isIdAny
    where
        isIdFirst c = isAlpha c || c == '_'
        isIdAny c = isAlphaNum c || c `elem` "_'#"
        
        continue f = a : lexRaw b
            where (a, b) = span f (x:xs)

isSymbol c = c `elem` "!@#$%&*+./<=>?\\^|:-~"

{-

lex (c:s) | isSingle c  = [([c],s)]
      | isSym c = [(c:sym,t)         | (sym,t) <- [span isSym s]]
      | isIdInit c  = [(c:nam,t)         | (nam,t) <- [span isIdChar s]]
      | isDigit c   = [(c:ds++fe,t)      | (ds,s)  <- [span isDigit s],
                           (fe,t)  <- lexFracExp s     ]
      | otherwise   = []    -- bad character

        where
        isSingle c  =  c `elem` ",;()[]{}`"
        isSym c     =  c `elem` "!@#$%&*+./<=>?\\^|:-~"
        isIdInit c  =  
        isIdChar c  =  isAlphaNum c || c `elem` "_'"

        lexFracExp ('.':c:s) | isDigit c
                                   = [('.':ds++e,u) | (ds,t) <- lexDigits (c:s),
                              (e,u)  <- lexExp t    ]
        lexFracExp s       = lexExp s

        lexExp (e:s) | e `elem` "eE"
             = [(e:c:ds,u) | (c:t)  <- [s], c `elem` "+-",
                           (ds,u) <- lexDigits t] ++
               [(e:ds,t)   | (ds,t) <- lexDigits s]
        lexExp s = [("",s)] 



lexRaw "" = []
lexRaw (' ':x) = lexRaw x
lexRaw (';':x) = lexRaw x

-- to make up for Hugs being wrong
lexRaw ('_':x) = lexRaw x

lexRaw ('{':'-':x) = f x
    where
        f ('-':'}':x) = lexRaw x
        f (x:xs) = f xs
        f [] = []

lexRaw x  = a : lexRaw b
    where [(a, b)] = lex x
-}
