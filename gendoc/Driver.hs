module Main(main) where

import Parser
import State
import Update	(addImport)
import XMLwrite
import System.Exit
import System.Environment
import System.Console.GetOpt
import Data.Set
import Data.FiniteMap
import Data.PackedString


main = do
  args <- getArgs
  case getOpt Permute options args of
    (transs, files, []) -> do
      let state = foldl (.) id 
		  (transs++map (addImport.dropExt.dropPath) files)
		  initialState
      state' <- processFiles state
      writeContent state'
    (_, _, errs) -> fatal errs

fatal :: [String] -> IO ()
fatal errs = do
  pn <- getProgName
  let header = "usage: "++pn++" <OPTIONS> <module>[.hs]"
  putStr (concat errs++usageInfo header options)
  exitWith (ExitFailure 1)

options :: [OptDescr (State -> State)]
options = [
  Option "I" [] (OptArg setPath "PATH") "colon-separated search path",
  Option "x" ["exclude"] (OptArg setExclude "FILE") "Haskell module to ignore",
  Option "o" ["output"] (ReqArg (\txt s -> s { outFile = txt }) "OUTPUT")
			"Output file."
  ]

-- Set or reset the search path in the State of the program.
--
setPath :: Maybe String -> State -> State
setPath Nothing s = s { inclPath=emptySet }
setPath (Just p) (s@State { inclPath=ip }) = 
  s { inclPath=union ip (mkSet $ map dropSlash $ extractPaths p) }
  where
    extractPaths :: String -> [String]
    extractPaths = eP ""
    eP :: String -> String -> [String]
    eP path (':':str) = path: eP "" str
    eP path (s:tr) = eP (path++[s]) tr
    eP "" [] = []
    eP path [] = [path]
    dropSlash "" = ""
    dropSlash "/" = ""
    dropSlash "\\" = ""
    dropSlash (s:tr) = s:dropSlash tr

-- Remove the extension from the Haskell file name.
--
dropExt :: FilePath -> HsModule
dropExt = reverse.dE.reverse
  where
    dE ('s':'h':'.':rem) = rem
    dE ('s':'h':'c':'.':rem) = rem
    dE ('c':'s':'h':'.':rem) = rem
    dE rem = rem

-- Remove the path in front of a filename.
--
dropPath :: FilePath -> HsModule
dropPath = dP ""
  where
    dP fname ('/':tr) = dP "" tr
    dP fname (s:tr) = dP (fname++[s]) tr
    dP fname [] = fname

-- Add a name to the set of Haskell modules which are not parsed.
--
setExclude :: Maybe String -> State -> State
setExclude Nothing s = s { filesExcl = emptySet }
setExclude (Just fname) s = s { filesExcl = filesExcl s `addToSet`
					    dropExt (dropPath fname) }

writeContent :: State -> IO ()
writeContent (state@State { modTab=mt, outFile=ot }) = do
  putStrLn ("\nWriting output to "++ot)
  writeFile ot (show (map (uncurry moduleToXML) (fmToList mt)))
