{-# OPTIONS -fglasgow-exts #-}

module ModuleScan (
  ModuleInfo(..),
  MethodInfo(..),
  scanModules
  ) where

import StringUtils (splitOn)

import Char (isSpace, isAlpha)
import List (intersperse, partition, isSuffixOf, group, sort)
import Prelude hiding (unwords)

import System (getArgs)
import Directory (getDirectoryContents, doesDirectoryExist, doesFileExist)

data ModuleInfo = ModuleInfo {
    module_name              :: String,
    module_prefix            :: String,
    module_needspreproc      :: Bool,
    module_filename          :: String,
    module_authors           :: [String],
    module_created           :: String,
    module_copyright_dates   :: Either String (String, String),
                                -- eg "2004" or "2004-2005"
    module_copyright_holders :: [String],
    module_imports           :: [(String, String)], -- mod name and the whole line
    module_context_lib       :: String,
    module_context_prefix    :: String,
    module_methods           :: [MethodInfo]
  } deriving Show

data MethodInfo = MethodInfo {
    methodinfo_cname :: String,
    methodinfo_unsafe :: Bool    -- {#call unsafe foo#} rather than {#call foo#}
  } deriving Show

data Line = None
          | Authors [String]
          | Created String
          | Copyright (Either String (String, String)) [String]
          | Module String String
          | Import String String
          | Context String String
          | CCall MethodInfo

usefulLine None = False
usefulLine _    = True

main = do
  [path] <- getArgs
  modules <- findModules path
  modInfos <- mapM (\moduleName -> do ppExists <- doesFileExist (moduleName ++ ".chs.pp")
                                      if ppExists then scanModule (moduleName ++ ".chs.pp")
                                                  else scanModule (moduleName ++ ".chs")) modules
  print modInfos

scanModules :: FilePath -> IO [ModuleInfo]
scanModules path = do
  modules <- findModules path
  mapM (\moduleName -> do ppExists <- doesFileExist (moduleName ++ ".chs.pp")
                          if ppExists then scanModule (moduleName ++ ".chs.pp")
                                      else scanModule (moduleName ++ ".chs")) modules

findModules :: FilePath -> IO [FilePath]
findModules path = do
  files <- getDirectoryContents path
  let (chsFiles, maybeDirs) = partition (\file -> ".chs" `isSuffixOf` file
                                            || ".chs.pp" `isSuffixOf` file) files
      modules = map head
              . group
              . sort
              . map extractModule
              $ chsFiles
      extractModule [] = []
      extractModule ('.':'c':'h':'s':[]) = []
      extractModule ('.':'c':'h':'s':'.':'p':'p':[]) = []
      extractModule (c:cs) = c : extractModule cs
  dirs <- let filterDirs ds [] = return (reverse ds)
              filterDirs ds (md:mds) = do
                isDir <- doesDirectoryExist md
                if isDir then filterDirs (md:ds) mds
                         else filterDirs     ds  mds
           in filterDirs [] [ path ++ "/" ++ maybeDir
                            | maybeDir <- maybeDirs, maybeDir /= ".", maybeDir /= ".."]
  subDirModules <- mapM findModules dirs
  return $ map ((path++"/")++) modules ++ concat subDirModules

scanModule :: FilePath -> IO ModuleInfo
scanModule file = do
  content <- readFile file
  let moduleInfo = scanModuleContent content file
  return moduleInfo {
      module_filename = moduleNameToFileName (module_name moduleInfo)
                                             (module_prefix moduleInfo)
                                             (module_needspreproc moduleInfo)
    }

scanModuleContent :: String -> String -> ModuleInfo
scanModuleContent content filename =
  let usefulLines = filter usefulLine [ scanLine line (tokenise line) | line <- lines content ] in
  ModuleInfo {
    module_name              = head $ [ name    | Module name prefix  <- usefulLines ] ++ [missing],
    module_prefix            = head $ [ prefix  | Module name prefix  <- usefulLines ] ++ [missing],
    module_needspreproc      = ".chs.pp" `isSuffixOf` filename,
    module_filename          = "",
    module_authors           = head $ [ authors | Authors authors     <- usefulLines ] ++ [[missing]],
    module_created           = head $ [ created | Created created     <- usefulLines ] ++ [missing],
    module_copyright_dates   = head $ [ dates   | Copyright dates _   <- usefulLines ] ++ [Left missing],
    module_copyright_holders = head $ [ authors | Copyright _ authors <- usefulLines ] ++ [[missing]],
    module_imports           = [ (name, line)   | Import name line    <- usefulLines ],
    module_context_lib       = head $ [ lib     | Context lib prefix  <- usefulLines ] ++ [missing],
    module_context_prefix    = head $ [ prefix  | Context lib prefix  <- usefulLines ] ++ [missing],
    module_methods           =        [ call    | CCall call  <- usefulLines ]
  }
  where missing = "{-missing-}"

moduleNameToFileName :: String -> String -> Bool -> String
moduleNameToFileName name prefix preproc  = map dotToSlash prefix ++ "/" ++ name
                                         ++ if preproc then ".chs.pp" else ".chs"
  where dotToSlash '.' = '/'
        dotToSlash  c  =  c

scanLine :: String -> [String] -> Line
scanLine _ ("--":"Author":":":author)   = scanAuthor author
scanLine _ ("--":"Created:":created)    = Created (unwords created)
scanLine _ ("--":"Copyright":"(":c:")":copyright) = scanCopyright copyright
scanLine _ ("module":moduleName)        = scanModuleName moduleName
scanLine _ ("{#":"context":context)     = scanContext context
scanLine line ("import":moduleName)      = scanImport line moduleName
scanLine line ("{#":"import":moduleName) = scanImport line moduleName
scanLine _ tokens | "{#" `elem` tokens  = scanCCall tokens

scanLine _ _ = None

scanAuthor :: [String] -> Line
scanAuthor = 
    Authors
  . map unwords
  . splitOn ","

scanCopyright :: [String] -> Line
scanCopyright (from:"..":to:name)         = Copyright (Right (from, to)) (map unwords $ splitOn "," name)
scanCopyright (from:"-":to:name)          = Copyright (Right (from, to)) (map unwords $ splitOn "," name)
scanCopyright ("[":from:"..":to:"]":name) = Copyright (Right (from, to)) (map unwords $ splitOn "," name)
scanCopyright (year:name)                 = Copyright (Left year)        (map unwords $ splitOn "," name)
scanCopyright line = error $ "scanCopyright: " ++ show line

scanModuleName :: [String] -> Line
scanModuleName line | ("(":moduleName:".":modulePrefix) <- reverse line =
  Module moduleName (concat (reverse modulePrefix))
scanModuleName line | ("where":")":_:"(":moduleName:".":modulePrefix) <- reverse line =
  Module moduleName (concat (reverse modulePrefix))
scanModuleName ("Graphics":".":"UI":".":"Gtk":".":"Gdk":".":"Enums":[]) = None
scanModuleName line = error $ "scanModuleName: " ++ show line

scanContext :: [String] -> Line
scanContext ("lib":"=\"":lib:"\"":"prefix":"=\"":prefix:"\"":"#}":[]) = Context lib prefix
scanContext ("lib":"=\"":lib:"\"":"prefix":"=\"":prefix:"\"#}":[])    = Context lib prefix
scanContext ("prefix":"=\"":prefix:"\"":"#}":[])                         = Context "" prefix
scanContext line = error $ "scanContext: " ++ show line

scanImport :: String -> [String] -> Line
scanImport line tokens = Import (concat $ takeWhile (\token -> isWord token || token == ".") tokens) line
  where isWord = all isAlpha

scanCCall :: [String] -> Line
scanCCall tokens =
  case takeWhile (\t -> t/="#}" && t/="#}."&& t/="#})") . tail . dropWhile (/="{#") $ tokens of
    ("call":"unsafe":cname:[]) -> CCall MethodInfo { methodinfo_cname = cname,
                                                     methodinfo_unsafe = True }
    ("call":         cname:[]) -> CCall MethodInfo { methodinfo_cname = cname,
                                                     methodinfo_unsafe = False }
    ("call":"fun":"unsafe":cname:[]) -> CCall MethodInfo { methodinfo_cname = cname,
                                                           methodinfo_unsafe = True }
    ("fun":"pure":_)           -> None
    ("type":_)                 -> None
    ("pointer":_)              -> None
    ("pointer*":_)             -> None
    ("enum":_)                 -> None
    ("get":_)                  -> None
    ("sizeof":_)                  -> None
    tokens -> error $ "scanCCall: " ++ show tokens

tokenise :: String -> [String]
tokenise s = case dropWhile isSpace s of
               "" -> []
               s' -> case span isBoundary s' of
                       ("", s'') -> case break isSpaceOrBoundary s'' of
                                     (w,s''') -> w : tokenise s'''
                       (w, s'') -> w : tokenise s''
  where isBoundary c = c `elem` ".,[]{}#()-=\""
        isSpaceOrBoundary c = isSpace c || isBoundary c

unwords   :: [String] -> String
unwords []       =  ""
unwords [w]      = w
unwords (w:".":ws) = w ++ ". " ++ unwords ws
unwords (w:ws)   = w ++ ' ' : unwords ws
