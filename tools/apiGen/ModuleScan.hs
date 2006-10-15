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
    module_needsc2hs         :: Bool,
    module_filename          :: String,
    module_authors           :: [String],
    module_created           :: String,
    module_rcs_version       :: String,
    module_rcs_timestamp     :: String,
    module_copyright_dates   :: Either String (String, String),
                                -- eg "2004" or "2004-2005"
    module_copyright_holders :: [String],
    module_exports           :: [String],
    module_imports           :: [(String, String)], -- mod name and the whole line
    module_context_lib       :: String,
    module_context_prefix    :: String,
    module_methods           :: [MethodInfo],
    module_deprecated        :: [String]      -- {-# DEPRECATED #-} pragmas
  } deriving Show

data MethodInfo = MethodInfo {
    methodinfo_cname :: String,       -- the full gtk_foo_bar
    methodinfo_shortcname :: String,  -- just foo_bar
    methodinfo_unsafe :: Bool    -- {#call unsafe foo#} rather than {#call foo#}
  } deriving Show

data Line = None
          | Authors [String]
          | Created String
	  | Version String String String String
          | Copyright (Either String (String, String)) [String]
          | Module String String
          | Export String
          | ExportEnd
          | Import String String
          | Context String String
          | CCall MethodInfo
          | Deprecated String
  deriving Show

usefulLine None = False
usefulLine _    = True

isModuleLine (Module _ _) = True
isModuleLine _            = False
isExportEndLine ExportEnd = True
isExportEndLine _         = False
isCCallLine (CCall _) = True
isCCallLine _         = False

main = do
  [path] <- getArgs
  modules <- findModules [] path
  modInfos <- mapM (\moduleName -> do ppExists <- doesFileExist (moduleName ++ ".chs.pp")
                                      if ppExists then scanModule (moduleName ++ ".chs.pp")
                                                  else scanModule (moduleName ++ ".chs")) modules
  print modInfos

scanModules :: FilePath -> [FilePath] -> IO [ModuleInfo]
scanModules path excludePaths = do
  modules <- findModules excludePaths path
  mapM (\moduleName -> do ppExists <- doesFileExist (moduleName ++ ".chs.pp")
                          chsExists <- doesFileExist (moduleName ++ ".chs")
                          if ppExists
                            then scanModule (moduleName ++ ".chs.pp")
                            else if chsExists
                                   then scanModule (moduleName ++ ".chs")
                                   else scanModule (moduleName ++ ".hs")
       ) modules

findModules :: [FilePath] -> FilePath -> IO [FilePath]
findModules excludePaths path | path `elem` excludePaths = return []
findModules excludePaths path = do
  files <- getDirectoryContents path
  let (chsFiles, maybeDirs) = partition (\file -> ".chs.pp" `isSuffixOf` file
                                               || ".chs"    `isSuffixOf` file
--                                               || ".hs.pp"  `isSuffixOf` file
                                               || ".hs"     `isSuffixOf` file) files
      modules = map head
              . group
              . sort
              . map extractModule
              $ chsFiles
      extractModule [] = []
      extractModule ('.':'h':'s':[]) = []
--      extractModule ('.':'h':'s':'.':'p':'p':[]) = []
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
  subDirModules <- mapM (findModules excludePaths) dirs
  return $ map ((path++"/")++) modules ++ concat subDirModules

scanModule :: FilePath -> IO ModuleInfo
scanModule file = do
  content <- readFile file
  let moduleInfo = scanModuleContent content file
  return moduleInfo {
      module_filename = moduleNameToFileName (module_name moduleInfo)
                                             (module_prefix moduleInfo)
                                             (module_needspreproc moduleInfo)
                                             (module_needsc2hs moduleInfo)
    }

scanModuleContent :: String -> String -> ModuleInfo
scanModuleContent content filename =
  let (headerLines, bodyLines) =
         break isCCallLine
       . filter usefulLine
       $ [ scanLine line (tokenise line) | line <- lines content ]
  in ModuleInfo {
    module_name              = head $ [ name    | Module name prefix  <- headerLines ] ++ [missing],
    module_prefix            = head $ [ prefix  | Module name prefix  <- headerLines ] ++ [missing],
    module_needspreproc      = ".chs.pp" `isSuffixOf` filename,
    module_needsc2hs         = ".chs" `isSuffixOf` filename,
    module_filename          = "",
    module_authors           = head $ [ authors | Authors authors     <- headerLines ] ++ [[missing]],
    module_created           = head $ [ created | Created created     <- headerLines ] ++ [missing],
    module_rcs_version       = head $ [ major ++ "." ++ minor
                                      | Version major minor _ _       <- headerLines ] ++ [""],
    module_rcs_timestamp     = head $ [ date ++ " " ++ time
                                      | Version _ _ date time         <- headerLines ] ++ [""],
    module_copyright_dates   = head $ [ dates   | Copyright dates _   <- headerLines ] ++ [Left missing],
    module_copyright_holders = head $ [ authors | Copyright _ authors <- headerLines ] ++ [[missing]],
    module_exports           = let exportLines = takeWhile (not.isExportEndLine)
                                               . dropWhile (not.isModuleLine)
                                               $ headerLines
                                in [ name       | Export name         <- exportLines ],
    module_imports           = [ (name, line)   | Import name line    <- headerLines ],
    module_context_lib       = head $ [ lib     | Context lib prefix  <- headerLines ] ++ [missing],
    module_context_prefix    = head $ [ prefix  | Context lib prefix  <- headerLines ] ++ [missing],
    module_methods           =        [ call    | CCall call  <- bodyLines ],
    module_deprecated        =        [ value   | Deprecated value    <- bodyLines ]
  }
  where missing = "{-missing-}"

moduleNameToFileName :: String -> String -> Bool -> Bool -> String
moduleNameToFileName name prefix preproc c2hs =
  map dotToSlash prefix ++ "/" ++ name
  ++ if preproc then ".chs.pp" else if c2hs then ".chs" else ".hs"
  where dotToSlash '.' = '/'
        dotToSlash  c  =  c

scanLine :: String -> [String] -> Line
scanLine _ ("--":"Author":":":author)   = scanAuthor author
scanLine _ ("--":"Created:":created)    = Created (unwords created)
scanLine _ ["--","Version",_,major,".",minor,_,_,_,date,time,_] = Version major minor date time
scanLine _ ("--":"Copyright":"(":c:")":copyright) = scanCopyright copyright
scanLine (' ':' ':_) ("module":moduleName) = Export (concat moduleName)
scanLine _ ("module":moduleName)        = scanModuleName moduleName
scanLine (' ':' ':_) (export:",":[])    = Export export
scanLine (' ':' ':_) (export:",":"--":_)= Export export
scanLine (' ':' ':_) (export:[])        = Export export
scanLine _ (")":"where":[])             = ExportEnd
scanLine _ ("{#":"context":context)     = scanContext context
scanLine line ("import":moduleName)     = scanImport line moduleName
scanLine line ("{#":"import":moduleName)= scanImport line moduleName
scanLine _ ("{#-":"DEPRECATED":symbol:_)= Deprecated symbol
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
scanModuleName line | (moduleName:".":modulePrefix) <- reverse line =
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
    ("call":"pure":"unsafe":cname:[]) -> CCall MethodInfo { methodinfo_cname = cname,
                                                            methodinfo_shortcname = cname,
                                                            methodinfo_unsafe = True }
    ("call":"unsafe":cname:[]) -> CCall MethodInfo { methodinfo_cname = cname,
                                                     methodinfo_shortcname = cname,
                                                     methodinfo_unsafe = True }
    ("call":         cname:[]) -> CCall MethodInfo { methodinfo_cname = cname,
                                                     methodinfo_shortcname = cname,
                                                     methodinfo_unsafe = False }
    ("call":"fun":"unsafe":cname:[]) -> CCall MethodInfo { methodinfo_cname = cname,
                                                           methodinfo_shortcname = cname,
                                                           methodinfo_unsafe = True }
    ("fun":"pure":_)           -> None
    ("type":_)                 -> None
    ("pointer":_)              -> None
    ("pointer*":_)             -> None
    ("enum":_)                 -> None
    ("set":_)                  -> None
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
