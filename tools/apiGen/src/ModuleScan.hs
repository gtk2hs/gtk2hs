{-# OPTIONS -fglasgow-exts #-}

module ModuleScan (
  ModuleInfo(..),
  FuncInfo(..),
  CCallInfo(..),
  scanModules
  ) where

import Utils (splitBy, splitOn)

import Data.Char (isSpace, isAlpha)
import Data.List (partition, isSuffixOf, group, sort)
import Data.Maybe (catMaybes)
import Prelude hiding (unwords)

import System.Directory (getDirectoryContents, doesDirectoryExist,
                         doesFileExist)

data ModuleInfo = ModuleInfo {
    module_name              :: String,
    module_prefix            :: String,
    module_needspreproc      :: Bool,
    module_needsc2hs         :: Bool,
    module_filename          :: String,
    module_authors           :: [String],
    module_created           :: String,
    module_copyright_dates   :: Either String (String, String),
                                -- eg "2004" or "2004-2005"
    module_copyright_holders :: [String],
    module_exports           :: [String],
    module_imports           :: [(String, String)], -- mod name and the whole line
    module_context_lib       :: String,
    module_context_prefix    :: String,
    module_functions         :: [FuncInfo],
    module_deprecated        :: [String]      -- {-# DEPRECATED #-} pragmas
  } deriving Show

data FuncInfo = FuncInfo {
    func_name      :: String,
    func_docs      :: [String],
    func_docs_hash :: String,
    func_body      :: [String],
    func_body_hash :: String,
    func_ccalls    :: [CCallInfo]
  } deriving Show

data CCallInfo = CCallInfo {
    ccall_name   :: String,
    ccall_unsafe :: Bool    
  } deriving Show

data Line = None
          | Authors [String]
          | Created String
          | Copyright (Either String (String, String)) [String]
          | Module String String
          | Export String
          | ExportEnd
          | Import String String
          | Context String String
          | Deprecated String
          | TypeSig String String
          | DocBegin String
          | Hash String String
          | Line String
  deriving Show

isModuleLine, isExportEndLine, isContextLine :: Line -> Bool

isModuleLine (Module _ _) = True
isModuleLine _            = False
isExportEndLine ExportEnd = True
isExportEndLine _         = False
isContextLine (Context _ _) = True
isContextLine _             = False

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
         break isContextLine
         [ scanLine line (tokenise line) | line <- lines content ]
  in ModuleInfo {
    module_name              = head $ [ name    | Module name _   <- headerLines ] ++ [missing],
    module_prefix            = head $ [ prefix  | Module _ prefix <- headerLines ] ++ [missing],
    module_needspreproc      = ".chs.pp" `isSuffixOf` filename
                            ||     ".pp" `isSuffixOf` filename,
    module_needsc2hs         = ".chs"    `isSuffixOf` filename
                            || ".chs.pp" `isSuffixOf` filename,
    module_filename          = "",
    module_authors           = head $ [ authors | Authors authors     <- headerLines ] ++ [[missing]],
    module_created           = head $ [ created | Created created     <- headerLines ] ++ [missing],
    module_copyright_dates   = head $ [ dates   | Copyright dates _   <- headerLines ] ++ [Left missing],
    module_copyright_holders = head $ [ authors | Copyright _ authors <- headerLines ] ++ [[missing]],
    module_exports           = let exportLines = takeWhile (not.isExportEndLine)
                                               . dropWhile (not.isModuleLine)
                                               $ headerLines
                                in [ name       | Export name      <- exportLines ],
    module_imports           = [ (name, line)   | Import name line <- headerLines ],
    module_context_lib       = head $ [ lib     | Context lib _    <- headerLines ] ++ [missing],
    module_context_prefix    = head $ [ prefix  | Context _ prefix <- headerLines ] ++ [missing],
    module_functions         = scanFunctions bodyLines,
    module_deprecated        =        [ value   | Deprecated value <- bodyLines ]
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
scanLine _ ("--":"Copyright":"(":_C:")":copyright) = scanCopyright copyright
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
scanLine line@('-':_) ("--":"|":_)      = DocBegin line
scanLine line@(c:_) (name:"::":_) | isAlpha c = TypeSig name line
scanLine _ ["--","%hash",('c':':':code)
                        ,('d':':':doc)] = Hash code doc
scanLine _ ["--","%hash",('d':':':doc)] = Hash "" doc
scanLine _ ["--","%hash",('c':':':code)] = Hash code ""
scanLine line _                         = Line line

scanAuthor :: [String] -> Line
scanAuthor = 
    Authors
  . map unwords
  . splitBy ","

scanCopyright :: [String] -> Line
scanCopyright (from:"..":to:name)         = Copyright (Right (from, to)) (map unwords $ splitBy "," name)
scanCopyright (from:"-":to:name)          = Copyright (Right (from, to)) (map unwords $ splitBy "," name)
scanCopyright ("[":from:"..":to:"]":name) = Copyright (Right (from, to)) (map unwords $ splitBy "," name)
scanCopyright (year:name)                 = Copyright (Left year)        (map unwords $ splitBy "," name)
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

scanCCall :: String -> Maybe CCallInfo
scanCCall line
  | "{#" `notElem` tokens = Nothing
  | otherwise =
  case takeWhile (\t -> t/="#}" && t/="#}."&& t/="#})")
     . tail
     . dropWhile (/="{#")
     $ tokens
    of ["call","pure","unsafe",cname] -> Just $ CCallInfo cname True
       ["call","pure"         ,cname] -> Just $ CCallInfo cname True
       ["call","unsafe"       ,cname] -> Just $ CCallInfo cname True
       ["call"                ,cname] -> Just $ CCallInfo cname False
       ["call","fun","unsafe" ,cname] -> Just $ CCallInfo cname True
       _                              -> Nothing
  where tokens = tokenise line

scanFunctions :: [Line] -> [FuncInfo]
scanFunctions =
    groupDocWithFunc
  . amalgamateLines 
  . splitOn isInterestingLine

  where isInterestingLine (TypeSig _ _) = True
        isInterestingLine (DocBegin  _) = True
        isInterestingLine (Hash    _ _) = True
        isInterestingLine _             = False

        amalgamateLines :: [[Line]] -> [[Line]]
        amalgamateLines ([hash@(Hash _ _), doc@(DocBegin _)]:chunks) =
          [hash] : amalgamateLines ([doc] : chunks)
        amalgamateLines ([ty@(TypeSig _ _)]:prog@(Line _:_):chunks) =
          (ty:prog)  : amalgamateLines chunks
        amalgamateLines ([doc@(DocBegin _)]:docs@(Line _:_):chunks) =
          (doc:docs) : amalgamateLines chunks
        amalgamateLines (chunk:chunks) = chunk : amalgamateLines chunks
        amalgamateLines [] = []

        groupDocWithFunc :: [[Line]] -> [FuncInfo]
        groupDocWithFunc ([Hash ch dh]:(DocBegin dl:dls):(TypeSig name tl:tls):chunks) =
          addDoc dl dls (funcInfo name tl tls ch dh) : groupDocWithFunc chunks
        groupDocWithFunc ((DocBegin dl:dls):(TypeSig name tl:tls):chunks) =
          addDoc dl dls (funcInfo name tl tls "" "") : groupDocWithFunc chunks
        groupDocWithFunc ([Hash ch dh]:(TypeSig name tl:tls):chunks) = 
          funcInfo name tl tls ch dh : groupDocWithFunc chunks
        groupDocWithFunc ((TypeSig name tl:tls):chunks) = 
          funcInfo name tl tls "" "" : groupDocWithFunc chunks
        groupDocWithFunc (chunk:chunks) = groupDocWithFunc chunks
        groupDocWithFunc [] = []

        funcInfo name tl tls ch dh =
          let dropTailingBlanks = reverse . dropWhile (all isSpace) . reverse
              body = dropTailingBlanks (tl : [ tl | Line tl <- tls ])
           in FuncInfo {
                func_name   = name,
                func_docs   = [],
                func_body   = body,
                func_ccalls = catMaybes (map scanCCall body),
                func_docs_hash = dh,
                func_body_hash = ch
              }
        addDoc dl dls inf = inf { func_docs = dl : [ dl | Line dl <- dls ] }

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
