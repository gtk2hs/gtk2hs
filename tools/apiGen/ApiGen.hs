-- ApiGen: takes an xml description of a GObject-style API and produces a .chs
-- binding module. Optionally it can be supplied with an xml documentation file
-- in which case the .chs file will contain haddock-format documentation too.

-- If you want to teach ApiGen how to marshal new types, the you want to modify
-- either genMarshalParameter or genMarshalResult in the Marshal module.

module Main (main) where

import Api
import Docs
import FormatDocs
import CodeGen
import StringUtils (ss, templateSubstitute, splitOn)
import ModuleScan
import ExcludeApi

import Monad  (when, liftM)
import List   (isPrefixOf, intersperse)
import System (getArgs, exitWith, ExitCode(..))
import Directory (doesDirectoryExist, createDirectory)

import qualified Text.XML.HaXml.Parse as Xml

import qualified System.Time

-------------------------------------------------------------------------------
-- Top level stuff
-------------------------------------------------------------------------------

main = do
  args <- getArgs
  when (length args < 2) usage

  -----------------------------------------------------------------------------
  -- Parse command line parameters
  --
  let (apiFile: templateFile: rem) = args
      docFile = case map (drop 6) (filter ("--doc=" `isPrefixOf`)  rem) of
                  [] -> ""
		  (docFile:_) -> docFile
      lib = case map (drop 6) (filter ("--lib=" `isPrefixOf`)  rem) of
              [] -> ""
	      (lib:_) -> lib
      prefix = case map (drop 9) (filter ("--prefix=" `isPrefixOf`)  rem) of
                 [] -> ""
                 (prefix:_) -> prefix
      modPrefix = case map (drop 12) (filter ("--modprefix=" `isPrefixOf`)  rem) of
                    [] -> ""
		    (modPrefix:_) -> modPrefix
      outdir = case map (drop 9) (filter ("--outdir=" `isPrefixOf`)  rem) of
                 [] -> ""
                 (outdir:_) -> if last outdir == '/' then outdir else outdir ++ "/"
      includeApiFiles = map (drop 13) (filter ("--includeapi=" `isPrefixOf`)  rem)
      excludeApiFiles = map (drop 13) (filter ("--excludeapi=" `isPrefixOf`)  rem)
      moduleRoot = case map (drop 14) (filter ("--scanmodules=" `isPrefixOf`)  rem) of
                     [] -> ""
                     (moduleRoot:_) -> moduleRoot
      excludePaths = map (drop 14) (filter ("--excludescan=" `isPrefixOf`)  rem)

  -----------------------------------------------------------------------------
  -- Read in the input files
  --
  content <- if apiFile == "-"
               then getContents	      -- read stdin
	       else readFile apiFile
  template <- readFile templateFile
  
  includeApiFilesContents <- mapM readFile includeApiFiles

  -----------------------------------------------------------------------------
  -- Parse the contents of the xml api file
  --
  let document = Xml.xmlParse apiFile content
      api = extractAPI document
  
      -- For example whe processing Gtk we'd like to know about the types
      -- included from Gdk and Pango
      includeApi = [ extractAPI (Xml.xmlParse apiFile content)
                   | (apiFile, content) <- zip includeApiFiles includeApiFilesContents]  
      knownTypes = makeKnownSymbolsMap (api ++ concat includeApi)

  -----------------------------------------------------------------------------
  -- Read in the documentation xml file if supplied
  --
  apiDoc <- if null docFile
              then return []
              else do content <- readFile docFile
                      return $ extractDocumentation (Xml.xmlParse docFile content)
  let apiDocMap = [ (moduledoc_name    moduleDoc, moduleDoc) | moduleDoc <- apiDoc ]
               ++ [ (moduledoc_altname moduleDoc, moduleDoc) | moduleDoc <- apiDoc ]

  -----------------------------------------------------------------------------
  -- Scan the existing modules if their root path is supplied
  --
  modulesInfo <- if null moduleRoot
                   then return []
                   else scanModules moduleRoot excludePaths
  let moduleInfoMap = [ (module_name moduleInfo, moduleInfo)
                      | moduleInfo <- modulesInfo ]

  -----------------------------------------------------------------------------
  -- Load up any api.exclude files supplied to filter out unwanted APIs
  --
  excludeApiFilesContents <- mapM readFile excludeApiFiles
  let filterSpecs = map parseFilterFile excludeApiFilesContents
      okAPI :: String -> Bool   --returns False to exclude the C function name
      okAPI | null (concat filterSpecs) = const True
            | otherwise = matcher (concat filterSpecs)
  
  -----------------------------------------------------------------------------
  -- A few values that are used in the template
  --
  time <- System.Time.getClockTime
  calendarTime <- System.Time.toCalendarTime time
  let day   = show (System.Time.ctDay calendarTime)
      month = show (System.Time.ctMonth calendarTime)
      year  = show (System.Time.ctYear calendarTime)
      date  = day ++ " " ++ month ++ " " ++ year

  -----------------------------------------------------------------------------
  -- Write the result file(s) by substituting values into the template file
  --
  mapM 
    (\(namespace, object', maybeModuleDoc, maybeModuleInfo) -> do
      let object = object' {
              object_methods = [ method
                               | method <- object_methods object'
                               , okAPI (method_cname method) ]
            }
      moduleDoc <- case maybeModuleDoc of
                     Nothing -> do when (not (null apiDoc)) $
		                     putStrLn ("Warning: no documentation found for module "
			                    ++ show (object_name object))
			           return noModuleDoc
		     Just moduleDoc -> return $ addVersionParagraphs namespace moduleDoc
      moduleInfo <-
            liftM (mungeMethodInfo object) $
            case maybeModuleInfo of
              Just moduleInfo -> do mkDirHier outdir (splitOn '.' (module_prefix moduleInfo))
                                    return moduleInfo
              Nothing -> do
                when (not (null moduleRoot) && not (object_deprecated object)) $
                  putStrLn ("Warning: no existing module found for module "
	                  ++ show (object_name object))
                return ModuleInfo {
                    module_name              = object_name object,
                    module_prefix            = modPrefix,
                    module_needspreproc      = False,
                    module_filename          = object_name object ++ ".chs",
                    module_authors           = ["[Insert your full name here]"],
                    module_created           = date,
                    module_copyright_dates   = Left year,
                    module_copyright_holders = ["[Insert your full name here]"],
                    module_imports           = [],
                    module_context_lib       = if null lib then namespace_library namespace else lib,
                    module_context_prefix    = if null prefix then namespace_library namespace else prefix,
                    module_methods           = []
                  }
      writeFile (outdir ++ module_filename moduleInfo) $
        templateSubstitute template (\var ->
          case var of
	    "YEAR"           -> ss $ formatCopyrightDates year (module_copyright_dates moduleInfo)
	    "DATE"           -> ss $ module_created moduleInfo
	    "OBJECT_NAME"    -> ss $ module_name moduleInfo
	    "AUTHORS"        -> ss $ concat $ intersperse ", " $ module_authors moduleInfo
            "COPYRIGHT"      -> ss $ concat $ intersperse ", " $ module_copyright_holders moduleInfo
            "DESCRIPTION"    -> haddocFormatParas knownTypes False (moduledoc_summary moduleDoc)
	    "DOCUMENTATION"  -> genModuleDocumentation knownTypes moduleDoc
	    "TODO"           -> genTodoItems object
	    "MODULE_NAME"    -> ss $ if null (module_prefix moduleInfo)
                                 then module_name moduleInfo
                                 else module_prefix moduleInfo ++ "." ++ module_name moduleInfo
	    "EXPORTS"        -> genExports object moduleDoc moduleInfo
	    "IMPORTS"        -> genImports moduleInfo
	    "CONTEXT_LIB"    -> ss $ module_context_lib moduleInfo
	    "CONTEXT_PREFIX" -> ss $ module_context_prefix  moduleInfo
	    "MODULE_BODY"    -> genModuleBody knownTypes object moduleDoc moduleInfo
	    _ -> ss "" ) ""
    ) [ (namespace
        ,object
        ,lookup (object_cname object) apiDocMap
        ,lookup (object_name object) moduleInfoMap)
      | namespace <- api
      , object <- namespace_objects namespace ]
    

usage = do
  putStr "\nProgram to generate a .chs Haskell binding module from an xml\n\
	\description of a GObject-style API. Usage:\n\
	\ApiGen <apiFile> <templateFile>\n\
	\         {--doc=<docFile>} {--lib=<lib>} {--prefix=<prefix>}\n\
	\         {--outdir=<outDir>} {--modprefix=<modPrefix>}\n\
	\         {--includeapi=<incApiFile>} {--excludeapi=<exclApiFile>}\n\
	\         {--scanmodules=<modulesRoot>} {--excludescan=<excludePath>}\n\
	\where\n\
	\  <apiFile>       an xml api file produced by gapi_parser.pl\n\
	\  <templateFile>  is the name and path of the output template file\n\
	\  <outDir>        is the name and path of the output file\n\
	\  <docFile>       api doc file output from format-doc.xsl\n\
	\  <lib>           set the lib to use in the c2hs {#context #}\n\
	\                  declaration (the default is taken from the api file)\n\
	\  <prefix>        set the prefix to use in the c2hs {#context #}\n\
	\                  declaration (the default is taken from the api file)\n\
	\  <modPrefix>     specify module name prefix, eg if using\n\
	\                  hierarchical module names\n\
	\  <incApiFile>    the api xml file for a parent api, for example Gtk\n\
	\                  uses types defined by Gdk and Pango.\n\
	\  <exclApiFile>   an 'api.ignore' file of regexps which can be used\n\
	\                  to stop specific API bindings being generated.\n\
        \  <modulesRoot>   the path to the existing modules.\n\
        \  <excludePath>   path to existing modules that you do not want to\n\
        \                  have scanned, perhaps because they are from a\n\
        \                  different library than the one being generated.\n"
  exitWith $ ExitFailure 1

formatCopyrightDates :: String -> Either String (String, String) -> String
formatCopyrightDates currentYear (Left year) | year == currentYear = year
                                             | otherwise = year ++ "-" ++ currentYear
formatCopyrightDates currentYear (Right (from, to)) = from ++ "-" ++ currentYear

mkDirHier :: String -> [String] -> IO ()
mkDirHier base [] = return ()
mkDirHier base (dir:dirs) = do
  let dirPath = base ++ "/" ++ dir
  exists <- doesDirectoryExist dirPath
  when (not exists) $
    createDirectory dirPath
  mkDirHier dirPath dirs
