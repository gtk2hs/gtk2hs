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
import StringUtils (ss, sc, templateSubstitute, splitOn)
import ModuleScan
import ExcludeApi
import MarshalFixup (fixModuleDocMapping)

import Control.Monad  (when, liftM)
import Data.List   (isPrefixOf, intersperse)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Directory (doesDirectoryExist, createDirectory)
import System.Directory (createDirectoryIfMissing)

import qualified Text.XML.HaXml.Parse as Xml

import qualified System.Time

import System.Console.GetOpt

-------------------------------------------------------------------------------
-- Top level stuff
-------------------------------------------------------------------------------

data Flag = Doc FilePath | Lib String | Prefix String
          | ModPrefix String | OutDir FilePath
          | IncludeAPI FilePath | ExcludeAPI FilePath
          | ScanModule FilePath | ExcludeScan FilePath

options :: [OptDescr Flag]
options =
 [ Option []     ["outdir"]      (ReqArg OutDir "DIR")
     "is the name and path of the output file"

 , Option []     ["doc"]         (ReqArg Doc "FILE")
     "api doc file output from format-doc.xsl"

 , Option []     ["lib"]         (ReqArg Lib "LIB")
     ("set the lib to use in the c2hs {#context #}\n"
   ++ "declaration (the default is taken from the api file)")

 , Option []     ["prefix"]      (ReqArg Prefix "PREFIX")
     ("set the prefix to use in the c2hs {#context #}\n"
   ++ "declaration (the default is taken from the api file)")

 , Option []     ["modprefix"]   (ReqArg ModPrefix "PREFIX")
     ("specify module name prefix, eg if using hierarchical\n"
   ++ "module names")

 , Option []     ["includeapi"]  (ReqArg IncludeAPI "FILE")
     ("the api xml file for a parent api, for example Gtk uses\n"      
   ++ "types defined by Gdk and Pango")

 , Option []     ["excludeapi"]  (ReqArg ExcludeAPI "FILE")
     ("an 'api.ignore' file of regexps which can be used to\n"
   ++ "stop specific API bindings being generated")

 , Option []     ["scanmodules"] (ReqArg ScanModule "DIR")
     "the path to the existing modules"

 , Option []     ["excludescan"] (ReqArg ExcludeScan "DIR")
     ("path to existing modules that you do not want to have\n"
   ++ "scanned, perhaps because they are from a different\n"
   ++" library than the one being generated")
 ]

header = unlines
  ["Program to generate a .chs Haskell binding module from an xml"
  ,"description of a GObject-style API."
  ,"usage: ApiGen <apiFile> <templateFile> [option]"
  ,"    <apiFile>           an xml api file produced by gapi_parser.pl"
  ,"    <templateFile>      is the name and path of the output template file"]

main = do
  args <- getArgs
  (flags, apiFile, templateFile) <-
    case getOpt Permute options args of
      (flags, [apiFile, templateFile], []) ->
        return (flags, apiFile, templateFile)

      (_,_,errs) -> do putStrLn (concat errs ++ usageInfo header options)
                       exitFailure

  -----------------------------------------------------------------------------
  -- Parse command line parameters
  --
  let firstOr x []    = x
      firstOr _ (x:_) = x
      docFile   = firstOr "" [ file | Doc file <- flags ]
      lib       = firstOr "" [ file | Lib file <- flags ]
      prefix    = firstOr "" [ prefix | Prefix prefix <- flags ]
      modPrefix = firstOr "" [ prefix | ModPrefix prefix <- flags ]
      outdir    = (\dir -> if last dir == '/' then dir else dir ++ "/") $
                  firstOr "" [ file | OutDir file <- flags ]
      includeApiFiles = [ file | IncludeAPI file <- flags ]
      excludeApiFiles = [ file | ExcludeAPI file <- flags ]
      moduleRoot      = firstOr "" [ dir | ScanModule dir <- flags ]
      excludePaths    = [ dir | ExcludeScan dir <- flags ]

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
              Just moduleInfo -> do createDirectoryIfMissing True
                                      outdir (splitOn '.' (module_prefix moduleInfo))
                                    return moduleInfo
              Nothing -> do
                return ModuleInfo {
                    module_name              = object_name object,
                    module_prefix            = modPrefix,
                    module_needspreproc      = False,
                    module_needsc2hs         = True,
                    module_filename          = object_name object ++ ".chs",
                    module_authors           = ["[Insert your full name here]"],
                    module_created           = date,
                    module_rcs_version       = "",
		    module_rcs_timestamp     = "",
		    module_copyright_dates   = Left year,
                    module_copyright_holders = ["[Insert your full name here]"],
                    module_exports           = [],
                    module_imports           = [],
                    module_context_lib       = if null lib then namespace_library namespace else lib,
                    module_context_prefix    = if null prefix then namespace_library namespace else prefix,
                    module_methods           = [],
                    module_deprecated        = []
                  }
      writeFile (outdir ++ module_filename moduleInfo) $
        templateSubstitute template (\var ->
          case var of
	    "YEAR"           -> ss $ formatCopyrightDates year (module_copyright_dates moduleInfo)
	    "DATE"           -> ss $ module_created moduleInfo
	    "OBJECT_KIND"    -> ss $ if object_isinterface object then "Interface" else "Widget"
	    "OBJECT_NAME"    -> ss $ module_name moduleInfo
	    "AUTHORS"        -> ss $ concat $ intersperse ", " $ module_authors moduleInfo
	    "RCS_VERSION"    -> sc '$'. ss "Revision". ss ": ". ss (module_rcs_version moduleInfo). ss " $"
	    "RCS_TIMESTAMP"  -> sc '$'. ss "Date". ss ": ". ss (module_rcs_timestamp moduleInfo). ss " $"
            "COPYRIGHT"      -> ss $ concat $ intersperse ", " $ module_copyright_holders moduleInfo
            "DESCRIPTION"    -> haddocFormatParas knownTypes False (moduledoc_summary moduleDoc)
	    "DOCUMENTATION"  -> genModuleDocumentation knownTypes moduleDoc
	    "TODO"           -> genTodoItems object
	    "MODULE_NAME"    -> genModuleName object moduleInfo
	    "EXPORTS"        -> genExports object moduleDoc moduleInfo
	    "IMPORTS"        -> genImports moduleInfo
	    "CONTEXT_LIB"    -> ss $ module_context_lib moduleInfo
	    "CONTEXT_PREFIX" -> ss $ module_context_prefix  moduleInfo
	    "MODULE_BODY"    -> genModuleBody knownTypes object moduleDoc moduleInfo
	    _ -> ss "" ) ""
    ) [ (namespace
        ,object
        ,lookup (fixModuleDocMapping (object_cname object)) apiDocMap
        ,lookup (object_name object) moduleInfoMap)
      | namespace <- api
      , object <- namespace_objects namespace
               ++ map mungeClassToObject (namespace_classes namespace)
               ++ map mungeBoxedToObject (namespace_boxed namespace) ]

formatCopyrightDates :: String -> Either String (String, String) -> String
formatCopyrightDates currentYear (Left year) | year == currentYear = year
                                             | otherwise = year ++ "-" ++ currentYear
formatCopyrightDates currentYear (Right (from, to)) = from ++ "-" ++ currentYear
