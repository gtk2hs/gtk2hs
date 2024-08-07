-- ApiGen: takes an xml description of a GObject-style API and produces a .chs
-- binding module. Optionally it can be supplied with an xml documentation file
-- in which case the .chs file will contain haddock-format documentation too.

-- If you want to teach ApiGen how to marshal new types, the you want to modify
-- either genMarshalParameter or genMarshalResult in the Marshal module.

module Main (main) where

import Module (Module(..))
import qualified Module
import qualified Api (API, extractAPI)
import qualified Docs (extractDocumentation, moduledoc_summary)
import qualified AddDocs (addDocsToModule, mkModuleDocMap,
                          handleDocNULLs, fixModuleHierarchy)
import qualified CodeGen (genModuleBody, genTodoItems, makeKnownSymbolsMap)
import qualified ModuleScan
import Utils

import Data.List   (intersperse)
import qualified Data.Map as Map (fromList)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Directory (createDirectoryIfMissing)

import qualified Text.XML.HaXml.Parse as Xml

import qualified System.Time

import System.Console.GetOpt

-------------------------------------------------------------------------------
-- Top level stuff
-------------------------------------------------------------------------------

data Flag = Doc FilePath | ModPrefix String | OutDir FilePath
          | IncludeAPI FilePath | ExcludeAPI FilePath
          | ScanModule FilePath | ExcludeScan FilePath

options :: [OptDescr Flag]
options =
 [ Option []     ["outdir"]      (ReqArg OutDir "DIR")
     "is the name and path of the output file"

 , Option []     ["doc"]         (ReqArg Doc "FILE")
     "api doc file output from format-doc.xsl"

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

header :: String
header = unlines
  ["Program to generate a .chs Haskell binding module from an xml"
  ,"description of a GObject-style API."
  ,"usage: ApiGen <apiFile> <templateFile> [option]"
  ,"    <apiFile>           an xml api file produced by gapi_parser.pl"
  ,"    <templateFile>      is the name and path of the output template file"]

main :: IO ()
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
      modPrefix = firstOr "" [ prefix' | ModPrefix prefix' <- flags ]
      outdir    = (\dir -> if last dir == '/' then dir else dir ++ "/") $
                  firstOr "" [ file | OutDir file <- flags ]
      includeApiFiles = [ file | IncludeAPI file <- flags ]
      excludeApiFiles = [ file | ExcludeAPI file <- flags ]
      moduleRoot      = firstOr "" [ dir | ScanModule dir <- flags ]
      excludePaths    = [ dir | ExcludeScan dir <- flags ]

  -----------------------------------------------------------------------------
  -- Read in the input files
  --
  apicontent <- if apiFile == "-"
               then getContents       -- read stdin
               else readFile apiFile
  template <- readFile templateFile

  includeApiFilesContents <- mapM readFile includeApiFiles

  -----------------------------------------------------------------------------
  -- Parse the contents of the xml api file
  --
  let document = Xml.xmlParse apiFile apicontent
      api = Api.extractAPI document

      -- For example when processing Gtk we'd like to know about the types
      -- included from Gdk and Pango
      includeApi = [ Api.extractAPI (Xml.xmlParse apiFile' content')
                   | (apiFile', content') <- zip includeApiFiles includeApiFilesContents]
      knownTypes = CodeGen.makeKnownSymbolsMap (api ++ concat includeApi)

  -----------------------------------------------------------------------------
  -- Read in the documentation xml file if supplied
  --
  apiDoc <- if null docFile
              then return []
              else do content <- readFile docFile
                      return $ Docs.extractDocumentation (Xml.xmlParse docFile content)
  let apiDocMap = AddDocs.mkModuleDocMap apiDoc

  -----------------------------------------------------------------------------
  -- Scan the existing modules if their root path is supplied
  --
  modulesInfo <- if null moduleRoot
                   then return []
                   else ModuleScan.scanModules moduleRoot excludePaths
  let moduleInfoMap = Map.fromList
        [ (ModuleScan.module_name moduleInfo, moduleInfo)
        | moduleInfo <- modulesInfo ]

  -----------------------------------------------------------------------------
  -- Load up any api.exclude files supplied to filter out unwanted APIs
  --
  excludeApiFilesContents <- mapM readFile excludeApiFiles

  -----------------------------------------------------------------------------
  -- A few values that are used in the template
  --
  time <- System.Time.getClockTime
  calendarTime <- System.Time.toCalendarTime time
  let day   = show (System.Time.ctDay calendarTime)
      month = show (System.Time.ctMonth calendarTime)
      year  = show (System.Time.ctYear calendarTime)
      date  = day ++ " " ++ month ++ " " ++ year

  let doEverything :: Api.API -> [Module]
      doEverything =
          map Module.reorderDecls
        . map Module.addDeclAvailableSincePara
        . map Module.fixModuleAvailableSince
        . map Module.filterNewActionSignals
        . map Module.excludeConstructOnlyAttrs
        . map Module.makeGetSetProps
        . map Module.makeOldSignals
        . map Module.filterVarArgs
        . map Module.filterDeprecated
        . map (Module.applyModuleScanInfo modPrefix date year moduleInfoMap)
        . map AddDocs.handleDocNULLs
        . map AddDocs.fixModuleHierarchy
        . map Module.deleteUnnecessaryDocs
        . map (AddDocs.addDocsToModule knownTypes apiDocMap)
        . map (Module.excludeApi excludeApiFilesContents)
        . Module.convertAPI

  -----------------------------------------------------------------------------
  -- Write the result file(s) by substituting values into the template file
  --
  flip mapM_ (doEverything api) $ \module_ -> do
    let modulePrefixToPath = map dotToPath
        dotToPath '.' = '/'
        dotToPath  c  =  c
    createDirectoryIfMissing True
      (outdir ++ '/' : modulePrefixToPath (module_prefix module_))
    writeFile (outdir ++ module_filename module_) $ render $
      templateSubstitute template $ \var ->
        case var of
          "YEAR"           -> text $ formatCopyrightDates year (module_copyright_dates module_)
          "DATE"           -> text $ module_created module_
          "OBJECT_KIND"    -> text $ show (module_kind module_)
          "OBJECT_NAME"    -> text $ module_name module_
          "AUTHORS"        -> hsep $ punctuate comma $ map text (module_authors module_)
          "COPYRIGHT"      -> hsep $ punctuate comma $ map text (module_copyright_holders module_)
          "TODO"           -> CodeGen.genTodoItems module_
          "MODULE_BODY"    -> CodeGen.genModuleBody knownTypes module_
          name             -> empty

formatCopyrightDates :: String -> Either String (String, String) -> String
formatCopyrightDates currentYear (Left year) | year == currentYear = year
                                             | otherwise = year ++ "-" ++ currentYear
formatCopyrightDates currentYear (Right (from, _)) = from ++ "-" ++ currentYear
