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
import StringUtils (ss, templateSubstitute)

import Monad  (when)
import List   (isPrefixOf)
import System (getArgs, exitWith, ExitCode(..))

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
  let docFile = case map (drop 6) (filter ("--doc=" `isPrefixOf`)  rem) of
                  [] -> ""
		  (docFile:_) -> docFile
  let lib = case map (drop 6) (filter ("--lib=" `isPrefixOf`)  rem) of
              [] -> ""
	      (lib:_) -> lib
  let prefix = case map (drop 9) (filter ("--prefix=" `isPrefixOf`)  rem) of
                 [] -> ""
                 (prefix:_) -> prefix
  let modPrefix = case map (drop 12) (filter ("--modprefix=" `isPrefixOf`)  rem) of
                    [] -> ""
		    (modPrefix:_) -> modPrefix ++ "."
  let outdir = case map (drop 9) (filter ("--outdir=" `isPrefixOf`)  rem) of
                 [] -> ""
                 (outdir:_) -> if last outdir == '/' then outdir else outdir ++ "/"
  let includeApiFiles = map (drop 13) (filter ("--includeapi=" `isPrefixOf`)  rem)

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
      knownTypes = makeKnownTypesMap (api ++ concat includeApi)

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
    (\(namespace, object, maybeModuleDoc) -> do
      moduleDoc <- case maybeModuleDoc of
                     Nothing -> do when (not (null apiDoc)) $
		                     putStrLn ("Warning: no documentation found for module "
			                    ++ show (object_name object))
			           return noModuleDoc
		     Just moduleDoc -> return $ addVersionParagraphs namespace moduleDoc
      writeFile (outdir ++ object_name object ++ ".chs") $
        templateSubstitute template (\var ->
          case var of
	    "YEAR"           -> ss year
	    "DATE"           -> ss date
	    "OBJECT_NAME"    -> ss (object_name object)
	    "DESCRIPTION"    -> ss (moduledoc_summary moduleDoc)
	    "DOCUMENTATION"  -> genModuleDocumentation moduleDoc
	    "TODO"           -> genTodoItems object
	    "MODULE_NAME"    -> ss (modPrefix ++ object_name object)
	    "EXPORTS"        -> genExports object moduleDoc
	    "IMPORTS"        -> ss $ "{#import Graphics.UI.Gtk.Types#}\n"
                                  ++ "-- CHECKME: extra imports may be required\n"
	    "CONTEXT_LIB"    -> ss (if null lib then namespace_library namespace else lib)
	    "CONTEXT_PREFIX" -> ss (if null prefix then namespace_library namespace else  prefix)
	    "MODULE_BODY"    -> genModuleBody knownTypes object moduleDoc
	    _ -> ss "" ) ""
    ) [ (namespace, object, lookup (object_cname object) apiDocMap)
      | namespace <- api
      , object <- namespace_objects namespace ]
    

usage = do
  putStr "\nProgram to generate a .chs Haskell binding module from an xml\n\
	\description of a GObject-style API. Usage:\n\
	\ApiGen <apiFile> <templateFile>\n\
	\         {--doc=<docFile>} {--lib=<lib>} {--prefix=<prefix>}\n\
	\         {--outdir=<outDir>} {--modprefix=<modPrefix>}\n\
	\         {--includeapi=<incApiFile>}\n\
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
	\                  uses types defined by Gdk and Pango.\n"
  exitWith $ ExitFailure 1
