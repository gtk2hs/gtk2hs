-- ApiGen: takes an xml description of a GObject-style API and produces a .chs
-- binding module.

--module Main (main) where

import Prelude hiding (Enum, lines)
import Monad  (when)
import Maybe  (catMaybes)
import Char   (toLower, toUpper, isAlpha, isAlphaNum, isUpper)
import List   (isPrefixOf, groupBy, sortBy)
import System (getArgs, exitWith, ExitCode(..))

import qualified Text.XML.HaXml as Xml
import qualified Text.XML.HaXml.Parse as Xml
import qualified Text.XML.HaXml.Escape as Xml

import qualified System.Time

-------------------------------------------------------------------------------
-- Types representing the content of the API XML file
-------------------------------------------------------------------------------
type API = [NameSpace]

data NameSpace = NameSpace {
    namespace_name :: String,
    namespace_library :: String,
    namespace_objects :: [Object],
    namespace_enums :: [Enum]
  } deriving Show

data Enum = Enum {
    enum_name :: String,
    enum_cname :: String,
    enum_variety :: String,
    enum_members :: [Member]
 } deriving Show

data Member = Member {
    member_name :: String,
    member_cname :: String,
    member_value :: String
  } deriving Show

data Object = Object {
    object_name :: String,
    object_cname :: String,
    object_parent :: String,
    object_constructors :: [Constructor],
    object_methods :: [Method]
  } deriving Show

data Constructor = Constructor {
    constructor_cname :: String,
    constructor_parameters :: [Parameter]
  } deriving Show

data Parameter = Parameter {
    parameter_type :: String,
    parameter_name :: String,
    parameter_isArray :: Bool
  }
               | VarArgs
  deriving Show

data Method = Method {
    method_name :: String,
    method_cname :: String,
    method_return_type :: String,
    method_parameters :: [Parameter]
  } deriving Show

-------------------------------------------------------------------------------
-- extract functions to convert the api xml file to the internal representation
-------------------------------------------------------------------------------
extractAPI :: Xml.Document -> API
extractAPI (Xml.Document _ _ (Xml.Elem "api" [] namespaces)) =
  catMaybes (map extractNameSpace namespaces)

extractNameSpace :: Xml.Content -> Maybe NameSpace
extractNameSpace (Xml.CElem (Xml.Elem "namespace"
                        [("name", Xml.AttValue name),
                         ("library", Xml.AttValue lib)] content)) =
  Just $ NameSpace {
    namespace_name = Xml.verbatim name,
    namespace_library = Xml.verbatim lib,
    namespace_objects = catMaybes (map extractObject content),
    namespace_enums = []
  }
extractNameSpace _ = Nothing

extractObject :: Xml.Content -> Maybe Object
extractObject (Xml.CElem (Xml.Elem "object"
                     [("name", Xml.AttValue name),
                      ("cname", Xml.AttValue cname),
                      ("parent", Xml.AttValue parent)] content)) =
  Just $ Object {
    object_name = Xml.verbatim name,
    object_cname = Xml.verbatim cname,
    object_parent = Xml.verbatim parent,
    object_constructors = catMaybes (map extractConstructor content),
    object_methods = catMaybes (map extractMethod content)
  }
extractObject _ = Nothing

extractMethod :: Xml.Content -> Maybe Method
extractMethod (Xml.CElem (Xml.Elem "method"
                     [("name", Xml.AttValue name),
                      ("cname", Xml.AttValue cname)]
                     (Xml.CElem (Xml.Elem "return-type"
                            [("type", Xml.AttValue return_type)] [])
                      :content))) =
  Just $ Method {
    method_name = Xml.verbatim name,
    method_cname = Xml.verbatim cname,
    method_return_type = Xml.verbatim return_type,
    method_parameters =
      case content of
        [] -> []
        [Xml.CElem (Xml.Elem "parameters" [] parameters)]
           -> map extractParameter parameters
  }
extractMethod _ = Nothing

extractParameter :: Xml.Content -> Parameter
extractParameter (Xml.CElem (Xml.Elem "parameter"
                        [("ellipsis", _)] [])) = VarArgs
extractParameter (Xml.CElem (Xml.Elem "parameter"
                        [("type", Xml.AttValue type_),
                         ("name", Xml.AttValue name)] [])) =
  Parameter {
    parameter_type = Xml.verbatim type_,
    parameter_name = Xml.verbatim name,
    parameter_isArray = False
  }
extractParameter (Xml.CElem (Xml.Elem "parameter"
                        [("type", Xml.AttValue type_),
			 ("array", _),
                         ("name", Xml.AttValue name)] [])) =
   Parameter {
     parameter_type = Xml.verbatim type_,
     parameter_name = Xml.verbatim name,
     parameter_isArray = True
   }
								 

extractConstructor :: Xml.Content -> Maybe Constructor
extractConstructor (Xml.CElem (Xml.Elem "constructor"
                     [("cname", Xml.AttValue cname)] content)) =
  Just $ Constructor {
    constructor_cname = Xml.verbatim cname,
    constructor_parameters =
      case content of
        [] -> []
        [Xml.CElem (Xml.Elem "parameters" [] parameters)]
           -> map extractParameter parameters
  }
extractConstructor _ = Nothing

-------------------------------------------------------------------------------
-- extract functions to convert the doc xml file to the internal representation
-------------------------------------------------------------------------------

data ApiDoc = ApiDoc {
    doc_target :: String,		-- C function name
    doc_paragraphs :: [DocPara],	-- documentation markup
    doc_since :: Maybe String		-- which version of the api the
  }					-- function is avaliable from, eg "2.4"

type DocPara = [DocParaSpan]
data DocParaSpan = DocText String       -- just simple text
                 | DocFuncXRef String   -- cross reference to a function name
                 | DocTypeXRef String   -- cross reference to a type name
                 | DocOtherXRef String  -- xref format not directly supported
                 | DocEmphasis String   -- emphasised text, usually italic
		 | DocLiteral String    -- some literal like numbers
		 | DocArg  String       -- function argument names

extractDocumentation :: Xml.Document -> [ApiDoc]
extractDocumentation (Xml.Document _ _ (Xml.Elem "apidoc" [] functions)) =
  map extractDocFunc functions

extractDocFunc :: Xml.Content -> ApiDoc
extractDocFunc
  (Xml.CElem (Xml.Elem "function" []
    [Xml.CElem (Xml.Elem "name" [] [Xml.CString _ name])
    ,Xml.CElem (Xml.Elem "since" [] since')
    ,Xml.CElem (Xml.Elem "doc" [] paras)]
  )) =
  let since = case since' of
                [] -> Nothing
		[Xml.CString _ since] -> Just since
   in ApiDoc {
          doc_target = name,
	  doc_paragraphs = map extractDocPara paras,
	  doc_since = since
        }

extractDocPara :: Xml.Content -> DocPara
extractDocPara (Xml.CElem elem@(Xml.Elem "para" [] _)) =
  case Xml.xmlUnEscape Xml.stdXmlEscaper elem of
    (Xml.Elem _ [] spans) -> map extractDocParaSpan spans

extractDocParaSpan :: Xml.Content -> DocParaSpan
extractDocParaSpan (Xml.CString _ text) = DocText text
extractDocParaSpan (Xml.CElem (Xml.Elem tag [] (CString _ text))) =
  case tag of
    "xref-func"  -> DocFuncXRef text
    "xref-type"  -> DocTypeXRef text
    "xref-other" -> DocOtherXRef text
    "emphasis"   -> DocEmphasis text
    "literal"    -> DocLiteral text
    "arg"        -> DocArg text
extractDocParaSpan other = error $ "extractDocParaSpan: " ++ Xml.verbatim other
 
haddocFormatParas :: [DocPara] -> ShowS
haddocFormatParas =
    ((ss "-- | ". drop 3).)
  . cat
  . map ((.(ss "--\n"))
       . cat
       . map (\line -> (ss "-- ").line.ss "\n")
       . map (sepBy " ")
       . wrapText 77
       . words
       . concatMap haddocFormatSpan)

haddocFormatSpan :: DocParaSpan -> String
haddocFormatSpan (DocText text)    = escapeHaddockSpecialChars text
haddocFormatSpan (DocTypeXRef text)    = "\"" ++ text ++ "\""
haddocFormatSpan (DocFuncXRef text)    = "'" ++ cFuncNameToHsName text ++ "'"
haddocFormatSpan (DocOtherXRef text)    = "'{FIXME: gtk-doc cross reference to:" ++ text ++ "}'"
haddocFormatSpan (DocEmphasis text)  = "/" ++ text ++ "/"
haddocFormatSpan (DocLiteral "TRUE")  = "@True@"
haddocFormatSpan (DocLiteral "FALSE") = "@False@"
  --likely that something should be changed to a Maybe type if this is emitted:
haddocFormatSpan (DocLiteral "NULL")  = "@CHECKME: Nothing@"
haddocFormatSpan (DocLiteral text) = "@" ++ text ++ "@"
haddocFormatSpan (DocArg  text)    = "@" ++ text ++ "@"

cFuncNameToHsName :: String -> String
cFuncNameToHsName =
    lowerCaseFirstChar
  . stripKnownPrefixes
  . concatMap (upperCaseFirstChar "cFuncNameToHsName")
  . splitBy '_'
  . takeWhile ('('/=)

escapeHaddockSpecialChars = escape
  where escape [] = []
        escape (''':'s':cs) = ''' : 's' : escape cs --often don't need to escape
        escape (c:cs) | c == '/' || c == '`'
                     || c == '"' || c == '@'
                     || c == '<' || c == '''
                      = '\\': c : escape cs
        escape (c:cs) =       c : escape cs              

-- wraps a list of words to lines of words
wrapText :: Int -> [String] -> [[String]]
wrapText width = wrap 3 []
  
  where wrap :: Int -> [String] -> [String] -> [[String]]
        wrap col line (word:words) | col + length word + 1 > width = reverse line : wrap 0 [] (word:words)
        wrap col line (word:words) = wrap (col + length word + 1) (word:line) words
        wrap _ []   [] = []
        wrap _ line [] = [reverse line]

-------------------------------------------------------------------------------
-- Now lets actually generate some code fragments based on the api info
-------------------------------------------------------------------------------
genFunction :: Object -> Method -> Maybe ApiDoc -> ShowS
genFunction object method doc =
  formattedDoc.
  ss functionName. ss " :: ". functionType. nl.
  ss functionName. sc ' '. sepBy " " paramNames. ss " =".
  indent 1. body. nl

  where functionName = lowerCaseFirstChar (method_name method)
	(classConstraints', paramTypes', paramMarshalers) =
	  unzip3 [ genMarshalParameter (changeParamNames (parameter_name p))
	  			       (parameter_type p)
		 | p <- method_parameters method ]
	classConstraints = [ c | Just c <- classConstraints' ]
	paramTypes = [ c | Just c <- paramTypes' ]
	paramNames = [ changeParamNames (parameter_name p)
		     | (Just _, p) <- zip paramTypes' (method_parameters method) ]
	(returnType, returnMarshaler) =
		genMarshalResult (method_return_type method)
	functionType = (case classConstraints of
	                  []  -> id
			  [c] -> ss c. ss " => "
			  cs  -> sc '('. sepBy ", " classConstraints. ss ") => ").
		       sepBy " -> " (paramTypes ++ [returnType])
	body = foldl (\body marshaler -> marshaler body) call (paramMarshalers++[returnMarshaler])
	call = ss "{# call ". ss (method_cname method). ss " #}"
        formattedDoc = case doc of
          Nothing -> ss "-- | \n-- \n"
          Just doc -> haddocFormatParas (doc_paragraphs doc)

genMethods :: Object -> [ApiDoc] -> [(ShowS, Maybe ApiDoc)]
genMethods object apiDoc = 
  [ (genFunction object method doc, doc)
  | (method, doc) <- methods object apiDoc ]

methods :: Object -> [ApiDoc] -> [(Method, Maybe ApiDoc)]
methods object docs =
  map snd $
  sortBy (\(i,_) (j,_) -> i `compare` j)
  [ case method_cname method `lookup` docmap of
      Nothing             -> (0,(mungeMethod object method, Nothing))
      (Just (doc, index)) -> (index,(mungeMethod object method, Just doc))
  | method <- object_methods object
  , null [ () | VarArgs <- method_parameters method] ] --exclude VarArgs methods
  where docmap = [ (doc_target doc, (doc,index)) | (doc,index) <- zip docs [1..]]
        

mungeMethod :: Object -> Method -> Method
mungeMethod object method =
  let self = Parameter {
               parameter_type = object_cname object ++ "*",
               parameter_name = "obj",
               parameter_isArray = False
             }
   in method {
        method_name = object_name object ++ method_name method,
        method_parameters = self : method_parameters method
      } 

genConstructors :: Object -> [ApiDoc] -> [(ShowS, Maybe ApiDoc)]
genConstructors object apiDoc =
  [ (genFunction object constructor doc, doc)
  | (constructor, doc) <- constructors object apiDoc ]

constructors :: Object -> [ApiDoc] -> [(Method, Maybe ApiDoc)]
constructors object docs =
  [ (mungeConstructor object constructor, constructor_cname constructor `lookup` docmap)
  | constructor <- object_constructors object
  , null [ () | VarArgs <- constructor_parameters constructor] ]
  where docmap = [ (doc_target doc, doc) | doc <- docs ]

mungeConstructor :: Object -> Constructor -> Method
mungeConstructor object constructor =
  Method {
    method_name = (object_name object++)
                . drop (length (object_cname object))
                . concatMap (upperCaseFirstChar "mungeConstructor")
                . splitBy '_'
                . constructor_cname
                $ constructor,
    method_cname = constructor_cname constructor,
    method_return_type = object_cname object ++ "*",
    method_parameters = constructor_parameters constructor
  }  

genExports :: Object -> [ApiDoc] -> ShowS
genExports object docs =
  doVersionIfDefs
    [ (ss "  ". ss (lowerCaseFirstChar (method_name constructor)). sc ',', doc)
    | (constructor, doc) <- constructors object docs ++ methods object docs]

genTodoItems :: Object -> ShowS
genTodoItems object =
  let varargsFunctions = 
        [ ss (constructor_cname constructor)
        | constructor <- object_constructors object
        , not $ null [ () | VarArgs <- constructor_parameters constructor] ]
       ++
        [ ss (method_cname method)
        | method <- object_methods object
        , not $ null [ () | VarArgs <- method_parameters method] ]
   in if null varargsFunctions
        then id
        else nl. comment. nl. comment.
             ss "TODO: the following varargs functions were not bound\n".
             lines (map (ss "-- * ".) varargsFunctions)

doVersionIfDefs :: [(ShowS, Maybe ApiDoc)] -> ShowS
doVersionIfDefs =
    lines
  . map (\group -> sinceVersion (snd (head group))
                                (lines (map fst group)))
  . groupBy (\(_,a) (_,b) -> fmap doc_since a == fmap doc_since b)
 
sinceVersion :: Maybe ApiDoc -> ShowS -> ShowS
sinceVersion (Just (ApiDoc _ _ (Just (major:'.':minor:[])))) body =
  ss "#if GTK_CHECK_VERSION(". sc major. ss ",". sc minor. ss ",0)\n".
  body.
  ss "\n#endif"
sinceVersion _ body = body

splitBy :: Char -> String -> [String]
splitBy sep str =
  case span (sep/=) str of
    (remainder,[]) -> [remainder]
    (word,_:remainder) -> word : splitBy sep remainder

-------------------------------------------------------------------------------
-- Here's the interesting bit that generates the fragments of mashaling code
-------------------------------------------------------------------------------

genMarshalParameter ::
	String ->	--parameter name suggestion (will be unique)
	String -> 	--C type decleration for the parameter we will marshal
	(Maybe String,	--parameter class constraints (or none)
	Maybe String,	--parameter type (or none if the arg is not exposed)
	ShowS -> ShowS)	--marshaling code (\body -> ... body ...)

genMarshalParameter name "gboolean" =
	(Nothing, Just "Bool",
	\body -> body. ss " (fromBool ". ss name. ss ")")

genMarshalParameter name typeName | typeName == "guint"  --these two are unsigned types
				 || typeName == "gint"
				 || typeName == "gsize"  --should they be Word or Int?
				 || typeName == "gssize" =
	(Nothing, Just "Int",
	\body -> body. ss " (fromIntegral ". ss name. ss ")")

genMarshalParameter name "const-gchar*" =
	(Nothing, Just "String",
	\body -> ss "withUTFString ". ss name. ss " $ \\". ss name. ss "Ptr ->".
		 indent 1. body. sc ' '. ss name. ss "Ptr")

genMarshalParameter name "GError**" =
	(Nothing, Nothing,
	\body -> ss "propagateGError $ \\". ss name. ss "Ptr ->".
	         indent 1. body. sc ' '. ss name. ss "Ptr")

genMarshalParameter name typeName | isUpper (head typeName)
                                 && last typeName == '*'
                                 && last (init typeName) /= '*'  = --then assume it is an object
	let typeName' = stripKnownPrefixes (init typeName) in
	(Just $ typeName' ++ "Class " ++ name, Just name,
	\body -> body. ss " (to". ss typeName'. sc ' '. ss name. ss ")")

genMarshalParameter name unknownType =
	(Nothing, Just $ "{-" ++ unknownType ++ "-}",
	\body -> body. ss " {-". ss name. ss "-}")

-- Takes the type string and returns the Haskell Type and the marshaling code
--
genMarshalResult :: String -> (String, ShowS -> ShowS)
genMarshalResult "gboolean" = ("IO Bool", \body -> ss "liftM toBool $". indent 1. body)
genMarshalResult "gint"     = ("IO Int",  \body -> ss "liftM fromIntegral $". indent 1. body)
genMarshalResult "guint"    = ("IO Int",  \body -> ss "liftM fromIntegral $". indent 1. body)
genMarshalResult "void"     = ("IO ()", id)
genMarshalResult "const-gchar*"  = ("IO String", \body -> body.
                                                          indent 1. ss  ">>= peekUTFString")
genMarshalResult "const-GSList*" =
  ("[{- element type -}]",
  \body -> body.
           indent 1. ss ">>= readGSList".
           indent 1. ss ">>= mapM (\\elemPtr -> {-marshal elem-})")
genMarshalResult "GSList*" =
  ("[{- element type -}]",
  \body -> body.
           indent 1. ss ">>= fromGSList".
           indent 1. ss ">>= mapM (\\elemPtr -> {-marshal elem-})")
genMarshalResult "GList*" =
  ("[{- element type -}]",
  \body -> body.
           indent 1. ss ">>= fromGList".
           indent 1. ss ">>= mapM (\\elemPtr -> {-marshal elem-})")

genMarshalResult typeName | isUpper (head typeName)
                         && last typeName == '*'
                         && last (init typeName) /= '*'  = --then assume it is an object
  
  let typeName' = stripKnownPrefixes (init typeName) in
  ("IO " ++ typeName',
  \body -> ss "makeNewGObject mk". ss typeName'. ss " $".
           indent 1. body)

genMarshalResult unknownType = ("{-" ++ unknownType ++ "-}", id)

stripKnownPrefixes :: String -> String
stripKnownPrefixes ('G':'t':'k':remainder) = remainder
stripKnownPrefixes ('G':'d':'k':remainder) = remainder
stripKnownPrefixes ('P':'a':'n':'g':'o':remainder) = remainder
stripKnownPrefixes other = other

changeParamNames :: String -> String
changeParamNames "type" = "type_"  --this is a common variable name in C but of
                                   --course a keyword in Haskell
changeParamNames other  =          --change "gtk_foo_bar" to "gtkFooBar"
    lowerCaseFirstChar
  . concatMap (upperCaseFirstChar $ "changeParamNames" ++ other)
  . filter (not.null) --to ignore tailing underscores
  . splitBy '_'
  $ other

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
              [] -> "gtk"
	      (lib:_) -> lib
  let prefix = case map (drop 9) (filter ("--prefix=" `isPrefixOf`)  rem) of
                 [] -> "gtk"
                 (prefix:_) -> prefix
  let modPrefix = case map (drop 12) (filter ("--modprefix=" `isPrefixOf`)  rem) of
                    [] -> ""
		    (modPrefix:_) -> modPrefix ++ "."
  let outdir = case map (drop 9) (filter ("--outdir=" `isPrefixOf`)  rem) of
                 [] -> ""
                 (outdir:_) -> if last outdir == '/' then outdir else outdir ++ "/"

  -----------------------------------------------------------------------------
  -- Read in the input files
  --
  content <- if apiFile == "-"
               then getContents	      -- read stdin
	       else readFile apiFile
  template <- readFile templateFile

  -----------------------------------------------------------------------------
  -- Parse the contents of the xml api file
  --
  let document = Xml.xmlParse apiFile content
      api = extractAPI document

  -----------------------------------------------------------------------------
  -- Read in the documentation xml file if supplied
  --
  apiDoc <- if null docFile
              then return []
              else do content <- readFile docFile
                      return $ extractDocumentation (Xml.xmlParse docFile content)

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
    (\object -> writeFile (outdir ++ object_name object ++ ".chs") $
    templateSubstitute template (\var ->
      case var of
	"YEAR"           -> ss year
	"DATE"           -> ss date
	"OBJECT_NAME"    -> ss (object_name object)
	"MODULE_DOCUMENTATION" -> ss "$MODULE_DOCUMENTATION"
        "MODULE_TODO"    -> genTodoItems object
        "MODULE_NAME"    -> ss (modPrefix ++ object_name object)
        "MODULE_EXPORTS" -> genExports object apiDoc
	"MODULE_IMPORTS" -> ss "$imports"
	"CONTEXT_LIB"    -> ss lib
	"CONTEXT_PREFIX" -> ss prefix
	"MODULE_BODY"    -> doVersionIfDefs (genConstructors object apiDoc
                                          ++ genMethods object apiDoc)
	_ -> ss ""
    ) "") [ object | namespace <- api,
		     object <- namespace_objects namespace ]
    

usage = do
  putStr "\nProgram to generate a .chs Haskell binding module from an xml\n\
	\description of a GObject-style API. Usage:\n\
	\ApiGen <xmlFile> <templateFile>\n\
	\         {--doc=<docFile>} {--lib=<lib>} {--prefix=<prefix>}\n\
	\         {--outdir=<outDir>} {--modprefix=<modPrefix>}\n\
	\where\n\
	\  <apiFile>       an xml api file produced by gapi2xml\n\
	\  <templateFile>  is the name and path of the output template file\n\
	\  <outDir>        is the name and path of the output file\n\
	\  <docFile>       api doc file output from format-doc.xsl\n\
	\  <lib>           set the lib to use in the c2hs {#context #}\n\
	\                  declaration (the default is \"gtk\")\n\
	\  <prefix>        set the prefix to use in the c2hs {#context #}\n\
	\                  declaration (the default is \"gtk\")\n\
	\  <modPrefix>     specify module name prefix, eg if using\n\
	\                  hierarchical module names\n"
  exitWith $ ExitFailure 1


-------------------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------------------

ss = showString
sc = showChar

nl = sc '\n'

indent :: Int -> ShowS
indent c = ss ("\n"++replicate (2*c) ' ')

comment :: ShowS
comment = ss "-- "

lowerCaseFirstChar :: String -> String
lowerCaseFirstChar (c:cs) = toLower c : cs

upperCaseFirstChar :: String -> String -> String
upperCaseFirstChar _ (c:cs) = toUpper c : cs
upperCaseFirstChar dbgMesg cs = error $ "upperCaseFirstChar: " ++ dbgMesg ++ cs ++ " !!"

cat :: [ShowS] -> ShowS
cat = foldl (.) id

lines :: [ShowS] -> ShowS
lines []     = id
lines [x]    = x
lines (x:xs) = x. sc '\n'. lines xs

sepBy :: String -> [String] -> ShowS
sepBy s []     = id
sepBy s [x]    = ss x
sepBy s (x:xs) = ss x. ss s. sepBy s xs

sepBy' :: String -> [ShowS] -> ShowS
sepBy' s []     = id
sepBy' s [x]    = x
sepBy' s (x:xs) = x. ss s. sepBy' s xs

templateSubstitute :: String -> (String -> ShowS) -> ShowS
templateSubstitute template varSubst = doSubst template
  where doSubst [] = id
        doSubst ('@':cs) = let (var,_:cs') = span ('@'/=) cs
                            in varSubst var . doSubst cs'
        doSubst (c:cs) = sc c . doSubst cs

