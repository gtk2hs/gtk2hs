-- ApiGen: takes an xml description of a GObject-style API and produces a .chs
-- binding module.

--module Main (main) where

import Prelude hiding (Enum, lines)
import Monad  (when)
import Maybe  (catMaybes, fromJust)
import Char   (toLower, toUpper, isAlpha, isAlphaNum, isUpper)
import List   (isPrefixOf, groupBy, sortBy)
import qualified List (lines)
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
    apidoc_name :: String,                 -- these docs apply to this object
    apidoc_summary :: String,              -- a one line summary
    apidoc_description :: [DocPara],       -- the main description
    apidoc_sections :: [DocSection],       -- any additional titled subsections
    apidoc_functions :: [FuncDoc]          -- documentation for each function
  }

noApiDoc = ApiDoc {
    apidoc_name = "",
    apidoc_summary = "",
    apidoc_description = [],
    apidoc_sections = [],
    apidoc_functions = []
  }

data DocSection = DocSection {
    section_title :: String,
    section_paras :: [DocPara]
  }

data FuncDoc = FuncDoc {
    funcdoc_name :: String,		-- C function name
    funcdoc_paragraphs :: [DocPara],	-- documentation markup
    funcdoc_since :: Maybe String	-- which version of the api the
  }					-- function is avaliable from, eg "2.4"

data DocPara =
    DocParaText [DocParaSpan]           -- an ordinary word-wrapped paragraph
  | DocParaProgram String               -- a verbatum section
  | DocParaExample String String        -- a verbatum section with a title
  | DocParaDefItem String [DocParaSpan] -- a definition list item
  | DocParaListItem [DocParaSpan]       -- a itemisted list item

data DocParaSpan = DocText String       -- just simple text
                 | DocFuncXRef String   -- cross reference to a function name
                 | DocTypeXRef String   -- cross reference to a type name
                 | DocOtherXRef String  -- xref format not directly supported
                 | DocEmphasis String   -- emphasised text, usually italic
		 | DocLiteral String    -- some literal like numbers
		 | DocArg  String       -- function argument names

extractDocumentation :: Xml.Document -> ApiDoc
extractDocumentation (Xml.Document _ _ (Xml.Elem "apidoc" [] (moduleinfo:functions))) =
  (extractDocModuleinfo moduleinfo) {
    apidoc_functions = map extractDocFunc functions
  }

extractDocModuleinfo :: Xml.Content -> ApiDoc
extractDocModuleinfo 
  (Xml.CElem (Xml.Elem "module" []
    [Xml.CElem (Xml.Elem "name" [] name)
    ,Xml.CElem (Xml.Elem "summary" [] summary)
    ,Xml.CElem (Xml.Elem "description" [] parasAndSections)
    ,Xml.CElem (Xml.Elem "object-hierarchy" [] _)]
  )) = 
  let (paras, sections) = span (\elem ->
        case elem of
          Xml.CElem (Xml.Elem "section" _ _) -> False
          _ -> True) parasAndSections
   in ApiDoc {
    apidoc_name = Xml.verbatim name,
    apidoc_summary = Xml.verbatim summary,
    apidoc_description = concatMap extractDocPara paras,
    apidoc_sections = map extractDocSection sections,
    apidoc_functions = undefined
  }

extractDocSection :: Xml.Content -> DocSection
extractDocSection
  (Xml.CElem (Xml.Elem "section" []
    (Xml.CElem (Xml.Elem "title" [] [Xml.CString _ title])
    :paras))) =
  DocSection {
    section_title = title,
    section_paras = concatMap extractDocPara paras
  }
extractDocSection other = error $ "extractDocSection: " ++ Xml.verbatim other

extractDocFunc :: Xml.Content -> FuncDoc
extractDocFunc
  (Xml.CElem (Xml.Elem "function" []
    [Xml.CElem (Xml.Elem "name" [] [Xml.CString _ name])
    ,Xml.CElem (Xml.Elem "since" [] since')
    ,Xml.CElem (Xml.Elem "doc" [] paras)]
  )) =
  let since = case since' of
                [] -> Nothing
		[Xml.CString _ since] -> Just since
   in FuncDoc {
          funcdoc_name = name,
	  funcdoc_paragraphs = concatMap extractDocPara paras,
	  funcdoc_since = since
        }
  
extractDocPara :: Xml.Content -> [DocPara]
extractDocPara (Xml.CElem elem@(Xml.Elem "para" [] _)) =
  case Xml.xmlUnEscape Xml.stdXmlEscaper elem of
    (Xml.Elem _ [] spans) -> extractDocPara' spans
extractDocPara (Xml.CElem (Xml.Elem "programlisting" _ content)) =
  let listing = concat [ str | (Xml.CString _ str) <- content ] in
  [DocParaProgram listing]
extractDocPara (Xml.CElem (Xml.Elem "example" _
                 [Xml.CElem (Xml.Elem "title" [] [Xml.CString _ title])
                 ,(Xml.CElem (Xml.Elem "programlisting" _ content))])) =
  let listing = concat [ str | (Xml.CString _ str) <- content ] in                 
  [DocParaExample title listing]
{-extractDocPara (Xml.CElem (Xml.Elem "example" _
                 [Xml.CElem (Xml.Elem "title" [] [Xml.CString _ title])
                 ,(Xml.CElem (Xml.Elem "programlisting" _ other))])) = error $ "extractDocPara: example1:\n" ++ Prelude.unlines (map ((++ "\n\n\nFOOBAR\n\n\n") . Xml.verbatim) other) ++ "\n len = " ++ show (length other)
extractDocPara (Xml.CElem (Xml.Elem "example" _ other)) = error $ "extractDocPara: example2:\n" ++ Xml.verbatim other ++ "\n len = " ++ show (length other)-}
extractDocPara other = error $ "extractDocPara: " ++ Xml.verbatim other

extractDocPara' :: [Xml.Content] -> [DocPara]
extractDocPara' = reconstructParas [] . map extractDocParaOrSpan
  where reconstructParas :: [DocParaSpan] -> [Either DocParaSpan DocPara] -> [DocPara]
        reconstructParas []    [] = []
        reconstructParas spans [] = [DocParaText (reverse spans)]
        reconstructParas spans (Left  span:rest) = reconstructParas (span:spans) rest
        reconstructParas []    (Right para:rest) = para : reconstructParas [] rest
        reconstructParas spans (Right para:rest) = DocParaText (reverse spans)
                                                 : para : reconstructParas [] rest

extractDocParaOrSpan :: Xml.Content -> Either DocParaSpan DocPara 
extractDocParaOrSpan (Xml.CElem (Xml.Elem "listitem" [] content)) =
  Right $ DocParaListItem (map extractDocParaSpan content)
extractDocParaOrSpan (Xml.CElem (Xml.Elem "definition" []
                       (Xml.CElem (Xml.Elem "term" [] [Xml.CString _ term])
                       :content))) =
  Right $ DocParaDefItem term (map extractDocParaSpan content)
extractDocParaOrSpan (Xml.CElem (Xml.Elem "programlisting" _ content)) =
  let listing = concat [ str | (Xml.CString _ str) <- content ] in
  Right $ DocParaProgram listing
extractDocParaOrSpan content@(Xml.CElem   _  ) = Left $ extractDocParaSpan content
extractDocParaOrSpan content@(Xml.CString _ _) = Left $ extractDocParaSpan content
extractDocParaOrSpan other = error $ "extractDocParaOrSpan: " ++ Xml.verbatim other

extractDocParaSpan :: Xml.Content -> DocParaSpan
extractDocParaSpan (Xml.CString _ text) = DocText text
extractDocParaSpan (Xml.CElem (Xml.Elem tag [] content)) =
  let text = concat [ str | (Xml.CString _ str) <- content ] in
  case tag of
    "xref-func"  -> DocFuncXRef text
    "xref-type"  -> DocTypeXRef text
    "xref-other" -> DocOtherXRef text
    "emphasis"   -> DocEmphasis text
    "literal"    -> DocLiteral text
    "arg"        -> DocArg text
    other -> error $ "extractDocParaSpan: other tag " ++ tag
extractDocParaSpan other = error $ "extractDocParaSpan: " ++ Xml.verbatim other
 
-------------------------------------------------------------------------------
-- Functions for formatting haddock documentation
-------------------------------------------------------------------------------

genModuleDocumentation :: ApiDoc -> ShowS
genModuleDocumentation apidoc =
  (if null (apidoc_description apidoc)
     then id
     else comment.ss "* Description".nl.
          comment.nl.
          comment.ss "| ".haddocFormatParas (apidoc_description apidoc).nl).
  (if null (apidoc_sections apidoc)
     then id
     else nl.comment.haddocFormatSections (apidoc_sections apidoc).nl.comment)

addVersionParagraphs :: NameSpace -> ApiDoc -> ApiDoc
addVersionParagraphs namespace apiDoc =
  apiDoc {
    apidoc_description = apidoc_description apiDoc ++ moduleVersionParagraph,
    apidoc_functions = functionVersionParagraphs moduleVersion (apidoc_functions apiDoc)
  }
  where functionVersionParagraphs :: Maybe String -> [FuncDoc] -> [FuncDoc]
        functionVersionParagraphs baseVersion funcdocs =
          [ if funcdoc_since funcdoc > baseVersion
              then funcdoc {
                     funcdoc_paragraphs = funcdoc_paragraphs funcdoc ++
                       let line = "Available since " ++ namespace_name namespace
                              ++ " version " ++ fromJust (funcdoc_since funcdoc)
                        in [DocParaText [DocText line]]
                   }
              else funcdoc
          | funcdoc <- funcdocs ]
  
        moduleVersionParagraph =
          case moduleVersion of
            Nothing -> []
            Just since ->
              let line = "Module available since " ++ namespace_name namespace
                      ++ " version " ++ since
               in [DocParaText [DocText line]]
  
        -- figure out if the whole module appeared in some version of gtk later 
        -- than the original version
        moduleVersion :: Maybe String
        moduleVersion = case [ funcdoc_since funcdoc
                             | funcdoc <- apidoc_functions apiDoc ] of
                          [] -> Nothing
                          versions -> minimum versions
  
haddocFormatSections :: [DocSection] -> ShowS
haddocFormatSections = 
    sepBy' "\n\n-- "
  . map (\section ->
         ss "* ". ss (section_title section). nl.
         comment.nl.
         comment.ss "| ".haddocFormatParas (section_paras section))

haddocFormatParas :: [DocPara] -> ShowS
haddocFormatParas =
    sepBy' "\n--\n-- "
  . map haddocFormatPara

haddocFormatPara :: DocPara -> ShowS
haddocFormatPara (DocParaText spans) = haddocFormatSpans spans
haddocFormatPara (DocParaProgram prog) =
    ((ss "* FIXME: port the follwing code example from C to Haskell or remove it".nl.
      comment).)
  . sepBy "\n-- > "
  . List.lines
  $ prog
haddocFormatPara (DocParaExample title prog) =
    ((ss "* ". ss title.nl.
      comment).)
  . sepBy "\n-- > "
  . List.lines
  $ prog
haddocFormatPara (DocParaDefItem term spans) =
  sc '['. ss term. sc ']'.
  haddocFormatSpans spans
haddocFormatPara (DocParaListItem spans) =
  ss "* ".
  haddocFormatSpans spans

haddocFormatSpans :: [DocParaSpan] -> ShowS
haddocFormatSpans =
    sepBy' "\n-- "
  . map (sepBy " ")
  . wrapText 77
  . words
  . concatMap haddocFormatSpan

haddocFormatSpan :: DocParaSpan -> String
haddocFormatSpan (DocText text)    = escapeHaddockSpecialChars text
haddocFormatSpan (DocTypeXRef text)    = "\"" ++ stripKnownPrefixes text ++ "\""
haddocFormatSpan (DocFuncXRef text)    = "'" ++ cFuncNameToHsName text ++ "'"
haddocFormatSpan (DocOtherXRef text)    = "'{FIXME: gtk-doc cross reference to:" ++ text ++ "}'"
haddocFormatSpan (DocEmphasis text)  = "/" ++ text ++ "/"
haddocFormatSpan (DocLiteral "TRUE")  = "@True@"
haddocFormatSpan (DocLiteral "FALSE") = "@False@"
  --likely that something should be changed to a Maybe type if this is emitted:
haddocFormatSpan (DocLiteral "NULL")  = "{@NULL@, FIXME: this should probably be converted"
                                                     ++ "to a Maybe data type}"
haddocFormatSpan (DocLiteral text) = "@" ++ text ++ "@"
haddocFormatSpan (DocArg  text)    = "@" ++ text ++ "@"

cFuncNameToHsName :: String -> String
cFuncNameToHsName =
    lowerCaseFirstChar
  . stripKnownPrefixes
  . concatMap upperCaseFirstChar
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
genFunction :: Object -> Method -> Maybe FuncDoc -> ShowS
genFunction object method doc =
  formattedDoc.
  ss functionName. ss " :: ". functionType. nl.
  ss functionName. sc ' '. sepBy " " paramNames. ss " =".
  indent 1. body

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
          Just doc -> ss "-- | ". haddocFormatParas (funcdoc_paragraphs doc). nl.
                      comment. nl

genModuleBody :: Object -> ApiDoc -> ShowS
genModuleBody object apiDoc =
  doVersionIfDefs (sepBy' "\n\n") $
     genConstructors object (apidoc_functions apiDoc)
  ++ genMethods object (apidoc_functions apiDoc)

genMethods :: Object -> [FuncDoc] -> [(ShowS, Maybe FuncDoc)]
genMethods object apiDoc = 
  [ (genFunction object method doc, doc)
  | (method, doc) <- methods object apiDoc ]

methods :: Object -> [FuncDoc] -> [(Method, Maybe FuncDoc)]
methods object docs =
  map snd $
  sortBy (\(i,_) (j,_) -> i `compare` j)
  [ case method_cname method `lookup` docmap of
      Nothing             -> (0,(mungeMethod object method, Nothing))
      (Just (doc, index)) -> (index,(mungeMethod object method, Just doc))
  | method <- object_methods object
  , null [ () | VarArgs <- method_parameters method] ] --exclude VarArgs methods
  where docmap = [ (funcdoc_name doc, (doc,index)) | (doc,index) <- zip docs [1..]]
        

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

genConstructors :: Object -> [FuncDoc] -> [(ShowS, Maybe FuncDoc)]
genConstructors object apiDoc =
  [ (genFunction object constructor doc, doc)
  | (constructor, doc) <- constructors object apiDoc ]

constructors :: Object -> [FuncDoc] -> [(Method, Maybe FuncDoc)]
constructors object docs =
  [ (mungeConstructor object constructor, constructor_cname constructor `lookup` docmap)
  | constructor <- object_constructors object
  , null [ () | VarArgs <- constructor_parameters constructor] ]
  where docmap = [ (funcdoc_name doc, doc) | doc <- docs ]

mungeConstructor :: Object -> Constructor -> Method
mungeConstructor object constructor =
  Method {
    method_name = (object_name object++)
                . drop (length (object_cname object))
                . concatMap upperCaseFirstChar
                . splitBy '_'
                . constructor_cname
                $ constructor,
    method_cname = constructor_cname constructor,
    method_return_type = object_cname object ++ "*",
    method_parameters = constructor_parameters constructor
  }  

genExports :: Object -> [FuncDoc] -> ShowS
genExports object docs =
  nl.
  comment.ss "* Constructors".nl.
  doVersionIfDefs lines
    [ (ss "  ". ss (lowerCaseFirstChar (method_name constructor)). sc ',', doc)
    | (constructor, doc) <- constructors object docs].
  nl.
  nl.
  comment.ss "* Methods".nl.
  doVersionIfDefs lines
    [ (ss "  ". ss (lowerCaseFirstChar (method_name method)). sc ',', doc)
    | (method, doc) <- methods object docs]

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

doVersionIfDefs :: ([ShowS] -> ShowS) -> [(ShowS, Maybe FuncDoc)] -> ShowS
doVersionIfDefs lines =
    lines
  . map (\group -> sinceVersion (snd (head group))
                                (lines (map fst group)))
  . groupBy (\(_,a) (_,b) -> fmap funcdoc_since a == fmap funcdoc_since b)
 
sinceVersion :: Maybe FuncDoc -> ShowS -> ShowS
sinceVersion (Just (FuncDoc _ _ (Just (major:'.':minor:[])))) body =
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

genMarshalParameter name typeName | typeName == "const-gchar*"
                                 || typeName == "const-char*" =
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
  . concatMap upperCaseFirstChar
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
              then return noApiDoc
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
    (\(namespace, object) ->
    let apiDoc' = addVersionParagraphs namespace apiDoc in
    writeFile (outdir ++ object_name object ++ ".chs") $
    templateSubstitute template (\var ->
      case var of
	"YEAR"           -> ss year
	"DATE"           -> ss date
	"OBJECT_NAME"    -> ss (object_name object)
        "DESCRIPTION"    -> ss (apidoc_summary apiDoc')
	"DOCUMENTATION"  -> genModuleDocumentation apiDoc'
        "TODO"           -> genTodoItems object
        "MODULE_NAME"    -> ss (modPrefix ++ object_name object)
        "EXPORTS"        -> genExports object (apidoc_functions apiDoc')
	"IMPORTS"        -> ss "-- CHECKME: extra imports may be required\n"
	"CONTEXT_LIB"    -> ss (if null lib then namespace_library namespace else lib)
	"CONTEXT_PREFIX" -> ss (if null prefix then namespace_library namespace else  prefix)
	"MODULE_BODY"    -> genModuleBody object apiDoc'
	_ -> ss ""
    ) "") [ (namespace, object)
          | namespace <- api
	  , object <- namespace_objects namespace ]
    

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
	\                  declaration (the default is taken from the api file)\n\
	\  <prefix>        set the prefix to use in the c2hs {#context #}\n\
	\                  declaration (the default is taken from the api file)\n\
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

upperCaseFirstChar :: String -> String
upperCaseFirstChar (c:cs) = toUpper c : cs

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
        doSubst ('\\':'@':cs) = sc '@' . doSubst cs
        doSubst ('@':cs) = let (var,_:cs') = span ('@'/=) cs
                            in varSubst var . doSubst cs'
        doSubst (c:cs) = sc c . doSubst cs

