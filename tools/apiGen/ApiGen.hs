-- ApiGen: takes an xml description of a GObject-style API and produces a .chs
-- binding module. Optionally it can be supplied with an xml documentation file
-- in which case the .chs file will contain haddock-format documentation too.

-- If you want to teach ApiGen how to marshal new types, the function you want
-- to modify is either genMarshalParameter or genMarshalResult near the end of
-- this file.

module Main (main) where

import Prelude hiding (Enum, lines)
import qualified Prelude (lines)
import Monad  (when)
import Maybe  (catMaybes, fromJust)
import Char   (toLower, toUpper, isSpace, isAlpha, isAlphaNum, isUpper)
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
extractParameter (Xml.CElem (Xml.Elem "callback"
                        [("cname", Xml.AttValue cname)] _)) =
   Parameter {
     parameter_type = "callback",
     parameter_name = Xml.verbatim cname,
     parameter_isArray = False
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
-- Types representing the content of the documentation XML file
-------------------------------------------------------------------------------
type ApiDoc = [ModuleDoc]

data ModuleDoc = ModuleDoc {
    moduledoc_name :: String,              -- these docs apply to this object
    moduledoc_altname :: String,           -- sometimes a better index entry
    moduledoc_summary :: String,           -- a one line summary
    moduledoc_description :: [DocPara],    -- the main description
    moduledoc_sections :: [DocSection],    -- any additional titled subsections
    moduledoc_hierarchy :: [DocParaSpan],  -- a tree of parent objects (as text)
    moduledoc_functions :: [FuncDoc]       -- documentation for each function
  }

noModuleDoc = ModuleDoc {
    moduledoc_name = "",
    moduledoc_altname = "",
    moduledoc_summary = "",
    moduledoc_description = [],
    moduledoc_sections = [],
    moduledoc_hierarchy = [],
    moduledoc_functions = []
  }

data DocSection = DocSection {
    section_title :: String,
    section_paras :: [DocPara]
  }

data FuncDoc = FuncDoc {
    funcdoc_name :: String,		-- C function name
    funcdoc_paragraphs :: [DocPara],	-- documentation markup
    funcdoc_params :: [ParamDoc],	-- parameter documentation
    funcdoc_since :: Maybe String	-- which version of the api the
  }					-- function is available from, eg "2.4"

data ParamDoc = ParamDoc {
    paramdoc_name :: String,            -- parameter name or "Returns"
    paramdoc_paragraph :: [DocParaSpan] -- a simple paragraph
  }

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


-------------------------------------------------------------------------------
-- extract functions to convert the doc xml file to the internal representation
-------------------------------------------------------------------------------
extractDocumentation :: Xml.Document -> ApiDoc
extractDocumentation (Xml.Document _ _ (Xml.Elem "apidoc" [] modules)) =
  map extractDocModule modules

extractDocModule :: Xml.Content -> ModuleDoc
extractDocModule (Xml.CElem (Xml.Elem "module" [] (moduleinfo:functions))) =
  (extractDocModuleinfo moduleinfo) {
    moduledoc_functions = map extractDocFunc functions
  }

extractDocModuleinfo :: Xml.Content -> ModuleDoc
extractDocModuleinfo 
  (Xml.CElem (Xml.Elem "module-info" []
    [Xml.CElem (Xml.Elem "name" [] name)
    ,Xml.CElem (Xml.Elem "altname" [] altname)
    ,Xml.CElem (Xml.Elem "summary" [] summary)
    ,Xml.CElem (Xml.Elem "description" [] parasAndSections)
    ,Xml.CElem (Xml.Elem "object-hierarchy" [] objHierSpans)]
  )) = 
  let (paras, sections) = span (\elem ->
        case elem of
          Xml.CElem (Xml.Elem "section" _ _) -> False
          _ -> True) parasAndSections
   in ModuleDoc {
    moduledoc_name = Xml.verbatim name,
    moduledoc_altname = Xml.verbatim altname,
    moduledoc_summary = Xml.verbatim summary,
    moduledoc_description = concatMap extractDocPara paras,
    moduledoc_sections = map extractDocSection sections,
    moduledoc_hierarchy = map extractDocParaSpan objHierSpans,
    moduledoc_functions = undefined
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
    ,Xml.CElem (Xml.Elem "doc" [] paras)
    ,Xml.CElem (Xml.Elem "params" [] params)]
  )) =
  let since = case since' of
                [] -> Nothing
		[Xml.CString _ since] -> Just since
   in FuncDoc {
          funcdoc_name = name,
	  funcdoc_paragraphs = concatMap extractDocPara paras,
          funcdoc_params = map extractParamDoc params,
	  funcdoc_since = since
        }

extractParamDoc :: Xml.Content -> ParamDoc
extractParamDoc 
  (Xml.CElem (Xml.Elem "param" []
    (Xml.CElem (Xml.Elem "name" [] [Xml.CString _ name])
    :spans))) =
  ParamDoc {
    paramdoc_name = name,
    paramdoc_paragraph = map extractDocParaSpan spans
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

extractDocParaSpan other@(Xml.CRef (Xml.RefEntity entity)) = DocText (Xml.verbatim other)
extractDocParaSpan other = error $ "extractDocParaSpan: " ++ Xml.verbatim other
 
-------------------------------------------------------------------------------
-- Functions for formatting haddock documentation
-------------------------------------------------------------------------------

genModuleDocumentation :: ModuleDoc -> ShowS
genModuleDocumentation moduledoc =
  (if null (moduledoc_description moduledoc)
     then id
     else comment.ss "* Description".nl.
          comment.nl.
          comment.ss "| ".haddocFormatParas (moduledoc_description moduledoc).nl).
  (if null (moduledoc_sections moduledoc)
     then id
     else nl.comment.haddocFormatSections (moduledoc_sections moduledoc).nl.comment)
  -- Getting decent formatting for the class hierarchy does not seem to be
  -- possible with the available haddock markup. So disabling for now.
  --
  .nl.
  (if null (moduledoc_hierarchy moduledoc)
     then id
     else nl.comment.ss "* Class Hierarchy".nl.
          comment.ss "|".nl.
          comment.ss "@".nl.
          comment.ss "|  ".haddocFormatHierarchy (moduledoc_hierarchy moduledoc).nl.
          comment.ss "@".nl)

haddocFormatHierarchy :: [DocParaSpan] -> ShowS
haddocFormatHierarchy =
    sepBy "\n-- |"
  . Prelude.lines
  . concatMap haddocFormatSpan

addVersionParagraphs :: NameSpace -> ModuleDoc -> ModuleDoc
addVersionParagraphs namespace apiDoc =
  apiDoc {
    moduledoc_description = moduledoc_description apiDoc ++ moduleVersionParagraph,
    moduledoc_functions = functionVersionParagraphs moduleVersion (moduledoc_functions apiDoc)
  }
  where functionVersionParagraphs :: Maybe String -> [FuncDoc] -> [FuncDoc]
        functionVersionParagraphs baseVersion funcdocs =
          [ if funcdoc_since funcdoc > baseVersion
              then funcdoc {
                     funcdoc_paragraphs = funcdoc_paragraphs funcdoc ++
                       let line = "* Available since " ++ namespace_name namespace
                              ++ " version " ++ fromJust (funcdoc_since funcdoc)
                        in [DocParaText [DocText line]]
                   }
              else funcdoc
          | funcdoc <- funcdocs ]
  
        moduleVersionParagraph =
          case moduleVersion of
            Nothing -> []
            Just since ->
              let line = "* Module available since " ++ namespace_name namespace
                      ++ " version " ++ since
               in [DocParaText [DocText line]]
  
        -- figure out if the whole module appeared in some version of gtk later 
        -- than the original version
        moduleVersion :: Maybe String
        moduleVersion = case [ funcdoc_since funcdoc
                             | funcdoc <- moduledoc_functions apiDoc ] of
                          [] -> Nothing
                          versions -> minimum versions
  
haddocFormatSections :: [DocSection] -> ShowS
haddocFormatSections = 
    sepBy' "\n\n-- "
  . map (\section ->
         ss "** ". ss (section_title section). nl.
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
  sc '['. ss term. ss "] ".
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
                                                     ++ " to a Maybe data type}"
haddocFormatSpan (DocLiteral text) = "@" ++ text ++ "@"
haddocFormatSpan (DocArg  text)    = "@" ++ cParamNameToHsName text ++ "@"

cFuncNameToHsName :: String -> String
cFuncNameToHsName =
    lowerCaseFirstChar
  . stripKnownPrefixes
  . concatMap upperCaseFirstChar
  . splitBy '_'
  . takeWhile ('('/=)

stripKnownPrefixes :: String -> String
stripKnownPrefixes ('G':'t':'k':remainder) = remainder
stripKnownPrefixes ('G':'d':'k':remainder) = remainder
stripKnownPrefixes ('P':'a':'n':'g':'o':remainder) = remainder
stripKnownPrefixes other = other

cParamNameToHsName :: String -> String
cParamNameToHsName  =          --change "gtk_foo_bar" to "gtkFooBar"
    lowerCaseFirstChar
  . concatMap upperCaseFirstChar
  . filter (not.null) --to ignore tailing underscores
  . splitBy '_'

changeIllegalNames :: String -> String
changeIllegalNames "type" = "type_"  --this is a common variable name in C but of
                                     --course a keyword in Haskell
changeIllegalNames other = other

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

  where functionName = cFuncNameToHsName (method_cname method)
	(classConstraints', paramTypes', paramMarshalers) =
	  unzip3 [ case genMarshalParameter
                          (changeIllegalNames (cParamNameToHsName (parameter_name p)))
	                  (parameter_type p) of
                     (c, ty, m) -> (c, (ty, parameter_name p), m)
		 | p <- method_parameters method ]
	classConstraints = [ c | Just c <- classConstraints' ]
	paramTypes = [ (paramType, lookup name paramDocMap)
                     | (Just paramType, name) <- paramTypes' ]
	paramNames = [ changeIllegalNames (cParamNameToHsName (parameter_name p))
		     | ((Just _, _), p) <- zip paramTypes' (method_parameters method) ]
	(returnType', returnMarshaler) =
		genMarshalResult (method_return_type method)
        returnType = (returnType', lookup "Returns" paramDocMap)
	functionType = (case classConstraints of
	                  []  -> id
			  [c] -> ss c. ss " => "
			  cs  -> sc '('. sepBy ", " classConstraints. ss ") => ").
                       formatParamTypes (paramTypes ++ [returnType])
	body = foldl (\body marshaler -> marshaler body)
                     call (paramMarshalers++[returnMarshaler])
	call = ss "{# call ". ss (method_cname method). ss " #}"
        formattedDoc = case doc of
          Nothing  -> ss "-- | \n-- \n"
          Just doc -> ss "-- | ". haddocFormatParas (funcdoc_paragraphs doc). nl.
                      comment. nl
        paramDocMap = case doc of
          Nothing  -> []
          Just doc -> [ (paramdoc_name paramdoc
                        ,(if paramdoc_name paramdoc == "Returns"
                           then [DocText "returns "]
                           else [DocArg (paramdoc_name paramdoc)
                                ,DocText " - "]
                         ) ++ paramdoc_paragraph paramdoc)
                      | paramdoc <- funcdoc_params doc ]
        
        formatParamTypes :: [(String, Maybe [DocParaSpan])] -> ShowS
        formatParamTypes paramTypes = format True False paramTypes
                                             -- True to indicate first elem
                                             -- False to mean previous param had no doc
          where format _    _ []                   = id
                format True _ ((t,Nothing)    :ts) =               ss t.
                                                     format False False ts
                format True _ ((t,Just doc)   :ts) = ss "\n    ". ss t.
                                                     ss (replicate (columnIndent - length t) ' ').
                                                     ss " -- ^ ". formatDoc t doc.
                                                     format False True  ts
                format _ True  ((t, Nothing)  :ts) = ss "\n -> ". ss t.
                                                     format False False ts
                format _ False ((t, Nothing)  :ts) = ss   " -> ". ss t.
                                                     format False False ts
                format _ _     ((t, Just doc) :ts) = ss "\n -> ". ss t.
                                                     ss (replicate (columnIndent - length t) ' ').
                                                     ss " -- ^ ". formatDoc t doc.
                                                     format False True  ts
                formatDoc :: String -> [DocParaSpan] -> ShowS
                formatDoc typeName =
                    sepBy' ("\n" ++ replicate (columnIndent+5) ' ' ++  "-- ")
                  . map (sepBy " ")
                  . wrapText (80 - columnIndent - 8)
                  . words
                  . concatMap haddocFormatSpan
                columnIndent = maximum [ length parmType | (parmType, _) <- paramTypes ]

genModuleBody :: Object -> ModuleDoc -> ShowS
genModuleBody object apiDoc =
  doVersionIfDefs (sepBy' "\n\n") $
     genConstructors object (moduledoc_functions apiDoc)
  ++ genMethods object (moduledoc_functions apiDoc)

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
               parameter_name = "self",
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
    method_name = cFuncNameToHsName (constructor_cname constructor),
    method_cname = constructor_cname constructor,
    method_return_type = object_cname object ++ "*",
    method_parameters = constructor_parameters constructor
  }  

genExports :: Object -> [FuncDoc] -> ShowS
genExports object docs =
  comment.ss "* Types".
  indent 1.ss (object_name object).sc ','.
  indent 1.ss (object_name object).ss "Class,".
  indent 1.ss "castTo".ss (object_name object).sc ','.nl.
  nl.
  (case [ (ss "  ". ss (cFuncNameToHsName (method_cname constructor)). sc ',', doc)
        | (constructor, doc) <- constructors object docs] of
     [] -> id
     cs -> comment.ss "* Constructors".nl.
           doVersionIfDefs lines cs.nl).
  nl.
  (case [ (ss "  ". ss (cFuncNameToHsName (method_cname method)). sc ',', doc)
        | (method, doc) <- methods object docs] of
     [] -> id
     cs -> comment.ss "* Methods".nl.
           doVersionIfDefs lines cs)

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
sinceVersion (Just (FuncDoc _ _ _ (Just (major:'.':minor:[])))) body =
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
	\body -> body.
                 indent 2. ss " (fromIntegral ". ss name. ss ")")

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
genMarshalResult "gchar*"        = ("IO String", \body -> body.
                                                          indent 1. ss  ">>= readUTFString")
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
	    "EXPORTS"        -> genExports object (moduledoc_functions moduleDoc)
	    "IMPORTS"        -> ss $ "{#import Graphics.UI.Gtk.Types#}\n"
                                  ++ "-- CHECKME: extra imports may be required\n"
	    "CONTEXT_LIB"    -> ss (if null lib then namespace_library namespace else lib)
	    "CONTEXT_PREFIX" -> ss (if null prefix then namespace_library namespace else  prefix)
	    "MODULE_BODY"    -> genModuleBody object moduleDoc
	    _ -> ss "" ) ""
    ) [ (namespace, object, lookup (object_cname object) apiDocMap)
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
