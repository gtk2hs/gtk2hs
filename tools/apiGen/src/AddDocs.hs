module AddDocs (
  addDocsToModule,
  mkModuleDocMap,
  handleDocNULLs,
  fixModuleHierarchy,
  ) where

import Module
import qualified Api (Parameter(..))
import qualified Docs
import HaddockDocs (Section(..), Para(..), Span(..))
import qualified MarshalFixup (fixModuleDocMapping, knownMiscType
                              ,cTypeNameToHSType, fixCFunctionName
                              ,maybeNullParameter, maybeNullResult)
import Marshal (CSymbol(..), KnownSymbols)
import Names
import Utils

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Version (Version(..))
import qualified Data.Char as Char (toLower, isSpace, isAlpha)
import qualified Data.List as List (lines, span, intersperse)
import Data.Tree (Forest, Tree(Node))
import Data.Maybe (isJust)

import Prelude hiding (words)

addDocsToModule :: KnownSymbols -> Map String Docs.ModuleDoc -> Module -> Module
addDocsToModule knownSymbols moduleDocMap module_ =
  case Map.lookup (MarshalFixup.fixModuleDocMapping (module_cname module_)) moduleDocMap of
    Nothing -> module_ {
        module_summary     = [],
        module_description = [],
        module_hierarchy   = []
      }
    Just doc ->
      let methodDocMap    = mkDeclDocMap Docs.funcdoc_name (Docs.moduledoc_functions doc)
          propDocMap      = mkDeclDocMap Docs.propdoc_name (Docs.moduledoc_properties doc)
          childPropDocMap = mkDeclDocMap Docs.propdoc_name (Docs.moduledoc_childprops doc)
          signalDocMap    = mkDeclDocMap (canonicalSignalName.Docs.signaldoc_name)
                                         (Docs.moduledoc_signals doc)
          
          endDocIndex = 1 + length (Docs.moduledoc_functions doc)

          decls = flip map (module_decls module_) $ \decl ->
            case decl_body decl of
              method@Method { method_cname = name } -> 
                case Map.lookup name methodDocMap of
                  Nothing -> decl { decl_index_doc = endDocIndex }
                  Just (n, Docs.FuncDoc { Docs.funcdoc_paragraphs = fundoc,
                                          Docs.funcdoc_params = paramDocs,
                                          Docs.funcdoc_since  = since })
                          -> decl { decl_doc = Just (convertParas knownSymbols fundoc),
                                    decl_index_doc = n,
                                    decl_body = method {
                                      method_param_docs = map convertParmDoc paramDocs
                                    },
                                    decl_since = parseVersion since
                             }

              AttributeProp { attribute_is_child = False,
                              attribute_cname = name } ->
                case Map.lookup name propDocMap of
                  Nothing -> decl
                  Just (n, Docs.PropDoc { Docs.propdoc_paragraphs = fundoc,
                                          Docs.propdoc_since = since })
                          -> decl { decl_doc = Just (convertParas knownSymbols fundoc),
                                    decl_index_doc = n,
                                    decl_since = parseVersion since }

              AttributeProp { attribute_is_child = True,
                              attribute_cname = name } ->
                case Map.lookup name childPropDocMap of
                  Nothing -> decl
                  Just (n, Docs.PropDoc { Docs.propdoc_paragraphs = fundoc,
                                          Docs.propdoc_since = since })
                          -> decl { decl_doc = Just (convertParas knownSymbols fundoc),
                                    decl_index_doc = n,
                                    decl_since = parseVersion since }

              Signal { signal_cname = name } ->
                case Map.lookup (canonicalSignalName name) signalDocMap of
                  Nothing -> decl
                  Just (n, Docs.SignalDoc { Docs.signaldoc_paragraphs = fundoc,
                                            Docs.signaldoc_since = since })
                          -> decl { decl_doc = Just (convertParas knownSymbols fundoc),
                                    decl_index_doc = n,
                                    decl_since = parseVersion since }
              _ -> decl
            
          modsince = case map Docs.funcdoc_since (Docs.moduledoc_functions doc) of
                    [] -> ""
                    versions -> minimum versions

       in module_ {
            module_summary = convertParas knownSymbols (Docs.moduledoc_summary doc),
            module_description = 
                Section 1 "Detail" (convertParas knownSymbols (Docs.moduledoc_description doc))
              : convertSections knownSymbols (Docs.moduledoc_sections doc),
            module_hierarchy = convertHierarchy (Docs.moduledoc_hierarchy doc),
            module_decls = decls,
            module_since = parseVersion modsince
          }

  where mkDeclDocMap :: (doc -> String) -> [doc] -> Map String (Int, doc)
        mkDeclDocMap key docs =
          Map.fromList [ (key doc, (n, doc)) | (n, doc) <- zip [1..] docs ]

        parseVersion "" = Nothing
        parseVersion v  = Just Version {
                            versionBranch = map read (splitBy '.' v),
                            versionTags = []
                          }
        
        convertHierarchy :: Forest String -> Forest [Span]
        convertHierarchy =
            map (fmap $ \s -> let s' = MarshalFixup.cTypeNameToHSType s
                               in if s == module_cname module_
                                    then [SpanText  s']
                                    else [SpanIdent s'])
          . filterForest (\s -> s == "GInterface"
                             || not (null s)
                             && s /= "GInitiallyUnowned"
                             && isJust (Map.lookup s knownSymbols))
        
        convertParmDoc :: Docs.ParamDoc -> (String, [Span])
        convertParmDoc Docs.ParamDoc {
          Docs.paramdoc_name      = name,
          Docs.paramdoc_paragraph = spans
        } = (name, convertSpans knownSymbols spans)

        filterForest :: (a -> Bool) -> Forest a -> Forest a
        filterForest = concatMap . filterTree

        filterTree :: (a -> Bool) -> Tree a -> Forest a
        filterTree p (Node x ts) | p x       = [Node x (filterForest p ts)]
                                 | otherwise = filterForest p ts

mkModuleDocMap :: Docs.ApiDoc -> Map String Docs.ModuleDoc
mkModuleDocMap apiDoc =
  Map.fromList $ [ (Docs.moduledoc_name    moduleDoc, moduleDoc) | moduleDoc <- apiDoc ]
              ++ [ (Docs.moduledoc_altname moduleDoc, moduleDoc) | moduleDoc <- apiDoc ]


handleDocNULLs :: Module -> Module
handleDocNULLs module_@Module {
    module_description = description,
    module_decls       = decls
  } =
  module_ {
    module_description = map fixSection description,
    module_decls       = map fixDecl decls
  }
  where fixSection (Section level title paras) =
                    Section level title (map (fixPara False) paras)
        fixDecl decl@Decl {
            decl_doc = Just doc,
            decl_body = method@Method {
              method_cname = cname,
              method_parameters = params,
              method_param_docs = param_docs
            }
          } = let nullsAllFixed =
                       MarshalFixup.maybeNullResult cname
                    || or [ MarshalFixup.maybeNullParameter
                              cname (cParamNameToHsName pname)
                          | Api.Parameter {
                              Api.parameter_name = pname
                            } <- params ]
               in decl {
                    decl_doc = Just (map (fixPara nullsAllFixed) doc),
                    decl_body = method {
                      method_param_docs = 
                        [ (name, concatMap (fixSpan nullsAllFixed) spans)
                        | (name, spans) <- param_docs]
                    }
                  }
        fixDecl decl@Decl {
            decl_doc = Just doc
          } = decl {
                decl_doc = Just (map (fixPara False) doc)
              }
        fixDecl decl = decl
        
        fixPara :: Bool -> Para -> Para
        fixPara fixed (ParaText spans) = ParaText (concatMap (fixSpan fixed) spans)
        --TODO: other para kinds
        fixPara _ para = para
        
        fixSpan :: Bool -> Span -> [Span]
        fixSpan True  (SpanMonospace [SpanText "NULL"]) = [SpanMonospace [SpanText "Nothing"]]
        fixSpan False (SpanMonospace [SpanText "NULL"]) = fixme
        fixSpan fixed (SpanMonospace spans)             = [SpanMonospace (concatMap (fixSpan fixed) spans)]
        fixSpan True  (SpanText "NULL")      = [SpanMonospace [SpanText "Nothing"]]
        fixSpan False (SpanText "NULL")      = fixme
        fixSpan _ span = [span]
        
        fixme = [SpanText "{"
                ,SpanMonospace [SpanText "NULL"]
                ,SpanText ", FIXME: this should probably be "
                ,SpanText "converted to a Maybe data type}"]


fixModuleHierarchy :: Module -> Module
fixModuleHierarchy
  module_@Module {
    module_description = description,
    module_hierarchy   = hierarchy,
    module_cname       = cname
  } =
  module_ {
    module_description = description ++ convertToSection hierarchy
  }
  where convertToSection :: Forest [Span] -> [Section]
        convertToSection [] = []
        convertToSection hierarchy =
          [Section 1 "Class Hierarchy" [ParaCode (formatHierarchy hierarchy)]]
          
        formatHierarchy :: Forest [Span] -> [[Span]]
        formatHierarchy = map ((SpanText "|  "):)
                        . concatMap drawHierarchy

        drawHierarchy :: Tree [Span] -> [[Span]]
        drawHierarchy (Node x ts0) = x : drawSubTrees ts0
          where drawSubTrees [] = []
                drawSubTrees (t:ts) =
                  shift (SpanText " +----") (SpanText "      ")
                        (drawHierarchy t) ++ drawSubTrees ts
                shift :: a -> a -> [[a]] -> [[a]]
                shift first other = zipWith (:) (first : repeat other)


convertSections :: KnownSymbols -> [Docs.DocSection] -> [Section]
convertSections knownSymbols = map $
  \Docs.DocSection {
     Docs.section_title = title,
     Docs.section_paras = paras
   } -> Section 2 title (convertParas knownSymbols paras)

convertParas :: KnownSymbols -> [Docs.DocPara] -> [Para]
convertParas knownSymbols = map (convertPara knownSymbols)

convertPara :: KnownSymbols -> Docs.DocPara -> Para
convertPara knownSymbols para = case para of
  Docs.DocParaText    spans -> ParaText (convertSpans knownSymbols spans)
  Docs.DocParaProgram prog  -> ParaVerbatm (fixme : "" : trimBlankLines prog)
  Docs.DocParaTitle   title -> ParaTitle title
  Docs.DocParaDefItem term def -> ParaDefItem (convertSpans knownSymbols term)
                                              (convertSpans knownSymbols def)
  Docs.DocParaListItem spans -> ParaListItem (convertSpans knownSymbols spans)
  where fixme = "FIXME: if the follwing is a C code example"
             ++ ", port it to Haskell or remove it"
        trimBlankLines = reverse . dropWhile (all Char.isSpace)
                       . reverse . dropWhile (all Char.isSpace) . lines


convertSpans :: KnownSymbols -> [Docs.DocParaSpan] -> [Span]
convertSpans knownSymbols =
    concatMap mungeTextSpans
  . map (convertSpan knownSymbols)
  . concatMap fixDocTypeXRef
  where mungeTextSpans (SpanText text) = mungeWords knownSymbols text
        mungeTextSpans (SpanMonospace [SpanText text]) =
                         [SpanMonospace (mungeWords knownSymbols text)]
        mungeTextSpans span = [span]

        fixDocTypeXRef span@(Docs.DocTypeXRef word) =
          case List.span (/= ':') word of
            (_,     [])            -> [span]
            (word', remainder)     -> [Docs.DocTypeXRef word'
                                      ,Docs.DocText remainder]
        fixDocTypeXRef span = [span]


convertSpan :: KnownSymbols -> Docs.DocParaSpan -> Span
convertSpan knownSymbols (Docs.DocText text)     = SpanText text
convertSpan knownSymbols (Docs.DocTypeXRef text) =
  case Map.lookup text knownSymbols of
    Nothing | text == "TRUE"  -> SpanMonospace [SpanText "True"]
            | text == "FALSE" -> SpanMonospace [SpanText "False"]
            | otherwise       -> SpanText  ("{" ++ text ++ ", FIXME: unknown type/value}")
    Just (SymObjectType _)    -> SpanIdent (MarshalFixup.cTypeNameToHSType text)
    Just (SymEnumType _)      -> SpanIdent (MarshalFixup.cTypeNameToHSType text)
    Just SymEnumValue         -> SpanIdent (cConstNameToHsName text)
    Just SymStructType        -> SpanText  ("{" ++ text ++ ", FIXME: struct type}")
    Just SymBoxedType         -> if MarshalFixup.knownMiscType text
                                   then SpanIdent (MarshalFixup.cTypeNameToHSType text)
                                   else SpanText ("{" ++ text ++ ", FIXME: boxed type}")
    Just SymClassType         -> SpanText  ("{" ++ text ++ ", FIXME: class type}")
    Just SymTypeAlias         -> SpanText  ("{" ++ text ++ ", FIXME: type alias}")
    Just SymCallbackType      -> SpanText  ("{" ++ text ++ ", FIXME: callback type}")
convertSpan _ (Docs.DocFuncXRef text)   = SpanIdent (cFuncNameToHsName text)
convertSpan _ (Docs.DocOtherXRef text)  = SpanIdent ("{FIXME: gtk-doc cross reference to:" ++ text ++ "}")
convertSpan _ (Docs.DocEmphasis text)   = SpanEmphasis text
convertSpan _ (Docs.DocLiteral "TRUE")  = SpanMonospace [SpanText "True"]
convertSpan _ (Docs.DocLiteral "FALSE") = SpanMonospace [SpanText "False"]
  --likely that something should be changed to a Maybe type if this is emitted:
convertSpan _ (Docs.DocLiteral "NULL") = SpanMonospace [SpanText "NULL"]
convertSpan knownSymbols (Docs.DocLiteral text) =
  case Map.lookup text knownSymbols of
    Nothing                       -> SpanMonospace [SpanText text]
    Just SymEnumValue             -> SpanIdent (cConstNameToHsName text)
    Just (SymObjectType _)        -> SpanIdent (MarshalFixup.cTypeNameToHSType text)
    _ -> SpanText ("{" ++ text ++ ", FIXME: unknown literal value}") --TODO fill in the other cases
convertSpan _ (Docs.DocArg text) = SpanMonospace [SpanText (cParamNameToHsName text)]


mungeWords :: KnownSymbols -> String -> [Span]
mungeWords knownSymbols =
    map (mungeWord knownSymbols)
  . splitWords
  . words

  where words :: String -> [Span]
        words [] = []
        words xs = case break Char.isSpace xs of
          (chunk,[])         -> SpanText chunk : []
          (chunk,rest)       ->
            case span Char.isSpace rest of
              (seps, rest)
                | null chunk -> SpanSpace (length seps) : words rest
                | otherwise  -> SpanText chunk
                              : SpanSpace (length seps) : words rest

        splitWords :: [Span] -> [Span]
        splitWords [] = []
        splitWords (span@(SpanText word) : spans) =
          case List.span isWordChar word of
            (_,     [])        -> span : splitWords spans
            (word', remainder) -> SpanText word' : SpanText remainder
                                       : splitWords spans
        splitWords (span:spans) = span : splitWords spans

        isWordChar c = Char.isAlpha c || c == '_' || c == '+'

mungeWord :: KnownSymbols -> Span -> Span
mungeWord knownSymbols (span@(SpanText word)) =
  case Map.lookup word knownSymbols of 
    Nothing
      | word == "GTK"      -> SpanText "Gtk+"
      | word == "GTK+"     -> SpanText "Gtk+"
      | word == "TRUE"     -> SpanMonospace [SpanText "True"]
      | word == "FALSE"    -> SpanMonospace [SpanText "False"]
      | word == "G_MAXINT" -> SpanMonospace [SpanText "("
                                            ,SpanIdent "maxBound"
                                            ,SpanText " :: Int)"]
      | otherwise          -> span

    Just type_ -> case type_ of
      (SymObjectType _) -> SpanIdent $ MarshalFixup.cTypeNameToHSType word
      (SymEnumType _)   -> SpanIdent $ MarshalFixup.cTypeNameToHSType word
      SymEnumValue      -> SpanIdent $ cConstNameToHsName word
      SymStructType     -> SpanText  $ "{" ++ word ++ ", FIXME: struct type}"
      SymBoxedType
        | MarshalFixup.knownMiscType word
                        -> SpanIdent $ MarshalFixup.cTypeNameToHSType word
        | otherwise     -> SpanText  $ "{" ++ word ++ ", FIXME: boxed type}"
      SymClassType      -> SpanText  $ "{" ++ word ++ ", FIXME: class type}"
      SymTypeAlias      -> SpanText  $ "{" ++ word ++ ", FIXME: type alias}"
      SymCallbackType   -> SpanText  $ "{" ++ word ++ ", FIXME: callback type}"

mungeWord _ span = span
