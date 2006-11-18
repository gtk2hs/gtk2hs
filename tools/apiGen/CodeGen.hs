module CodeGen (
  genModuleBody,
  genTodoItems,
  makeKnownSymbolsMap
  ) where

import Module       (Module(..), Decl(..), DeclBody(..), isAttr)
import qualified Api
import qualified HaddockDocs (Span(..), formatParasFragment, formatSections,
                     formatParas, formatSpans, haddockSection)
import HaddockDocs  (Span(..))
import Marshal      (CSymbol(..), ParameterKind(..), EnumKind(..),
                     KnownSymbols, genMarshalParameter, genMarshalResult,
                     genMarshalOutParameter, genCall, genMarshalProperty,
                     convertSignalType)
import Names        (cParamNameToHsName, cFuncNameToHsName, hsTypeNameToCGetType)
import Utils
import MarshalFixup (maybeNullParameter, maybeNullResult, leafClass,
                     nukeParameterDocumentation)

import Prelude hiding (Enum, lines)
import Data.List    (groupBy, sortBy, partition, intersperse)
import Data.Maybe   (fromMaybe, catMaybes, isNothing)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Version

import Numeric (showHex)
import Data.List (foldl')
import Data.Word (Word16)
import Data.Char (ord)

-------------------------------------------------------------------------------
-- More doc formatting utils
-------------------------------------------------------------------------------

deprecated name comment =
  pragma $ text "DEPRECATED" <+> name <+> doubleQuotes comment

pragma d = text "{-#" <+> d <+> text "#-}"
c2hsHook name d = text "{#" <+> text name <+> d <+> text "#}"

tuple []  = empty
tuple [x] = x
tuple  xs = parens (hsep $ punctuate comma xs)

tuple' xs = parens (hsep $ punctuate comma xs)

-------------------------------------------------------------------------------
-- Now lets actually generate some code fragments based on the api info
-------------------------------------------------------------------------------

genDecl :: KnownSymbols -> Decl -> Doc
genDecl knownSymbols decl =
     hashes
  $$ formattedDocs
  $$ formattedCode
  $$ deprecatedNote

  where
    generatedDocs =
      case decl_doc decl of
        Nothing  -> empty
        Just []  -> comment <+> char '|'
                 $$ comment
        Just doc -> HaddockDocs.formatParas 77 doc
                 $$ comment
    handWrittenDocs = vcat (map text $ decl_user_docs decl)
    generatedDocsHash   = hash generatedDocs
    handWrittenDocsHash = hash handWrittenDocs
    formattedDocs
      | decl_user_docs_hash decl
     == generatedDocsHash = handWrittenDocs
      | otherwise         = generatedDocs

    generatedCode = genDeclCode knownSymbols decl
    handWrittenCode = vcat (map text $ decl_user_code decl)
    generatedCodeHash   = hash generatedCode
    handWrittenCodeHash = hash handWrittenCode
    formattedCode
      | decl_user_code_hash decl
     == generatedCodeHash = handWrittenCode
      | otherwise         = generatedCode

    dhash | generatedDocsHash == handWrittenDocsHash = empty
          | otherwise = text "d:" <> text generatedDocsHash
    chash | generatedCodeHash == handWrittenCodeHash = empty
          | otherwise = text "c:" <> text generatedCodeHash
    hashes | isEmpty chash && isEmpty dhash = empty
           | otherwise = comment <+> text "%hash" <+> chash <+> dhash
--                    $$ comment <+> text "%hash" <+> text "c:" <> text (decl_user_code_hash decl)
--                                                <+> text "d:" <> text (decl_user_docs_hash decl)

    hash :: Doc -> String
    hash = ($[])
         . showHex
         . foldl' (\h c -> h * 33 + (fromIntegral (ord c)))
                  (5381 :: Word16)
         . render

    deprecatedNote
      | decl_deprecated decl
                  = deprecated (text $ decl_name decl)
                               (text $ decl_deprecated_comment decl)
      | otherwise = empty

genDeclCode :: KnownSymbols -> Decl -> Doc
genDeclCode knownSymbols Decl{ decl_body = method@(Method {}) } =
     text functionName <+> text "::" <+> classContext <+> firstLineParamsType
  $$ nest 1 multiLineParamsType
  $$ text functionName <+> formattedParamNames <+> equals
  $$ nest 2 codebody

  where functionName = cFuncNameToHsName (method_cname method)
	(classConstraints, paramTypes', paramMarshalers) =
	  unzip3 [ case genMarshalParameter knownSymbols (method_cname method)
                          (changeIllegalNames (cParamNameToHsName (Api.parameter_name p)))
	                  (Api.parameter_type p) of
                     (c, ty, m) -> (c, (ty, Api.parameter_name p), m)
		 | p <- method_parameters method ]
	inParamTypes = [ (paramType, lookup name paramDocMap)
                     | (InParam paramType, name) <- paramTypes' ]
	inParamNames = [ changeIllegalNames (cParamNameToHsName (Api.parameter_name p))
		     | ((InParam _, _), p) <- zip paramTypes' (method_parameters method) ]
	outParamTypes = [ (paramType, lookup name paramDocMap)
                        | (OutParam paramType, name) <- paramTypes' ]
        formattedParamNames = hsep $ map text inParamNames
	(returnType', returnMarshaler) =
		genMarshalResult knownSymbols (method_cname method)
                                  (method_is_constructor method) (method_return_type method)
        returnType | null outParamTypes  = ("IO " ++ returnType', lookup "Returns" paramDocMap)
		   | otherwise = case unzip outParamTypes of
                                   (types', docs') ->
				     let types | returnType' == "()" = types'
				               | otherwise           = returnType' : types'
					 docs = mergeParamDocs (lookup "Returns" paramDocMap) docs'
				      in (case types of
				            [t] -> "IO " ++ t
					    _   -> "IO (" ++ concat (intersperse ", " types) ++ ")"
					 ,docs)
	(outParamMarshalersBefore, outParamMarshalersAfter, returnOutParamFragments) =
             unzip3 [ genMarshalOutParameter outParamType (changeIllegalNames (cParamNameToHsName name))
                    | (OutParam outParamType, name) <- paramTypes' ]
        returnOutParams body | null outParamTypes = body
                             | otherwise = body
                                        $$ text "return" <+> tuple' returnOutParamFragments
	codebody = foldl (\body marshaler -> marshaler body)
                     call (paramMarshalers
                       ++ [ (\body -> frag $$ body) | frag <- reverse outParamMarshalersBefore ]
                       ++ [ (\body -> body $$ frag) | frag <- outParamMarshalersAfter ]
                       ++ [returnMarshaler,returnOutParams])
	call = genCall (fromMaybe (method_cname method) (method_shortcname method))
                       (method_is_unsafe_ffi method)
        docNullsAllFixed = maybeNullResult (method_cname method)
                        || or [ maybeNullParameter (method_cname method) (cParamNameToHsName (Api.parameter_name p))
                              | p <- method_parameters method ]
        paramDocMap = [ (name
                        ,(if name == "Returns"
                           then [SpanText "returns "]
                           else [SpanMonospace [SpanText (Names.cParamNameToHsName name)]
                                ,SpanText " - "]
                         ) ++ paragraph)
                      | (name, paragraph) <- method_param_docs method
		      , not $ nukeParameterDocumentation
                                (method_cname method)
                                (cParamNameToHsName name) ]

        classContext = case catMaybes classConstraints of
	                 []  -> empty
			 cs  -> tuple (map text cs) <+> text "=>"
        
        (firstLineParams, multiLineParams) = span (isNothing.snd) (inParamTypes ++ [returnType])
        
        firstLineParamsType :: Doc
        firstLineParamsType =
            hsep
          . intersperse (text "->")
          . map (text.fst)
          $ firstLineParams
        
        multiLineParamsType :: Doc
        multiLineParamsType =
            vcat
          . (\lines ->
             case lines of
               [] -> []
               (l:ls) | null firstLineParams -> nest 3 l : map (text "->" <+>) ls
                      | otherwise -> map (text "->" <+>) lines)
          . map (\(type_, doc) ->
            case doc of
              Nothing  -> text type_
              Just doc -> text type_
                       <> text (replicate (columnIndent - length type_) ' ')
                      <+> HaddockDocs.haddockSection (char '^')
                            (HaddockDocs.formatSpans 3 (80 - columnIndent - 8) doc))
          $ multiLineParams

        columnIndent = maximum [ length parmType | (parmType, Just _) <- multiLineParams ]

genDeclCode knownSymbols decl@(Decl{ decl_body = attr@(AttributeProp { attribute_is_child = False }) }) =
  genAtter decl propertyName classConstraint getterType setterType False (Right body)
  where propertyName = decl_name decl
        (propertyType, gvalueKind, needsGetTypeCCall) =
          genMarshalProperty knownSymbols (attribute_type attr)
        body = text attrType <> text "AttrFrom" <> text gvalueKind <> text "Property"
           <+> doubleQuotes (text (attribute_cname attr))
            $$ if needsGetTypeCCall
                  then let attrType  = attribute_type attr
                           ccall_name = text (hsTypeNameToCGetType attrType)
                        in nest 2 $ c2hsHook "call pure unsafe" ccall_name
                  else empty
          where attrType | attribute_readable attr
                        && attribute_writeable attr = "new"
                         | attribute_readable  attr = "read"
                         | attribute_writeable attr = "write"
        getterType | attribute_readable attr  = Just propertyType 
                   | otherwise                = Nothing
        (setterType, classConstraint)
                   | attribute_writeable attr 
                  && gvalueKind == "Object"  =
                    if leafClass (attribute_type attr)
                      then (Just propertyType, Nothing)
                      else let typeVar = lowerCaseFirstChar propertyType
                               classConstraint' = propertyType ++ "Class " ++ typeVar
                            in (Just typeVar, Just classConstraint')
                   | attribute_writeable attr = (Just propertyType, Nothing)
                   | otherwise                = (Nothing, Nothing)


genDeclCode knownSymbols decl@(Decl{ decl_body = attr@(AttributeProp { attribute_is_child = True }) }) =
  genAtter decl propertyName classConstraint getterType setterType True (Right body)
  where propertyName = decl_name decl
        (propertyType, gvalueKind, needsGetTypeCCall) =
          genMarshalProperty knownSymbols (attribute_type attr)
        body = text attrType <> text "AttrFromContainerChild" <> text gvalueKind <> text "Property"
           <+> doubleQuotes (text (attribute_cname attr))
            $$ if needsGetTypeCCall
                  then let attrType  = attribute_type attr
                           ccall_name = text (hsTypeNameToCGetType attrType)
                        in nest 2 $ c2hsHook "call pure unsafe" ccall_name
                  else empty
          where attrType | attribute_readable attr
                        && attribute_writeable attr = "new"
                         | attribute_readable  attr = "read"
                         | attribute_writeable attr = "write"
        getterType | attribute_readable attr  = Just propertyType 
                   | otherwise                = Nothing
        (setterType, classConstraint)
                   | attribute_writeable attr 
                  && gvalueKind == "Object"   =
                    if leafClass (attribute_type attr)
                      then (Just propertyType, Nothing)
                      else let typeVar = lowerCaseFirstChar propertyType
                               classConstraint' = propertyType ++ "Class " ++ typeVar ++ ", WidgetClass child"
                            in (Just typeVar, Just classConstraint')
                   | attribute_writeable attr = (Just propertyType, Just "WidgetClass child")
                   | otherwise                = (Nothing, Just "WidgetClass child")

genDeclCode knownSymbols decl@(Decl{ decl_body = attr@(AttributeGetSet {}) }) =
  genAtter decl propertyName classConstraint
           (Just getterType) (Just setterType) False
           (Left (text (decl_name getter), text (decl_name setter)))
  where propertyName = decl_name decl
        (getterType, _) = genMarshalResult knownSymbols (method_cname getter_body) False
                              (method_return_type getter_body)
        (classConstraint, setterType) =
          case let param = method_parameters setter_body !! 1
                   paramName = changeIllegalNames (cParamNameToHsName (Api.parameter_name param))
                   paramType = Api.parameter_type param
                in genMarshalParameter knownSymbols (method_cname setter_body) paramName paramType of
            (classConstraint', InParam setterType', _) -> (classConstraint', setterType')
            (_, OutParam _, _)  -> (Nothing, "{- FIXME: should be in param -}")
        getter@Decl { decl_body = getter_body } = attribute_getter attr
        setter@Decl { decl_body = setter_body } = attribute_setter attr

genDeclCode knownSymbols Decl{ decl_module = module_,
                               decl_name = signalName,
                               decl_body = signal@Module.Signal {} }
  | signal_is_old_style signal =
      text signalName <+> text "::" <+> oldSignalType
   $$ text signalName <+> equals <+> text "connect_" <> connectCall <+> signalCName <+> text (show $ signal_is_after signal)

  | otherwise =
      text (lowerCaseFirstChar signalName) <+> text "::" <+> signalType
   $$ text (lowerCaseFirstChar signalName) <+> equals <+> text "Signal" <+> parens (text "connect_" <> connectCall <+> signalCName)

  where connectCall = let paramCategories' = if null paramCategories then [text "NONE"] else map text paramCategories
                       in hcat (punctuate (char '_') paramCategories') <> text "__" <> text returnCategory
        -- strip off the object arg to the signal handler
        params = case Module.signal_parameters signal of
                   (param:params') | Api.parameter_type param
                                  == (module_cname module_) ++ "*" -> params'
                   params' -> params'
        (paramCategories, paramTypes) = unzip [ convertSignalType knownSymbols (Api.parameter_type parameter)
                                              | parameter <- params ]
        (returnCategory, returnType) = convertSignalType knownSymbols (Module.signal_return_type signal)
        signalType = text (module_name module_) <> text "Class self => Signal self" <+> parens callbackType
        oldSignalType = text (module_name module_) <> text "Class self => self"
                     $$ nest nestLevel (text "->" <+> callbackType
                             $$ text "->" <+> text "IO (ConnectId self)")
          where nestLevel = -(length signalName + 3)
        callbackType | null paramTypes = text "IO" <+> text returnType
                     | otherwise = parens (hsep . intersperse (text "->") . map text $ (paramTypes ++ ["IO " ++ returnType]))
        signalCName = doubleQuotes (text $ Module.signal_cname signal)

genDeclCode _
  Decl { decl_body = Instance { instance_class_name = className,
                                instance_type_name  = typeName }} =
  text "instance" <+> text className <+> text typeName


mergeParamDocs :: Maybe [Span] -> [Maybe [Span]] -> Maybe [Span]
mergeParamDocs doc docs =
  case catMaybes (doc:docs) of
    [] -> Nothing
    [doc'] -> Just doc'
    docs' -> let (varNames, paramDocs) =
                   unzip [ case doc' of 
                            (SpanMonospace [SpanText varName] : _)
                               -> (cParamNameToHsName varName, doc')
                            _  -> ("_", doc')
                         | doc' <- docs' ]
                 returnValName = SpanMonospace [SpanText $ "(" ++ concat (intersperse ", " varNames) ++ ")"]
                 fixmeMessage  = SpanText " {FIXME: merge return value docs} "
              in Just $ returnValName : fixmeMessage : concat paramDocs

changeIllegalNames :: String -> String
changeIllegalNames "type" = "type_"   --these are common variable names in C but
changeIllegalNames "where" = "where_" --of course are keywords in Haskell
changeIllegalNames "data" = "data_"
changeIllegalNames other = other

genModuleBody :: KnownSymbols -> Module -> Doc
genModuleBody knownTypes module_ =
     summary
  $$ comment
  $$ (if Module.module_deprecated module_
        then text "module" <+> moduleName
          $$ deprecatedNote <+> lparen
        else text "module" <+> moduleName <+> lparen)
 $+$ documentation
 $+$ exports
  $$ nest 2 (rparen <+> text "where")
 $+$ imports
 $+$ context
 $+$ decls
  
  where summary = HaddockDocs.formatParasFragment (module_summary module_)

        moduleName | isEmpty prefix = name
                   | otherwise      = prefix <> char '.' <> name
          where name = text (Module.module_name module_)
                prefix = text (Module.module_prefix module_)

        deprecatedNote | Module.module_deprecated module_ =
          deprecated empty (text "this module should not be used in newly-written code.")
                       | otherwise = empty

        documentation = HaddockDocs.formatSections (module_description module_)

        exports = genExports module_
        imports = genImports module_

        context = c2hsHook "context" $
                    text "lib" <> equals <> doubleQuotes (text $ module_context_lib module_)
                <+> text "prefix" <> equals <> doubleQuotes (text $ module_context_prefix module_)

        decls = genDecls knownTypes module_

genDecls :: KnownSymbols -> Module -> Doc
genDecls knownSymbols module_ =
    doVersionIfDefs vsep
  . map adjustDeprecatedAndSinceVersion
  $  sectionHeader "Interfaces"
     [ (genDecl knownSymbols decl, (since, deprecated))
     | decl@Decl { decl_since = since,
                   decl_deprecated = deprecated,
                   decl_body = Instance { }
       } <- module_decls module_ ]
  ++ sectionHeader "Constructors"
     [ (genDecl knownSymbols decl, (since, deprecated))
     | decl@Decl { decl_since = since,
                   decl_deprecated = deprecated,
                   decl_body = Method { method_is_constructor = True }
       } <- module_decls module_ ]
  ++ sectionHeader "Methods"
     [ (genDecl knownSymbols decl, (since, deprecated))
     | decl@Decl { decl_since = since,
                   decl_deprecated = deprecated,
                   decl_body = Method { method_is_constructor = False }
       } <- module_decls module_ ]
  ++ sectionHeader "Attributes"
     [ (genDecl knownSymbols decl, (since, deprecated))
     | decl@Decl { decl_since = since,
                   decl_deprecated = deprecated
       } <- module_decls module_, isAttr decl ]
  ++ sectionHeader "Child Attributes"
     [ (genDecl knownSymbols decl, (since, deprecated))
     | decl@Decl { decl_since = since,
                   decl_deprecated = deprecated,
                   decl_body = AttributeProp { attribute_is_child = True }
       } <- module_decls module_ ]
  ++ sectionHeader "Signals"
     [ (genDecl knownSymbols decl, (since, deprecated))
     | decl@Decl { decl_since = since,
                   decl_deprecated = deprecated,
                   decl_body = Module.Signal {}
       } <- module_decls module_ ]
  where sectionHeader _    []      = []
        sectionHeader name entries =
          let header = (text "--------------------"
                     $$ comment <+> text name, (Nothing, False))
           in header : entries
        adjustDeprecatedAndSinceVersion (doc, (since, deprecated)) =
          (doc, (module_since module_ `max` since, Module.module_deprecated module_ || deprecated))
        

genAtter :: Decl -> String
         -> Maybe String -> Maybe String -> Maybe String -> Bool
         -> Either (Doc, Doc) Doc -> Doc
genAtter Decl { decl_module = module_ }
         propertyName classConstraint getterType setterType isChild attrImpl =
    text propertyName <+> text "::" <+> classContext <+> child <+> attrType <+> objectParamType <+> attrArgs
 $$ text propertyName <+> equals <+> body
 $$ nest 2 body'
  where objectType = text (module_name module_)
        objectParamType | leafClass (module_cname module_) = objectType
                        | otherwise                        = text "self"
        classContext = case (leafClass (module_cname module_), classConstraint) of 
                         (True,  Nothing)              -> empty
                         (False, Nothing)              ->
                           objectType <> text "Class self =>"
                         (True,  Just classConstraint') ->
                           text classConstraint' <+> text "=>"
                         (False, Just classConstraint') ->
                           parens (objectType <> text "Class self" <> comma
                               <+> text classConstraint') <+> text "=>"
        (attrType, attrConstructor, attrArgs) =
          case (getterType, setterType) of
            (Just gt, Nothing)        -> (text "ReadAttr",     text "readAttr", text gt)
            (Nothing, Just st)        -> (text "WriteAttr",    text "writeAttr", text st)
            (Just gt, Just st)
              | gt == st              -> (text "Attr",          text "newAttr", text gt)
              | length (words st) > 1 -> (text "ReadWriteAttr", text "newAttr", text gt <+> parens (text st))
              | otherwise             -> (text "ReadWriteAttr", text "newAttr", text gt <+> text st)
	    _ -> error $ "no getter or setter for " ++ module_name module_ ++ " :: " ++ propertyName 
        child | isChild   = text "child" <+> text "->"
              | otherwise = empty
        body  = case attrImpl of
                  Left  _ -> attrConstructor
                  Right b -> b
        body' = case attrImpl of
                  Left (getter, setter) ->
                    case (getterType, setterType) of
                      (Just _, Nothing) -> getter
                      (Nothing, Just _) -> setter
                      (Just _,  Just _) -> getter $$ setter
                  Right _ -> empty

makeKnownSymbolsMap :: Api.API -> KnownSymbols
makeKnownSymbolsMap api =
   (Map.fromListWith (\a b -> b)
  . concat)
  [ [ (Api.enum_cname enum
      ,case Api.enum_variety enum of
        Api.EnumVariety -> SymEnumType EnumKind
        Api.FlagsVariety -> SymEnumType FlagsKind)
    | enum <- Api.namespace_enums namespace ]
 ++ [ (Api.object_cname object, objectKind object)
    | object <- Api.namespace_objects namespace ]
 ++ [ (Api.class_cname class_, SymClassType)
    | class_ <- Api.namespace_classes namespace ]
 ++ [ (Api.boxed_cname boxed, SymBoxedType)
    | boxed <- Api.namespace_boxed namespace ]
 ++ [ (Api.member_cname member, SymEnumValue)
    | enum <- Api.namespace_enums namespace
    , member <- Api.enum_members enum ]
 ++ [ (Api.misc_cname misc, miscToCSymbol misc )
    | misc <- Api.namespace_misc namespace ]
  | namespace <- api ]

        -- find if an object inherits via GtkObject or directly from GObject
  where objectKind :: Api.Object -> CSymbol
        objectKind object | "GObject" `elem` parents = SymObjectType parents
                          | Api.object_parent object == "GTypeInstance" = SymStructType
                          -- FIXME: These hacks should go elsewhere
                          | otherwise = SymObjectType [Api.object_cname object, "GObject"]
          where parents = objectParents object
        objectParents :: Api.Object -> [String]
        objectParents object = Api.object_cname object :
          case Api.object_parent object `Map.lookup` objectMap of
            Nothing -> [Api.object_parent object]
            Just parent -> objectParents parent
        objectMap :: Map String Api.Object
        objectMap = Map.fromList
                    [ (Api.object_cname object, object)
                    | namespace <- api
                    , object <- Api.namespace_objects namespace ]
        miscToCSymbol (Api.Struct   _ _) = SymStructType
        miscToCSymbol (Api.Alias    _ _) = SymTypeAlias
        miscToCSymbol (Api.Callback _ _) = SymCallbackType

genExports :: Module -> Doc
genExports module_ =
    doVersionIfDefs vcat
  . map adjustDeprecatedAndSinceVersion
  $  sectionHeader False "Types"
     [(text (Module.module_name module_), defaultAttrs)
     ,(text (Module.module_name module_) <> text "Class", defaultAttrs)
     ,(text "castTo" <> text (Module.module_name module_), defaultAttrs)
     ,(text "to"     <> text (Module.module_name module_), defaultAttrs)]
  ++ sectionHeader True "Constructors"
     [ (text name, (since, deprecated))
     | Decl { decl_name = name,
              decl_since = since,
              decl_deprecated = deprecated,
              decl_body = Method { method_is_constructor = True }
       } <- exports ]
  ++ sectionHeader True "Methods"
     [ (text name, (since, deprecated))
     | Decl { decl_name = name,
              decl_since = since,
              decl_deprecated = deprecated,
              decl_body = Method { method_is_constructor = False }
       } <- exports ]
  ++ sectionHeader True "Attributes"
     [ (text name, (since, deprecated))
     | decl@Decl { decl_since = since,
                   decl_deprecated = deprecated,
                   decl_name = name 
       } <- module_decls module_
       , isAttr decl ]
  ++ sectionHeader True "Child Attributes"
     [ (text name, (since, deprecated))
     | Decl { decl_name = name,
              decl_since = since,
              decl_deprecated = deprecated,
              decl_body = AttributeProp { attribute_is_child = True }
       } <- module_decls module_ ]
  ++ sectionHeader True "Signals"
     [ (text name, (since, deprecated))
     | Decl { decl_name = name,
              decl_since = since,
              decl_deprecated = deprecated,
              decl_body = Module.Signal { signal_is_old_style = False }
       } <- exports ]
  ++ sectionHeader True "Deprecated"
     [ (text name, (since, deprecated))
     | Decl { decl_name = name,
              decl_since = since,
              decl_deprecated = deprecated,
              decl_body = Module.Signal { signal_is_old_style = True }
       } <- exports ]

  where defaultAttrs = (Nothing, False)
        sectionHeader _ _    []      = []
        sectionHeader False name entries = (text "-- * " <> text name, defaultAttrs)
                                   : map (\(doc, attrs) -> (nest 2 (doc <> comma), attrs)) entries
        sectionHeader True name entries = (emptyLine, defaultAttrs)
                                   : (text "-- * " <> text name, defaultAttrs)
                                   : map (\(doc, attrs) -> (nest 2 (doc <> comma), attrs)) entries
        adjustDeprecatedAndSinceVersion (doc, (since, deprecated)) =
          (doc, (Module.module_since module_ `max` since, Module.module_deprecated module_ || deprecated))
        exports = sortBy (comparing decl_index_export) (module_decls module_)

genImports :: Module -> Doc
genImports module_ = 
  (case [ text importLine
        | (_, importLine) <- stdModules ] of
     []   -> empty
     mods -> vcat mods)
  $+$ vcat [ text importLine
           | (_, importLine) <- extraModules ]
  where (stdModules, extraModules)
          | null (Module.module_imports module_) =
             ([(undefined, "import Monad\t(liftM)")]
             ,[(undefined, "import System.Glib.FFI")
             ,(undefined, "{#import Graphics.UI.Gtk.Types#}")
             ,(undefined, "-- CHECKME: extra imports may be required")])
          | otherwise = partition (\(m, _) -> m `elem` knownStdModules)
                                  (Module.module_imports module_)
        knownStdModules = ["Maybe", "Monad", "Char", "List", "Data.IORef"]

genTodoItems :: Module -> Doc
genTodoItems Module { module_todos = varargsFunctions } 
  | null varargsFunctions = empty
  | otherwise =
       comment <+> text "TODO: the following varargs functions were not bound"
    $$ (commentBlock . map ((text "  " <>) . text)) varargsFunctions
    $$ comment

type Deprecated = Bool
type Since = Maybe Version

doVersionIfDefs :: ([Doc] -> Doc) -> [(Doc, (Since, Deprecated))] -> Doc
doVersionIfDefs sep =
    sep
  . layoutChunks True empty
  . makeChunks [Nothing] False
  . map (\group@((_,(since, deprecated)):_) -> (map fst group, since, deprecated))
  . groupBy (equating snd)

  where makeChunks :: [Since] -> Deprecated -> [([Doc], Since, Deprecated)] -> [Chunk]
        makeChunks    sinceStack  True [] = EndChunk : makeChunks sinceStack False []
        makeChunks (_:[])         _    [] = []
        makeChunks (_:sinceStack) _    [] = EndChunk : makeChunks sinceStack False []
        makeChunks sinceStack@(sinceContext:_) prevDeprecated whole@((group, since, deprecated):rest)
          | deprecated < prevDeprecated = EndChunk              : makeChunks sinceStack deprecated whole
          | since < sinceContext        = EndChunk              : makeChunks (tail sinceStack)  prevDeprecated whole
          | deprecated > prevDeprecated = BeginDeprecatedChunk  : makeChunks sinceStack deprecated whole
          | since > sinceContext        = BeginSinceChunk since : makeChunks (since:sinceStack) prevDeprecated whole
          | otherwise                   = SimpleChunk group     : makeChunks sinceStack prevDeprecated rest
        
        layoutChunks :: Bool -> Doc -> [Chunk] -> [Doc]
        layoutChunks _     doc  []                             = doc : []
        layoutChunks _     doc (EndChunk              :chunks) =       layoutChunks False (doc $$ endif) chunks
        layoutChunks False doc (SimpleChunk group     :chunks) = doc : layoutChunks False (sep group) chunks
        layoutChunks True  doc (SimpleChunk group     :chunks) =       layoutChunks False (doc $$ sep group) chunks
        layoutChunks _     doc (BeginDeprecatedChunk  :chunks) = doc : layoutChunks True ifndefDeprecated chunks
        layoutChunks _     doc (BeginSinceChunk since :chunks) = doc : layoutChunks True (ifSinceVersion since) chunks
        
        ifSinceVersion (Just Version { versionBranch = [major,minor] }) =
          text "#if GTK_CHECK_VERSION(" <> int major <> comma <> int minor <> text ",0)"
        ifndefDeprecated = text "#ifndef DISABLE_DEPRECATED"
        endif = text "#endif"

data Chunk = SimpleChunk [Doc]
           | BeginDeprecatedChunk
           | BeginSinceChunk Since
           | EndChunk
