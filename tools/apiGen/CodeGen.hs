module CodeGen (
  genModuleBody,
  genExports,
  genTodoItems,
  makeKnownSymbolsMap,
  mungeMethodInfo
  ) where

import Api
import Docs
import FormatDocs
import Marshal
import StringUtils
import ModuleScan

import Prelude hiding (Enum, lines)
import List   (groupBy, sortBy, isPrefixOf, isSuffixOf)
import Maybe  (isNothing)
import Data.FiniteMap

import Debug.Trace (trace)

-------------------------------------------------------------------------------
-- Now lets actually generate some code fragments based on the api info
-------------------------------------------------------------------------------
genFunction :: KnownSymbols -> Method -> Maybe FuncDoc -> Maybe MethodInfo -> ShowS
genFunction knownSymbols method doc info =
  formattedDoc.
  (if method_deprecated method
     then ss "-- * Warning this function is deprecated\n--\n"
     else id).
  ss functionName. ss " :: ". functionType. nl.
  ss functionName. sc ' '. sepBy " " paramNames. ss " =".
  indent 1. body

  where functionName = cFuncNameToHsName (method_cname method)
	(classConstraints', paramTypes', paramMarshalers) =
	  unzip3 [ case genMarshalParameter knownSymbols
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
		genMarshalResult knownSymbols (method_return_type method)
        returnType = (returnType', lookup "Returns" paramDocMap)
	functionType = (case classConstraints of
	                  []  -> id
			  [c] -> ss c. ss " => "
			  cs  -> sc '('. sepBy ", " classConstraints. ss ") => ").
                       formatParamTypes (paramTypes ++ [returnType])
	body = foldl (\body marshaler -> marshaler body)
                     call (paramMarshalers++[returnMarshaler])
	call = ss (genCall (method_cname method) safety)
        safety = case info of
                  Nothing -> False
                  Just info -> methodinfo_unsafe info
        formattedDoc = case doc of
          Nothing  -> ss "-- | \n-- \n"
          Just doc -> ss "-- | ". haddocFormatParas knownSymbols (funcdoc_paragraphs doc). nl.
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
                  . wrapText 3 (80 - columnIndent - 8)
                  . map (mungeWord knownSymbols)
                  . words
                  . concatMap (haddocFormatSpan knownSymbols)
                columnIndent = maximum [ length parmType | (parmType, _) <- paramTypes ]

genModuleBody :: KnownSymbols -> Object -> ModuleDoc -> ModuleInfo -> ShowS
genModuleBody knownSymbols object apiDoc modInfo =
  doVersionIfDefs (sepBy' "\n\n") $
     genConstructors knownSymbols object (moduledoc_functions apiDoc)
  ++ genMethods knownSymbols object (moduledoc_functions apiDoc) (module_methods modInfo)
  ++ genProperties knownSymbols object (moduledoc_properties apiDoc)
  ++ genSignals knownSymbols object (moduledoc_signals apiDoc)

-- fixup the names of the C functions we got from scaning the original modules
-- we want the fully qualified "gtk_foo_bar" rather than "foo_bar" so that the
-- names match up consistently with the ones from the API xml file.
mungeMethodInfo :: Object -> ModuleInfo -> ModuleInfo
mungeMethodInfo object modInfo =
  modInfo {
    module_methods = map (\methodInfo -> 
                       if methodinfo_cname methodInfo `elem` shortMethodNames
                         then methodInfo {
                                methodinfo_cname = prefix ++ methodinfo_cname methodInfo
                              }
                         else methodInfo) (module_methods modInfo)
  }
  where shortMethodNames = map (stripPrefix . method_cname) (object_methods object)
        stripPrefix cname | prefix `isPrefixOf` cname = drop (length prefix) cname
                          | otherwise = cname
        prefix = module_context_prefix modInfo ++ "_"

genMethods :: KnownSymbols -> Object -> [FuncDoc] -> [MethodInfo] -> [(ShowS, (Since, Deprecated))]
genMethods knownSymbols object apiDoc methodInfo = 
  [ (genFunction knownSymbols method doc info, (maybe "" funcdoc_since doc, method_deprecated method))
  | (method, doc, info) <- methods object apiDoc methodInfo True]

methods :: Object -> [FuncDoc] -> [MethodInfo] -> Bool -> [(Method, Maybe FuncDoc, Maybe MethodInfo)]
methods object docs methodsInfo sortByExisting =
  map snd $
  sortBy (comparing fst)
  [ let (doc, docIndex) = case lookup (method_cname method) docmap of
                            Nothing -> (Nothing, endDocIndex)
                            Just (doc, index) -> (Just doc, index)
        (info,infoIndex)= case lookup (method_cname method) infomap of
                            Nothing -> (Nothing, endInfoIndex)
                            Just (info, index) -> (Just info, index)
        index | sortByExisting = (infoIndex, docIndex) --preserve order from existing module
              | otherwise      = (docIndex, infoIndex) --use gtk-doc order
     in (index,(mungeMethod object method, doc, info))
  | method <- object_methods object
  , null [ () | VarArgs <- method_parameters method]   --exclude VarArgs methods
--  , not ("_get_type" `isSuffixOf` method_cname method && method_shared method)
  , not (method_deprecated method && isNothing (lookup (method_cname method) infomap)) ]
  where docmap =  [ (funcdoc_name doc, (doc,index))
                  | (doc,index) <- zip docs [1..] ]
        infomap = [ (methodinfo_cname info, (info,index))
                  | (info,index) <- zip methodsInfo [1..] ]
        endDocIndex = length docs
        endInfoIndex = length methodsInfo

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

genConstructors :: KnownSymbols -> Object -> [FuncDoc] -> [(ShowS, (Since, Deprecated))]
genConstructors knownSymbols object apiDoc =
  [ (genFunction knownSymbols constructor doc Nothing, (maybe "" funcdoc_since doc, notDeprecated))
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
    method_parameters = constructor_parameters constructor,
    method_shared = False,
    method_deprecated = False
  }  

properties :: Object -> [PropDoc] -> [(Property, Maybe PropDoc)]
properties object docs =
  [ (property, property_cname property `lookup` docmap)
  | property <- object_properties object ]
  where docmap = [ (map dashToUnderscore (propdoc_name doc), doc)
                 | doc <- docs ]
        dashToUnderscore '-' = '_'
        dashToUnderscore  c  =  c

genProperties :: KnownSymbols -> Object -> [PropDoc] -> [(ShowS, (Since, Deprecated))]
genProperties knownSymbols object apiDoc = 
  [ (genProperty knownSymbols object property doc, (maybe "" propdoc_since doc, notDeprecated))
  | (property, doc) <- properties object apiDoc ]

genProperty :: KnownSymbols -> Object -> Property -> Maybe PropDoc -> ShowS
genProperty knownSymbols object property doc = 
  formattedDoc.
  ss propertyName. ss " :: Attr ". objectType. sc ' '. ss propertyType. nl.
  ss propertyName. ss " = Attr ". 
  indent 1. getter.
  indent 1. setter
  where objectType = ss (object_name object)
        propertyName = cFuncNameToHsName (property_cname property)
        getter = ss "(\\obj -> do ". ss gvalueConstructor. ss " result <- objectGetProperty \"". ss (property_cname property). ss "\"".
                 indent 7. ss "return result)"
        setter = ss "(\\obj val -> objectSetProperty obj \"". ss (property_cname property). ss "\" (". ss gvalueConstructor. ss " val))"
        formattedDoc = case doc of
          Nothing  -> ss "-- | \n-- \n"
          Just doc -> ss "-- | ". haddocFormatParas knownSymbols (propdoc_paragraphs doc). nl.
                      comment. nl
        (propertyType, gvalueConstructor) = genMarshalProperty knownSymbols (property_type property)

signals :: Object -> [SignalDoc] -> [(Signal, Maybe SignalDoc)]
signals object docs =
  [ (signal, map dashToUnderscore (signal_cname signal) `lookup` docmap)
  | signal <- object_signals object ]
  where docmap = [ (map dashToUnderscore (signaldoc_name doc), doc)
                 | doc <- docs ]
        dashToUnderscore '-' = '_'
        dashToUnderscore  c  =  c

genSignals :: KnownSymbols -> Object -> [SignalDoc] -> [(ShowS, (Since, Deprecated))]
genSignals knownSymbols object apiDoc = 
  [ (genSignal knownSymbols object signal doc, (maybe "" signaldoc_since doc, notDeprecated))
  | (signal, doc) <- signals object apiDoc ] 

genSignal :: KnownSymbols -> Object -> Signal -> Maybe SignalDoc -> ShowS
genSignal knownSymbols object property doc = 
  formattedDoc.
  ss "on". signalName. ss ", after". signalName. ss " :: ". nl.
  ss "on".    signalName. ss " = connect_{-type-}". connectType. sc ' '. signalCName. ss " False". nl.
  ss "after". signalName. ss " = connect_{-type-}". connectType. sc ' '. signalCName. ss " True". nl

  where connectType = id
        signalName = ss (upperCaseFirstChar (cFuncNameToHsName (signal_cname property)))
        signalCName = sc '"'. ss (signal_cname property). sc '"'
        formattedDoc = case doc of
          Nothing  -> ss "-- | \n-- \n"
          Just doc -> ss "-- | ". haddocFormatParas knownSymbols (signaldoc_paragraphs doc). nl.
                      comment. nl

makeKnownSymbolsMap :: API -> KnownSymbols
makeKnownSymbolsMap api =
   (listToFM
  . reverse
  . concat)
  [ [ (enum_cname enum
      ,case enum_variety enum of
        EnumVariety -> SymEnumType EnumKind
        FlagsVariety -> SymEnumType FlagsKind)
    | enum <- namespace_enums namespace ]
 ++ [ (object_cname object, objectKind object)
    | object <- namespace_objects namespace ]
 ++ [ ("GObject", SymObjectType GObjectKind) ]
 ++ [ (member_cname member, SymEnumValue)
    | enum <- namespace_enums namespace
    , member <- enum_members enum ]
  | namespace <- api ]

        -- find if an object inherits via GtkObject or directly from GObject
  where objectKind :: Object -> CSymbol
        objectKind object = lookup (objectParents object)
          where lookup [] = trace ( "Warning: " ++ object_name object
                                 ++ " does not inherit from GObject! "
                                 ++ show (objectParents object)) SymStructType
                lookup ("GObject":os) = SymObjectType GObjectKind
                lookup ("GtkObject":os) = SymObjectType GtkObjectKind
                lookup (_:os) = lookup os
        objectParents :: Object -> [String]
        objectParents object = object_cname object :
          case object_parent object `lookup` objectMap of
            Nothing -> [object_parent object]
            Just parent -> objectParents parent
        objectMap :: [(String, Object)]
        objectMap = [ (object_cname object, object)
                    | namespace <- api
                    , object <- namespace_objects namespace ]

genExports :: Object -> ModuleDoc -> ModuleInfo -> ShowS
genExports object docs modInfo =
  comment.ss "* Types".
  indent 1.ss (object_name object).sc ','.
  indent 1.ss (object_name object).ss "Class,".
  indent 1.ss "castTo".ss (object_name object).sc ','.
  (case [ (ss "  ". ss (cFuncNameToHsName (method_cname constructor)). sc ','
          ,(maybe "" funcdoc_since doc, notDeprecated))
        | (constructor, doc) <- constructors object (moduledoc_functions docs)] of
     [] -> id
     cs -> nl.nl.comment.ss "* Constructors".nl.
           doVersionIfDefs lines cs).
  (case [ (ss "  ". ss (cFuncNameToHsName (method_cname method)). sc ','
          ,(maybe "" funcdoc_since doc, method_deprecated method))
        | (method, doc, _) <- methods object (moduledoc_functions docs)
                                (module_methods modInfo) False] of
     [] -> id
     cs -> nl.nl.comment.ss "* Methods".nl.
           doVersionIfDefs lines cs).
  (case [ (ss "  ". ss (cFuncNameToHsName (property_cname property)). sc ','
          ,(maybe "" propdoc_since doc, notDeprecated))
        | (property, doc) <- properties object (moduledoc_properties docs)] of
     [] -> id
     cs -> nl.nl.comment.ss "* Properties".nl.
           doVersionIfDefs lines cs).
  (case [ let signalName = (upperCaseFirstChar . cFuncNameToHsName . signal_cname) signal in 
          (ss "  on".    ss signalName. sc ','.nl.
           ss "  after". ss signalName. sc ','
          ,(maybe "" signaldoc_since doc, notDeprecated))
        | (signal, doc) <- signals object (moduledoc_signals docs)] of
     [] -> id
     cs -> nl.nl.comment.ss "* Signals".nl.
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

type Deprecated = Bool
notDeprecated = False

doVersionIfDefs :: ([ShowS] -> ShowS) -> [(ShowS, (Since, Deprecated))] -> ShowS
doVersionIfDefs lines =
    lines
  . map (\group@((_,(since, deprecated)):_) ->
             sinceVersion since
           . ifdefDeprecated deprecated
           $ (lines (map fst group)))
  . groupBy (\(_,a) (_,b) -> a == b)
 
sinceVersion :: Since -> ShowS -> ShowS
sinceVersion [major,'.',minor] body =
  ss "#if GTK_CHECK_VERSION(". sc major. ss ",". sc minor. ss ",0)\n".
  body.
  ss "\n#endif"
sinceVersion _ body = body

ifdefDeprecated :: Deprecated -> ShowS -> ShowS
ifdefDeprecated True body =
  ss "#ifndef DISABLE_DEPRECATED\n".
  body.
  ss "\n#endif"
ifdefDeprecated _ body = body

comparing :: (Ord a) => (b -> a) -> b -> b -> Ordering
comparing p x y = compare (p x) (p y)
