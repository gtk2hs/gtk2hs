{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module Module where

import Data.Version (Version)
import qualified Api
import qualified Docs
import qualified ModuleScan
import qualified Names (cFuncNameToHsPropName, cFuncNameToHsName,
                             cAttrNametoHsName)
import HaddockDocs (Section(..), Para(..), Span(..))
import qualified MarshalFixup (cTypeNameToHSType, actionSignalWanted,
                               fixModuleAvailableSince, fixModuleDocMapping)
import qualified ExcludeApi
import qualified Names (cFuncNameToHsPropName, cFuncNameToHsName,
                        cAttrNametoHsName)
import Utils

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.List as List
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Tree (Forest, Tree(Node))
import Data.Version
import Control.Monad (mplus)

data Module = Module {
  module_name         :: String,  -- Hs module and type name, eg "Container"
  module_cname        :: String,  -- C object name, eg "GtkContainer"

  module_prefix       :: String,  -- Hs module namespace prefix, eg "Graphics.Gtk"
  module_namespace    :: String,  -- C object namespace, eg "Gtk"

  module_filename          :: String,  -- name part of the file including ext
  module_needs_c2hs        :: Bool,
  
  module_authors           :: [String],
  module_created           :: String,
  module_copyright_dates   :: Either String (String, String),
                              -- eg "2004" or "2004-2005"
  module_copyright_holders :: [String],

  module_context_lib       :: String,
  module_context_prefix    :: String,
  
  module_kind         :: ModuleKind,
  
  module_imports      :: [(String, String)],
  module_exports      :: [String],
  module_todos        :: [String],
  
  module_deprecated   :: Bool,
  module_since        :: Maybe Version,

  module_summary     :: [Para],        -- usually a one line summary
  module_description :: [Section],     -- the main description (including subsections)
  module_hierarchy   :: Forest [Span], -- a tree of parent objects (as text)

  module_decls :: [Decl]
}

data ModuleKind = Interface | Widget | Object
  deriving Show

data Decl = Decl {
  decl_name               :: String,
  decl_doc                :: Maybe [Para],
  decl_since              :: Maybe Version,
  decl_deprecated         :: Bool,
  decl_deprecated_comment :: String,
  decl_index_api          :: Int,
  decl_index_doc          :: Int,
  decl_index_code         :: Int,
  decl_index_export       :: Int,
  decl_bound              :: Bool,
  decl_user_code          :: [String],
  decl_user_docs          :: [String],
  decl_user_code_hash     :: String,
  decl_user_docs_hash     :: String,
  decl_body               :: DeclBody,
  decl_module             :: Module
}

declDefaults :: Decl
declDefaults = Decl {
    decl_doc = Just [],
    decl_since = Nothing,
    decl_deprecated = False,
    decl_deprecated_comment = "this function should not be used in newly-written code",
    decl_index_doc = 0,
    decl_index_code = 0,
    decl_index_export = 0,
    decl_bound = False,
    decl_user_code = [],
    decl_user_docs = [],
    decl_user_code_hash = [],
    decl_user_docs_hash = []
  }

data DeclBody
  = Method {
      method_name           :: String,
      method_cname          :: String,
      method_shortcname     :: Maybe String,
      method_return_type    :: String,
      method_parameters     :: [Api.Parameter],
      method_param_docs     :: [(String, [Span])],
      method_is_unsafe_ffi  :: Bool, -- {#call unsafe foo#} rather than {#call foo#}
      method_is_shared      :: Bool,
      method_is_constructor :: Bool
} | AttributeProp {
      attribute_name  :: String,
      attribute_cname :: String,
      attribute_type :: String,
      attribute_readable :: Bool,
      attribute_writeable :: Bool,
      attribute_construct :: Bool,
      attribute_constructonly :: Bool,
      attribute_is_child :: Bool
} | AttributeGetSet {
      attribute_type :: String,
      attribute_readable :: Bool,
      attribute_writeable :: Bool,
      attribute_getter :: Decl,
      attribute_setter :: Decl
} | Signal {
      signal_name   :: String,
      signal_cname  :: String,
      signal_return_type  :: String,
      signal_parameters   :: [Api.Parameter],
      signal_is_action    :: Bool,
      signal_is_old_style :: Bool,
      signal_is_after     :: Bool
} | Instance {
      instance_class_name :: String,
      instance_type_name  :: String
}


convertAPI :: Api.API -> [Module]
convertAPI = concatMap convertNameSpace

convertNameSpace :: Api.NameSpace -> [Module]
convertNameSpace namespace =
  let objects = Api.namespace_objects namespace
             ++ map mungeClassToObject (Api.namespace_classes namespace)
             ++ map mungeBoxedToObject (Api.namespace_boxed namespace)
   in  map (convertObject namespace) objects

mungeClassToObject :: Api.Class -> Api.Object
mungeClassToObject cl =
  Api.Object {
    Api.object_name         = Api.class_name cl,
    Api.object_cname        = Api.class_cname cl,
    Api.object_parent       = "",
    Api.object_constructors = [],
    Api.object_methods      = Api.class_methods cl,
    Api.object_properties   = [],
    Api.object_childprops   = [],
    Api.object_signals      = [],
    Api.object_implements   = [],
    Api.object_deprecated   = False,
    Api.object_isinterface  = False
  }

mungeBoxedToObject :: Api.Boxed -> Api.Object
mungeBoxedToObject boxed =
  Api.Object {
    Api.object_name         = Api.boxed_name boxed,
    Api.object_cname        = Api.boxed_cname boxed,
    Api.object_parent       = "",
    Api.object_constructors = Api.boxed_constructors boxed,
    Api.object_methods      = Api.boxed_methods boxed,
    Api.object_properties   = [],
    Api.object_childprops   = [],
    Api.object_signals      = [],
    Api.object_implements   = [],
    Api.object_deprecated   = False,
    Api.object_isinterface  = False
  }


convertObject :: Api.NameSpace -> Api.Object -> Module
convertObject namespace object =
  let module_ =
        Module {
          module_name  = Api.object_name object,
          module_cname = Api.object_cname object,
          
          module_namespace = Api.namespace_name namespace,

          module_needs_c2hs = True,

          module_authors = ["[Insert your full name here]"],
          module_copyright_holders = ["[Insert your full name here]"],

          module_context_lib = Api.namespace_library namespace,
          module_context_prefix = Api.namespace_library namespace,

          module_kind = if Api.object_isinterface object then Interface else Widget,

          module_deprecated = Api.object_deprecated object,
          module_since = Nothing,

          module_imports = [],
          
          module_decls =
            let addIndexAndModule n decl = decl {
                   decl_index_api = n,
                   decl_module = module_
                 }
            in [ addIndexAndModule n (convertConstructor object constructor)
               | (n, constructor) <- zip [1..] (Api.object_constructors object) ]

            ++ [ addIndexAndModule n (convertMethod module_ object method)
               | (n, method) <- zip [1..] (Api.object_methods object) ]

            ++ [ addIndexAndModule n (convertProperty False object prop)
               | (n, prop) <- zip [1..] (Api.object_properties object) ]

            ++ [ addIndexAndModule n (convertProperty True object prop)
               | (n, prop) <- zip [1..] (Api.object_childprops object) ]

            ++ [ addIndexAndModule n (convertSignals object signal)
               | (n, signal) <- zip [1..] (Api.object_signals object) ]

            ++ [ addIndexAndModule n (convertInterfaces object class_)
               | (n, class_) <- zip [1..] (Api.object_implements object) ]
        }
   in module_

convertConstructor :: Api.Object -> Api.Constructor -> Decl
convertConstructor object constructor = declDefaults {
    decl_name = Names.cFuncNameToHsName (Api.constructor_cname constructor),
    decl_body = Method {
      method_name  = error "constructor method_name", --Names.cFuncNameToHsName (constructor_cname constructor),
      method_cname = Api.constructor_cname constructor,
      method_shortcname  = Nothing,
      method_return_type = Api.object_cname object ++ "*",
      method_parameters  = Api.constructor_parameters constructor,
      method_param_docs  = [],
      method_is_unsafe_ffi  = False,
      method_is_shared      = True,
      method_is_constructor = True
    }
  }

convertMethod :: Module -> Api.Object -> Api.Method -> Decl
convertMethod module_ object method =
  let funcName = Names.cFuncNameToHsName (Api.method_cname method)
      methodName = drop (length (module_name module_)) funcName
   in declDefaults {
        decl_name = funcName,
        decl_deprecated = Api.method_deprecated method,
        decl_body = Method {
          method_name  = methodName,
          method_cname = Api.method_cname method,
          method_shortcname  = Nothing,
          method_return_type = Api.method_return_type method,
          method_parameters  = if Api.method_shared method
                                 then Api.method_parameters method
                                 else self : Api.method_parameters method,
          method_param_docs  = [],
          method_is_unsafe_ffi  = False,
          method_is_shared      = Api.method_shared method,
          method_is_constructor = False
        }
      }
  where self = Api.Parameter {
                 Api.parameter_type = Api.object_cname object ++ "*",
                 Api.parameter_name = "self",
                 Api.parameter_out  = False,
                 Api.parameter_isArray = False
               }

convertProperty :: Bool -> Api.Object -> Api.Property -> Decl
convertProperty isChild object property =
  declDefaults {
    decl_name =
      let objName  = lowerCaseFirstWord (Api.object_name object)
          propName = Names.cAttrNametoHsName (Api.property_cname property)
       in if isChild
            then objName ++ "Child" ++ propName
            else objName ++ propName,
    decl_body = AttributeProp {
      attribute_name = Names.cAttrNametoHsName (Api.property_cname property),
      attribute_cname = Api.property_cname property,
      attribute_type = Api.property_type property,
      attribute_readable = Api.property_readable property,
      attribute_writeable = Api.property_writeable property,
      attribute_construct = Api.property_construct property,
      attribute_constructonly = Api.property_constructonly property,
      attribute_is_child = isChild
    }
  }


convertSignals :: Api.Object -> Api.Signal -> Decl
convertSignals object signal =
  declDefaults {
    decl_name =
      let objName = lowerCaseFirstWord (Api.object_name object)
       in objName ++ Api.signal_name signal,
    decl_body = Signal {
      signal_name   = Api.signal_name signal,
      signal_cname  = Api.signal_cname signal,
      signal_is_action   = Api.signal_action signal,
      signal_return_type = Api.signal_return_type signal,
      signal_parameters  = Api.signal_parameters  signal,
      signal_is_old_style = False
    }
  }

convertInterfaces :: Api.Object -> String -> Decl
convertInterfaces object interfaceName =
  declDefaults {
    decl_name = "",
    decl_doc  = Nothing,
    decl_body =
      Instance {
        instance_class_name = MarshalFixup.cTypeNameToHSType interfaceName ++ "Class",
        instance_type_name  = Api.object_name object       
      }
  }

deleteUnnecessaryDocs :: Module -> Module
deleteUnnecessaryDocs module_ =
  module_ {
    module_decls = map delDocs (module_decls module_)
  }
  where delDocs decl@Decl {
          decl_body = method@Method { method_param_docs = param_docs,
                                      method_is_constructor = True }
        } = decl {
          decl_body = method {
            method_param_docs = filter ((/="Returns") . fst) param_docs
          }
        }
        delDocs decl = decl

applyModuleScanInfo :: String -> String -> String
                    -> Map String ModuleScan.ModuleInfo -> Module -> Module
applyModuleScanInfo modPrefix date year modInfoMap module_ =
  case Map.lookup (module_name module_) modInfoMap of
    Nothing ->
      module_ {
        module_prefix          = modPrefix,
        module_filename        = module_name module_ ++ ".chs",
        module_copyright_dates = Left year,
        module_created         = date
      }
    Just info ->
      module_ {
        module_prefix            = ModuleScan.module_prefix info,
        module_filename          = ModuleScan.module_filename info,
        module_needs_c2hs        = ModuleScan.module_needsc2hs info,

        module_authors           = ModuleScan.module_authors info,
        module_copyright_dates   = ModuleScan.module_copyright_dates info,
        module_copyright_holders = ModuleScan.module_copyright_holders info,
        module_created           = ModuleScan.module_created info,
        
        module_imports           = ModuleScan.module_imports info,
        
        module_decls = map (addExportIndex . addDeclScanInfo)
                           (module_decls module_)
      }
      where declInfoMap = Map.fromListWith (\a b -> b)
              [ (ModuleScan.func_name funcinfo, (n, funcinfo))
              | (n, funcinfo) <- zip [1..] (ModuleScan.module_functions info) ]

            addDeclScanInfo decl@Decl { decl_body = body } =
              case Map.lookup (decl_name decl) declInfoMap of
                Nothing -> decl { decl_index_code = maxBound }
                Just (n, funcinfo) ->
                  decl {
                    decl_index_code = n,
                    decl_bound = True,
                    decl_user_code = ModuleScan.func_body funcinfo, 
                    decl_user_docs = ModuleScan.func_docs funcinfo,
                    decl_user_code_hash = ModuleScan.func_body_hash funcinfo,
                    decl_user_docs_hash = ModuleScan.func_docs_hash funcinfo,
                    decl_body = addDeclBodyScanInfo funcinfo body
                  }
            
            addDeclBodyScanInfo funcinfo method@Method{} =
              method {
                method_shortcname = shortcname,
                method_is_unsafe_ffi = unsafe
              }
              where fullCName cname | prefix `List.isPrefixOf` cname = cname
                                    | otherwise = prefix ++ cname
                    prefix = module_context_prefix module_ ++ "_"
                    isshortcname ccall =
                         fullCName (ModuleScan.ccall_name ccall)
                      == method_cname method
                    ccalls = ModuleScan.func_ccalls funcinfo
                    ccall  = listToMaybe (filter isshortcname ccalls)
                    shortcname = fmap ModuleScan.ccall_name ccall
                    unsafe = maybe False ModuleScan.ccall_unsafe ccall

            addDeclBodyScanInfo funcinfo body = body

            exportIndexMap = Map.fromList (zip (ModuleScan.module_exports info) [1..])

            --TODO: remove the special case for signals and on.
            addExportIndex decl@Decl { decl_body = Signal { signal_name = name } } =
              decl {
                decl_index_export = fromMaybe maxBound
                                      (Map.lookup ("on"++name) exportIndexMap)
              }
            addExportIndex decl@Decl { decl_name = name } =
              decl {
                decl_index_export = fromMaybe maxBound
                                      (Map.lookup name exportIndexMap)
              }

-- only use this until we have scan info, because we don't remove ones
-- that are currently bound
filterDeprecated :: Module -> Module
filterDeprecated module_ =
  module_ {
    module_decls =
      [ decl
      | decl <- module_decls module_
      , decl_bound decl || not (decl_deprecated decl) ]
  }

filterVarArgs :: Module -> Module
filterVarArgs module_ =
  module_ {
    module_decls = normal,
    module_todos = map (method_cname . decl_body) varargs
  }
  where (varargs, normal) = List.partition hasVarArgs (module_decls module_)
        hasVarArgs :: Decl -> Bool
        hasVarArgs Decl{ decl_body = Method{ method_parameters = params }}
          = not $ null [ () | Api.VarArgs <- params ]
        hasVarArgs _ = False

makeOldSignals :: Module -> Module
makeOldSignals module_ =
  module_ {
    module_decls = makeOldSignal (module_decls module_)
  }
  where makeOldSignal (decl@Decl { decl_name = newName,
                                   decl_body = signal@Signal {} } : decls) =
          decl :
          let oldName = "on" ++ signal_name signal in
          decl {
            decl_name = oldName,
            decl_deprecated = True,
            decl_deprecated_comment = "instead of '" ++ oldName ++ " obj' "
                                   ++ "use 'on obj " ++ newName ++ "'",
            decl_doc = Nothing,
            decl_body = signal {
              signal_is_old_style = True,
              signal_is_after     = False
            }
          } :
          let oldName = "after" ++ signal_name signal in
          decl {
            decl_name = oldName,
            decl_deprecated = True,
            decl_deprecated_comment = "instead of '" ++ oldName ++ " obj' "
                                   ++ "use 'after obj " ++ newName ++ "'",
            decl_doc = Nothing,
            decl_body = signal {
              signal_is_old_style = True,
              signal_is_after     = True
            }
          } :
          makeOldSignal decls

        makeOldSignal (decl:decls) = decl : makeOldSignal decls
        makeOldSignal [] = []

filterNewActionSignals :: Module -> Module
filterNewActionSignals module_ =
  module_ {
    module_decls = filterNewActionSignals' (module_decls module_)
  }
  where
    filterNewActionSignals'
      (Decl { decl_module = Module { module_cname = objCName },
              decl_body = Signal { signal_is_action = True,
                                   signal_is_old_style = False,
                                   signal_cname = sigCName } } : decls)
      | not (MarshalFixup.actionSignalWanted objCName sigCName)
      = filterNewActionSignals' decls

    filterNewActionSignals'
      (decl@Decl { decl_module = Module { module_cname = objCName },
                   decl_body = Signal { signal_is_action = True,
                                        signal_is_old_style = True,
                                        signal_cname = sigCName } } : decls)
      | MarshalFixup.actionSignalWanted objCName sigCName
      = decl { decl_doc = Nothing } : filterNewActionSignals' decls

    filterNewActionSignals'
      (decl@Decl { decl_body = Signal { signal_is_action = True,
                                        signal_is_old_style = True } } : decls)
      = decl {- decl_doc = Just [] -} : filterNewActionSignals' decls

    filterNewActionSignals'
      (decl@Decl { decl_body = Signal { signal_is_action = False,
                                        signal_is_old_style = True } } : decls)
      = decl { decl_doc = Nothing } : filterNewActionSignals' decls

    filterNewActionSignals' (decl:decls) = decl : filterNewActionSignals' decls
    filterNewActionSignals' [] = []
  


makeGetSetProps :: Module -> Module
makeGetSetProps module_ =
  module_ {
    module_decls =
      concat
      [ let attrName = Names.cFuncNameToHsPropName (method_cname getter_body)
            declName = lowerCaseFirstWord (module_name module_) ++ attrName
         in declDefaults {
              decl_name = declName,
              decl_body = AttributeGetSet {
                attribute_type = method_return_type getter_body,
                attribute_readable  = True,
                attribute_writeable = True,
                attribute_getter = getter,
                attribute_setter = setter
              },
              decl_doc = Just $
                   fromMaybe [] (decl_doc setter)
                ++ fromMaybe [] (decl_doc getter)
                ++ [ParaText [SpanText $ "TODO: merge attr docs from those of"
                                      ++ " the setter and getter methods"]],
              decl_module = module_,
              decl_index_api = maxBound :: Int
            }
            : deprecateGetterMethod getter declName
            : deprecateSetterMethod setter declName
            : []
      | (getter@Decl { decl_body = getter_body }
        ,setter@Decl { decl_body = setter_body }) <- extraProps ]

   ++ map fst directProps
   ++ genericProps
   ++ filter (not . isGetSetMethod) nonPropsDecls
   ++ concat
      [ deprecateGetterMethod getter (decl_name prop)
      : deprecateSetterMethod setter (decl_name prop) : []
      | (prop,(getter, setter)) <- directProps ],
   module_todos = module_todos module_
  }

  where
    deprecateGetterMethod decl attrName = decl {
        decl_deprecated = True,
        decl_doc        = Nothing,
        decl_deprecated_comment =
             "instead of '" ++ decl_name decl ++ " obj'"
          ++ " use 'get obj " ++ attrName ++ "'"
      }
    deprecateSetterMethod decl attrName = decl {
        decl_deprecated = True,
        decl_doc        = Nothing,
        decl_deprecated_comment =
             "instead of '" ++ decl_name decl ++ " obj value'"
          ++ " use 'set obj [ " ++ attrName ++ " := value ]'"
      }

    -- methods that are getters or setters for properties. We want to deprecate
    -- these methods and eventually not export the ones that are not needed to
    -- implement the properties. That is, we'd remove the directProps ones completely
    -- and keep but not export the extraProps ones. We only want to continue to
    -- support the attributes as the public API, we don't want duplication in the API
    -- between attributes and getter/setter methods.
    getterSetterMethodNames =
         [ decl_name decl | (_,(get, set)) <- directProps, decl <- [get,set] ]
      ++ [ decl_name decl |    (get, set)  <- extraProps,  decl <- [get,set] ]

    isGetSetMethod Decl { decl_body = Method {}, decl_name = name }
                     = name `elem` getterSetterMethodNames
    isGetSetMethod _ = False

    (genericProps, -- existing GObject properties with generic implementation
     directProps,  -- existing GObject properties but with direct implementation
     extraProps)   -- extra properties with direct implementation
      = mergeBy (\(Decl { decl_body = prop }) (Decl { decl_body = func }, _) ->
                  attribute_name prop `compare` drop 3 (method_name func))
                (List.sortBy (comparing (attribute_name.decl_body)) props)
                (List.sortBy (comparing (method_name.decl_body.fst)) methodsThatLookLikeProperties)

    (props, nonPropsDecls) = List.partition isProp (module_decls module_)
    
    isProp (Decl { decl_body = AttributeProp {} }) = True
    isProp _                                       = False

    methodsThatLookLikeProperties :: [(Decl, Decl)]
    methodsThatLookLikeProperties =
      intersectBy (equating (drop 3 . method_name . decl_body)) getters setters
      where getters = [ decl
                      | decl@Decl{decl_body = method@Method{}} <- module_decls module_
                      , not (method_is_shared method)
                      , not (decl_deprecated decl)
                      , "Get" `List.isPrefixOf` method_name method
                      , length (method_parameters method) == 1 ]
            setters = [ decl
                      | decl@Decl{decl_body = method@Method{}} <- module_decls module_
                      , not (method_is_shared method)
                      , not (decl_deprecated decl)
                      , "Set" `List.isPrefixOf` method_name method
                      , length (method_parameters method) == 2
                      , method_return_type method == "void" ]

            intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [(a,a)]
            intersectBy eq xs ys = [ (x,y) | x <- xs, Just y <- [List.find (eq x) ys] ]

isAttr, isMethod :: Decl -> Bool

isAttr (Decl { decl_body = AttributeProp { attribute_is_child = False } }) = True
isAttr (Decl { decl_body = AttributeGetSet {} }) = True
isAttr _                                         = False

isMethod Decl { decl_body = Method {} } = True
isMethod _                              = False

reorderDecls :: Module -> Module
reorderDecls module_ =
  module_ {
    --FIXME: this wirdness about treating methods differently 
    --       is only for compatability, remove it.
    module_decls = List.sortBy (comparing lexicographicCurDocApi) methods
                ++ List.sortBy (comparing decl_index_api) others
  }
  where (methods, others) = List.partition isMethod (module_decls module_)
        lexicographicCurDocApi decl =
          (decl_index_code decl
          ,decl_index_doc decl
          ,decl_index_api decl)


fixModuleAvailableSince :: Module -> Module
fixModuleAvailableSince module_ =
  module_ {
    module_since = MarshalFixup.fixModuleAvailableSince (module_cname module_)
           `mplus` module_since module_
  }

addDeclAvailableSincePara :: Module -> Module
addDeclAvailableSincePara module_@Module { module_since = baseVersion } =
  module_ {
    module_decls = map (addDeprecatedPara . addAvailablePara)
                       (module_decls module_),
    module_summary = module_summary module_
                  ++ moduleVersionParagraph
                  ++ moduleDeprecatedParagraph
  }
  where moduleVersionParagraph =
          case module_since module_ of
            Nothing -> []
            Just since ->
              let line = "Module available since "
                      ++ fixNamespace (module_namespace module_)
                      ++ " version " ++ showVersion since
               in [ParaListItem [SpanText line]]

        moduleDeprecatedParagraph =
          if module_deprecated module_
            then let line = "Warning: this module is deprecated "
                         ++ "and should not be used in newly-written code."
            
                  in [ParaListItem [SpanText line]]
            else []

        addAvailablePara decl@Decl { decl_since = Just since }
          | Just since > baseVersion =
          decl {
            decl_doc =
              let line = "Available since "
                      ++ fixNamespace (module_namespace module_)
                      ++ " version " ++ showVersion since
               in fmap (++[ParaListItem [SpanText line]]) (decl_doc decl)
          }
        addAvailablePara decl = decl

        addDeprecatedPara decl@Decl { decl_deprecated = True }
          | not (module_deprecated module_) =
          decl {
            decl_doc =
              let line = "Warning: " ++ decl_deprecated_comment decl
               in fmap (++[ParaListItem [SpanText line]]) (decl_doc decl)
          }
        addDeprecatedPara decl = decl
        
        fixNamespace "Gtk" = "Gtk+"
        fixNamespace other = other

canonicalSignalName :: String -> String
canonicalSignalName = map dashToUnderscore
  where dashToUnderscore '-' = '_'
        dashToUnderscore  c  =  c


excludeApi :: [String] -> Module -> Module
excludeApi [] module_ = module_
excludeApi excludeApiFilesContents module_ =
  module_ {
    module_decls = filter okAPI (module_decls module_)
  }
  where match = ExcludeApi.matcher
                (concatMap ExcludeApi.parseFilterFile excludeApiFilesContents)
      
        okAPI :: Decl -> Bool   --returns False to exclude the C function name
        okAPI Decl { decl_body = Method { method_cname = cname } }
          = match cname
        okAPI _ = True


excludeConstructOnlyAttrs :: Module -> Module
excludeConstructOnlyAttrs module_ =
  module_ {
    module_decls = filter (not . isConstructOnly) (module_decls module_)
  }
  where isConstructOnly Decl {
                          decl_body = AttributeProp {
                            attribute_constructonly = True,
                            attribute_readable = False
                          }
                        } =  True
        -- this is a bad hack to prevent a wonky attribute from spoiling the translation
        isConstructOnly Decl {
                          decl_body = AttributeProp {
                            attribute_constructonly = False,
                            attribute_readable = False,
                            attribute_writeable = False
                          }
                        } =  True
        isConstructOnly _ = False


{-
content <- readFile "gtk-api.xml"
let api = Api.extractAPI (Text.XML.HaXml.Parse.xmlParse "gtk-api.xml" content)
let module_ = head [ module_ | module_@Module { module_name = "Viewport" } <- convertAPI api ]
let (props, decls) = List.partition isProp (module_decls module_)
let props' = List.sortBy (comparing (attribute_name.decl_body.fst)) (zip props [1..])
let gsetters = List.sortBy (comparing (method_name.decl_body.fst)) (methodsThatLookLikeProperties module_)
let (genericProps, directProps, extraProps) = StringUtils.mergeBy (\(Decl { decl_body = prop }, _) (Decl { decl_body = func }, _) -> attribute_name prop `compare` drop 3 (method_name func)) props' gsetters
-}
