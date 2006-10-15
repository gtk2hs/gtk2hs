module Api (
  API,
  NameSpace(..),
  Enum(..),
  EnumVariety(..),
  Member(..),
  Object(..),
  Class(..),
  Boxed(..),
  Field(..),
  Constructor(..),
  Parameter(..),
  Method(..),
  Property(..),
  Signal(..),
  Misc(..),
  extractAPI
  ) where

import Prelude hiding (Enum)
import Maybe  (catMaybes, isJust)
import Char (isSpace)
import List (find)

import qualified Text.XML.HaXml as Xml

-------------------------------------------------------------------------------
-- Types representing the content of the API XML file
-------------------------------------------------------------------------------
type API = [NameSpace]

data NameSpace = NameSpace {
    namespace_name :: String,
    namespace_library :: String,
    namespace_objects :: [Object],
    namespace_classes :: [Class],
    namespace_boxed :: [Boxed],
    namespace_enums :: [Enum],
    namespace_misc :: [Misc]
  } deriving Show

data Enum = Enum {
    enum_name :: String,
    enum_cname :: String,
    enum_variety :: EnumVariety,
    enum_members :: [Member]
 } deriving Show

data EnumVariety = EnumVariety | FlagsVariety
   deriving Show

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
    object_methods :: [Method],
    object_properties :: [Property],
    object_childprops :: [Property],
    object_signals :: [Signal],
    object_implements :: [String],
    object_deprecated :: Bool,
    object_isinterface ::Bool
  } deriving Show

data Class = Class {
    class_name :: String,
    class_cname :: String,
    class_methods :: [Method]
  } deriving Show

data Boxed = Boxed {
    boxed_name :: String,
    boxed_cname :: String,
    boxed_constructors :: [Constructor],
    boxed_methods :: [Method],
    boxed_fields :: [Field],
    boxed_opaque :: Bool
  } deriving Show

data Field = Field {
    field_name :: String,
    field_cname :: String,
    field_type :: String,
    field_public :: Bool, --public or private
    field_bits :: Int
  } deriving Show

data Constructor = Constructor {
    constructor_cname :: String,
    constructor_parameters :: [Parameter]
  } deriving Show

data Parameter = Parameter {
    parameter_type :: String,
    parameter_name :: String,
    parameter_out  :: Bool,
    parameter_isArray :: Bool
  }
               | VarArgs
  deriving Show

data Method = Method {
    method_name :: String,
    method_cname :: String,
    method_return_type :: String,
    method_return_owned :: Bool,
    method_parameters :: [Parameter],
    method_shared :: Bool,
    method_deprecated :: Bool
  } deriving Show

data Property = Property {
    property_name :: String,
    property_cname :: String,
    property_type :: String,
    property_readable :: Bool,
    property_writeable :: Bool,
    property_construct :: Bool,
    property_constructonly :: Bool
  } deriving Show

data Signal = Signal {
    signal_name :: String,
    signal_cname :: String,
    signal_when :: String,
    signal_action :: Bool,
    signal_return_type :: String,
    signal_parameters :: [Parameter]    
  } deriving Show

data Misc =
    Struct {
      misc_name :: String,
      misc_cname :: String
    }
  | Alias {
      misc_name :: String,
      misc_cname :: String
    }
  | Callback {
      misc_name :: String,
      misc_cname :: String
    }
  deriving Show

-------------------------------------------------------------------------------
-- extract functions to convert the api xml file to the internal representation
-------------------------------------------------------------------------------
extractAPI :: Xml.Document -> API
extractAPI (Xml.Document _ _ (Xml.Elem "api" [] namespaces) _) =
  catMaybes (map extractNameSpace (concatMap (Xml.foldXml white) namespaces))
  where
  -- remove empty CString constructors from the whole document
  white :: Xml.CFilter
  white (Xml.CString False str) | all isSpace str = []
  white elem = [elem]

extractNameSpace :: Xml.Content -> Maybe NameSpace
extractNameSpace (Xml.CElem (Xml.Elem "namespace"
                        [("name", Xml.AttValue name),
                         ("library", Xml.AttValue lib)] content)) =
  Just $ NameSpace {
    namespace_name = Xml.verbatim name,
    namespace_library = Xml.verbatim lib,
    namespace_objects = catMaybes (map extractObject content),
    namespace_classes = catMaybes (map extractClass content),
    namespace_boxed = catMaybes (map extractBoxed content),
    namespace_enums = catMaybes (map extractEnum content),
    namespace_misc = catMaybes (map extractMisc content)
  }
extractNameSpace _ = Nothing

extractEnum :: Xml.Content -> Maybe Enum
extractEnum (Xml.CElem (Xml.Elem "enum"
                     [("name", Xml.AttValue name),
                      ("cname", Xml.AttValue cname),
                      ("gtype", Xml.AttValue gtype),
                      ("type", Xml.AttValue variety)] members)) =
  Just $ Enum {
    enum_name = Xml.verbatim name,
    enum_cname = Xml.verbatim cname,
    enum_variety = case Xml.verbatim variety of
                     "enum" -> EnumVariety
                     "flags" -> FlagsVariety,
    enum_members = map extractEnumMember members
  }
extractEnum (Xml.CElem (Xml.Elem "enum"
                     [("name", Xml.AttValue name),
                      ("cname", Xml.AttValue cname),
                      ("type", Xml.AttValue variety)] members)) =
  Just $ Enum {
    enum_name = Xml.verbatim name,
    enum_cname = Xml.verbatim cname,
    enum_variety = case Xml.verbatim variety of
                     "enum" -> EnumVariety
                     "flags" -> FlagsVariety,
    enum_members = map extractEnumMember members
  }
extractEnum (Xml.CElem (Xml.Elem "enum"
                     [("name", Xml.AttValue name),
                      ("cname", Xml.AttValue cname),
                      ("deprecated", Xml.AttValue [Left "1"]),
                      ("gtype", Xml.AttValue gtype),
                      ("type", Xml.AttValue variety)] members)) =
  Just $ Enum {
    enum_name = Xml.verbatim name,
    enum_cname = Xml.verbatim cname,
    enum_variety = case Xml.verbatim variety of
                     "enum" -> EnumVariety
                     "flags" -> FlagsVariety,
    enum_members = map extractEnumMember members
  }
extractEnum (Xml.CElem (Xml.Elem "enum"
                     [("name", Xml.AttValue name),
                      ("cname", Xml.AttValue cname),
                      ("deprecated", Xml.AttValue [Left "1"]),
                      ("type", Xml.AttValue variety)] members)) =
  Just $ Enum {
    enum_name = Xml.verbatim name,
    enum_cname = Xml.verbatim cname,
    enum_variety = case Xml.verbatim variety of
                     "enum" -> EnumVariety
                     "flags" -> FlagsVariety,
    enum_members = map extractEnumMember members
  }
extractEnum other@(Xml.CElem (Xml.Elem "enum" _ _)) = error $ "extractEnum: " ++ Xml.verbatim other
extractEnum _ = Nothing

extractEnumMember :: Xml.Content -> Member
extractEnumMember (Xml.CElem (Xml.Elem "member"
                     (("cname", Xml.AttValue cname):
                      ("name", Xml.AttValue name):value) [])) =
  Member {
    member_name = Xml.verbatim name,
    member_cname = Xml.verbatim cname,
    member_value =
      case value of
        [] -> ""
        [("value", Xml.AttValue value)] -> Xml.verbatim value
  }

extractObject :: Xml.Content -> Maybe Object
extractObject (Xml.CElem (Xml.Elem "object"
                     (("name", Xml.AttValue name):
                      ("cname", Xml.AttValue cname):
                      remainder) content)) =
  let (parent, deprecated) =
        case remainder of
          [] -> ([Left "Unknown"], False)
          [("parent", Xml.AttValue parent)] -> (parent, False)
          [("deprecated", Xml.AttValue deprecated),
           ("parent", Xml.AttValue parent)] -> (parent, True)
  in Just $ Object {
    object_name = Xml.verbatim name,
    object_cname = Xml.verbatim cname,
    object_parent = Xml.verbatim parent,
    object_constructors = catMaybes (map extractConstructor content),
    object_methods = catMaybes (map extractMethod content),
    object_properties = catMaybes (map extractProperty content),
    object_childprops = catMaybes (map extractChildProperty content),
    object_signals = catMaybes (map extractSignal content),
    object_implements = concat (catMaybes (map extractImplements content)),
    object_deprecated = deprecated,
    object_isinterface = False
  }
extractObject (Xml.CElem (Xml.Elem "interface"
                     [("name", Xml.AttValue name),
                      ("cname", Xml.AttValue cname)] content)) =
  Just $ Object {
    object_name = Xml.verbatim name,
    object_cname = Xml.verbatim cname,
    object_parent = "GObject",
    object_constructors = catMaybes (map extractConstructor content),
    object_methods = catMaybes (map extractMethod content),
    object_properties = catMaybes (map extractProperty content),
    object_childprops = [],
    object_signals = catMaybes (map extractSignal content),
    object_implements = concat (catMaybes (map extractImplements content)),
    object_deprecated = False,
    object_isinterface = True
  }
extractObject (Xml.CElem (Xml.Elem "object" [("name", Xml.AttValue name)] [])) | null (Xml.verbatim name) = Nothing
extractObject other@(Xml.CElem (Xml.Elem "object" _ _)) = error $ "extractObject: " ++ Xml.verbatim other
extractObject other@(Xml.CElem (Xml.Elem "interface" _ _)) = error $ "extractObject: " ++ Xml.verbatim other
extractObject _ = Nothing

extractClass :: Xml.Content -> Maybe Class
extractClass (Xml.CElem (Xml.Elem "class"
                     [("name", Xml.AttValue name),
                      ("cname", Xml.AttValue cname)] content)) =
  Just $ Class {
    class_name = Xml.verbatim name,
    class_cname = Xml.verbatim cname,
    class_methods = catMaybes (map extractMethod content)
  }
extractClass _ = Nothing

extractBoxed :: Xml.Content -> Maybe Boxed
extractBoxed (Xml.CElem (Xml.Elem "boxed"
                     (("name", Xml.AttValue name):
                      ("cname", Xml.AttValue cname):remainder) content)) =
  Just $ Boxed {
    boxed_name = Xml.verbatim name,
    boxed_cname = Xml.verbatim cname,
    boxed_methods = catMaybes (map extractMethod content),
    boxed_constructors = catMaybes (map extractConstructor content),
    boxed_fields = catMaybes (map extractField content),
    boxed_opaque = case remainder of
                     [] -> False
                     [("opaque", _)] -> True
  }
extractBoxed _ = Nothing

extractField :: Xml.Content -> Maybe Field
extractField (Xml.CElem (Xml.Elem "field"
                     [("name", Xml.AttValue name),
                      ("cname", Xml.AttValue cname),
                      ("type", Xml.AttValue type_)] content)) =
  Just $ Field {
    field_name = Xml.verbatim name,
    field_cname = Xml.verbatim cname,
    field_type = Xml.verbatim type_,
    field_public = False,
    field_bits = -1
  }
extractField (Xml.CElem (Xml.Elem "field"
                     [("name", Xml.AttValue name),
                      ("cname", Xml.AttValue cname),
                      ("type", Xml.AttValue type_),
                      ("access", Xml.AttValue [Left "public"])] content)) =
  Just $ Field {
    field_name = Xml.verbatim name,
    field_cname = Xml.verbatim cname,    
    field_type = Xml.verbatim type_,
    field_public = True,
    field_bits = -1
  }
extractField (Xml.CElem (Xml.Elem "field"
                     [("name", Xml.AttValue name),
                      ("cname", Xml.AttValue cname),
                      ("bits", Xml.AttValue bits),
                      ("type", Xml.AttValue type_)] content)) =
  Just $ Field {
    field_name = Xml.verbatim name,
    field_cname = Xml.verbatim cname,
    field_type = Xml.verbatim type_,
    field_public = False,
    field_bits = read (Xml.verbatim bits)
  }
extractField _ = Nothing

extractMethod :: Xml.Content -> Maybe Method
extractMethod (Xml.CElem (Xml.Elem "method"
                     (("name", Xml.AttValue name):
                      ("cname", Xml.AttValue cname):
                      remainder)
                     (Xml.CElem (Xml.Elem "return-type"
                            (("type", Xml.AttValue return_type):remainder') [])
                      :content))) =
  let (shared, deprecated) =
        case remainder of 
          []                                 -> (False, False)
          [("shared", _)]                    -> (True,  False)
          [("deprecated", _)]                -> (False, True)
          [("deprecated", _), ("shared", _)] -> (True,  True)
      owned =
        case remainder' of
          []                                      -> False
          [("owned", Xml.AttValue [Left "true"])] -> True
  in Just $ Method {
    method_name = Xml.verbatim name,
    method_cname = Xml.verbatim cname,
    method_return_type = Xml.verbatim return_type,
    method_return_owned = owned,
    method_parameters =
      case content of
        [] -> []
        [Xml.CElem (Xml.Elem "parameters" [] parameters)]
           -> map extractParameter parameters,
   method_shared = shared,
   method_deprecated = deprecated
  }
extractMethod other@(Xml.CElem (Xml.Elem "method" _ _)) = error $ "extractMethod: " ++ Xml.verbatim other
extractMethod _ = Nothing

extractParameter :: Xml.Content -> Parameter
extractParameter (Xml.CElem (Xml.Elem "parameter"
                        [("ellipsis", _)] [])) = VarArgs
extractParameter (Xml.CElem (Xml.Elem "parameter"
                        [("ellipsis", _)
                        ,("printf_format_args", _)] [])) = VarArgs
extractParameter (Xml.CElem (Xml.Elem "parameter"
                        [("type", Xml.AttValue type_),
                         ("name", Xml.AttValue name)] [])) =
  Parameter {
    parameter_type = Xml.verbatim type_,
    parameter_name = Xml.verbatim name,
    parameter_out  = False,
    parameter_isArray = False
  }
extractParameter (Xml.CElem (Xml.Elem "parameter"
                        [("type", Xml.AttValue type_),
                         ("pass_as", Xml.AttValue [Left "out"]),
                         ("name", Xml.AttValue name)] [])) =
  Parameter {
    parameter_type = Xml.verbatim type_,
    parameter_name = Xml.verbatim name,
    parameter_out  = True,
    parameter_isArray = False
  }
extractParameter (Xml.CElem (Xml.Elem "parameter"
                        [("name", Xml.AttValue name),
                         ("type", Xml.AttValue type_)] [])) =
  Parameter {
    parameter_type = Xml.verbatim type_,
    parameter_name = Xml.verbatim name,
    parameter_out  = False,
    parameter_isArray = False
  }
extractParameter (Xml.CElem (Xml.Elem "parameter"
                        [("type", Xml.AttValue type_),
                         ("name", Xml.AttValue name),
                         ("printf_format" ,_)] [])) =
  Parameter {
    parameter_type = Xml.verbatim type_,
    parameter_name = Xml.verbatim name,
    parameter_out  = False,
    parameter_isArray = False
  }
extractParameter (Xml.CElem (Xml.Elem "parameter"
                        [("type", Xml.AttValue type_),
			 ("array", _),
                         ("name", Xml.AttValue name)] [])) =
   Parameter {
     parameter_type = Xml.verbatim type_,
     parameter_name = Xml.verbatim name,
     parameter_out  = False,
     parameter_isArray = True
   }
extractParameter (Xml.CElem (Xml.Elem "callback"
                        [("cname", Xml.AttValue cname)] _)) =
   Parameter {
     parameter_type = "callback",
     parameter_name = Xml.verbatim cname,
     parameter_out  = False,
     parameter_isArray = False
   }
extractParameter other = error $ "extractParameter: " ++ Xml.verbatim other
  
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

extractProperty :: Xml.Content -> Maybe Property
extractProperty (Xml.CElem (Xml.Elem "property"
                     (("name", Xml.AttValue name):
                      ("cname", Xml.AttValue cname):
                      ("type", Xml.AttValue type_):others) [])) =
  Just $ Property {
    property_name = Xml.verbatim name,
    property_cname = Xml.verbatim cname,
    property_type = Xml.verbatim type_,
    property_readable  = (not.null) [ () | ("readable", _) <- others],
    property_writeable = (not.null) [ () | ("writeable", _) <- others],
    property_construct = (not.null) [ () | ("construct", _) <- others],
    property_constructonly  = (not.null) [ () | ("construct-only", _) <- others]
  }
extractProperty _ = Nothing

extractChildProperty :: Xml.Content -> Maybe Property
extractChildProperty (Xml.CElem (Xml.Elem "childprop"
                     (("name", Xml.AttValue name):
                      ("cname", Xml.AttValue cname):
                      ("type", Xml.AttValue type_):others) [])) =
  Just $ Property {
    property_name = Xml.verbatim name,
    property_cname = Xml.verbatim cname,
    property_type = Xml.verbatim type_,
    property_readable  = (not.null) [ () | ("readable", _) <- others],
    property_writeable = (not.null) [ () | ("writeable", _) <- others],
    property_construct = (not.null) [ () | ("construct", _) <- others],
    property_constructonly  = (not.null) [ () | ("construct-only", _) <- others]
  }
extractChildProperty _ = Nothing

extractSignal :: Xml.Content -> Maybe Signal
extractSignal (Xml.CElem (Xml.Elem "signal"
                     (("name", Xml.AttValue name):
                      ("cname", Xml.AttValue cname):remainder)
                     (Xml.CElem (Xml.Elem "return-type"
                            [("type", Xml.AttValue return_type)] [])
                      :content))) =
  Just $ Signal {
    signal_name = Xml.verbatim name,
    signal_cname = Xml.verbatim cname,
    signal_when = head [ Xml.verbatim when
                       | ("when", Xml.AttValue when) <- remainder ] ++ "",
    signal_action = isJust $ find ((== "action") . fst) remainder,
    signal_return_type = Xml.verbatim return_type,
    signal_parameters =
      case content of
        [] -> []
        [Xml.CElem (Xml.Elem "parameters" [] parameters)]
           -> map extractParameter parameters
  }
extractSignal _ = Nothing

extractImplements :: Xml.Content -> Maybe [String]
extractImplements (Xml.CElem (Xml.Elem "implements" [] interfaces)) =
  Just $ map extractInterface interfaces
extractImplements _ = Nothing

extractInterface :: Xml.Content -> String
extractInterface (Xml.CElem (Xml.Elem "interface"
                    [("cname", Xml.AttValue cname)] [] )) = Xml.verbatim cname

extractMisc :: Xml.Content -> Maybe Misc
extractMisc (Xml.CElem (Xml.Elem elem
                  (("name", Xml.AttValue name):
                   ("cname", Xml.AttValue cname):_) _))
  | elem == "struct"   = Just Struct {
                                misc_name = Xml.verbatim name,
                                misc_cname = Xml.verbatim cname
                              }
  | elem == "alias"    = Just Alias {
                                misc_name = Xml.verbatim name,
                                misc_cname = Xml.verbatim cname
                              }
  | elem == "callback" = Just Callback {
                                misc_name = Xml.verbatim name,
                                misc_cname = Xml.verbatim cname
                              }
extractMisc (Xml.CElem (Xml.Elem "object" _ _))    = Nothing
extractMisc (Xml.CElem (Xml.Elem "class" _ _))     = Nothing
extractMisc (Xml.CElem (Xml.Elem "boxed" _ _))     = Nothing
extractMisc (Xml.CElem (Xml.Elem "interface" _ _)) = Nothing
extractMisc (Xml.CElem (Xml.Elem "enum" _ _))      = Nothing
extractMisc other = error $ "extractMisc: " ++ Xml.verbatim other
