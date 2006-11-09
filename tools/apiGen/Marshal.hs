module Marshal (
  KnownSymbols,
  CSymbol(..),
  EnumKind(..),
  ParameterKind(..),
  genMarshalParameter,
  genMarshalOutParameter,
  genMarshalResult,
  genMarshalProperty,
  convertSignalType,
  genCall
  ) where

import MarshalFixup

import Utils
import Data.Char (isUpper)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)

type KnownSymbols = Map String CSymbol

data CSymbol = SymObjectType { sym_object_parents :: [String] }
             | SymEnumType   EnumKind
             | SymEnumValue
             | SymStructType
             | SymBoxedType
             | SymClassType
             | SymTypeAlias
             | SymCallbackType
  deriving (Eq, Show)

data EnumKind = EnumKind | FlagsKind
  deriving (Eq, Show)

symbolIsObject (Just (SymObjectType _)) = True
symbolIsObject _                        = False

symbolIsEnum (Just (SymEnumType EnumKind)) = True
symbolIsEnum _                             = False

symbolIsFlags (Just (SymEnumType FlagsKind)) = True
symbolIsFlags _                              = False

symbolIsBoxed (Just SymBoxedType) = True
symbolIsBoxed _                   = False

-------------------------------------------------------------------------------
-- More doc formatting utils
-------------------------------------------------------------------------------

c2hsHook name d = text "{#" <+> text name <+> d <+> text "#}"
lambda var = char '\\' <> var <+> text "->"
ptr var = text var <> text "Ptr"

-------------------------------------------------------------------------------
-- Here's the interesting bit that generates the fragments of mashaling code
-------------------------------------------------------------------------------

data ParameterKind = InParam  String
                   | OutParam String
                   | UnusedParam

genMarshalParameter ::
        KnownSymbols -> --a collection of types we know to be objects or enums
        String ->       --function name (useful to lookup per-func fixup info)
	String ->	--parameter name suggestion (will be unique)
	String -> 	--C type decleration for the parameter we will marshal
	(Maybe String,	--parameter class constraints (or none)
	ParameterKind,	--parameter type (or UnusedParam if the arg is not exposed)
	Doc -> Doc)	--marshaling code (\body -> ... body ...)

genMarshalParameter _ _ name "gboolean" =
	(Nothing, InParam "Bool",
	\body -> body
              $$ nest 2 (parens (text "fromBool" <+> text name)))

genMarshalParameter _ _ name typeName
			   | typeName == "guint"  --these two are unsigned types
			  || typeName == "gint"
			  || typeName == "glong"
			  || typeName == "int"
			  || typeName == "gsize"  --should they be Word or Int?
			  || typeName == "gssize" =
	(Nothing, InParam "Int",
	\body -> body
              $$ nest 2 (parens (text "fromIntegral" <+> text name)))

genMarshalParameter _ _ name "guint16" =
	(Nothing, InParam "Word16",
	\body -> body
              $$ nest 2 (parens (text "fromIntegral" <+> text name)))

genMarshalParameter _ _ name "guint32" =
	(Nothing, InParam "Word32",
	\body -> body
              $$ nest 2 (parens (text "fromIntegral" <+> text name)))

genMarshalParameter _ _ name typeName
                           | typeName == "gdouble"
                          || typeName == "double" =
	(Nothing, InParam "Double",
	\body -> body
              $$ nest 2 (parens (text "realToFrac" <+> text name)))

genMarshalParameter _ _ name "gfloat" =
	(Nothing, InParam "Float",
	\body -> body
              $$ nest 2 (parens (text "realToFrac" <+> text name)))

genMarshalParameter _ _ name "gunichar" =
	(Nothing, InParam "Char",
	\body -> body
              $$ nest 2 (parens (text "(fromIntegral . ord)" <+> text name)))

genMarshalParameter _ funcName name typeName | typeName == "const-gchar*"
                                            || typeName == "const-char*" =
  if maybeNullParameter funcName name
    then (Nothing, InParam "Maybe String",
	 \body -> text "maybeWith withUTFString" <+> text name <+> char '$' <+> lambda (ptr name)
               $$ body
               $$ nest 2 (text name <> text "Ptr"))
    else (Nothing, InParam "String",
	 \body -> text "withUTFString" <+> text name <+> char '$' <+> lambda (ptr name)
               $$ body
               $$ nest 2 (ptr name))

genMarshalParameter _ _ name "GError**" =
	(Nothing, UnusedParam,
	\body -> text "propagateGError $" <+> lambda (text name <> text "Ptr")
	      $$ body
              $$ nest 2 (ptr name))

-- Objects -----------------------------
genMarshalParameter knownSymbols funcName name typeName'
            | isUpper (head typeName')
           && last typeName' == '*'
           && last typeName /= '*'
           && symbolIsObject typeKind =
  let classContext
        | leafClass typeName = Nothing
        | otherwise          = Just $ shortTypeName ++ "Class " ++ name
      argType = (if maybeNullParameter funcName name then "Maybe " else "")
             ++ (if leafClass typeName then shortTypeName else name)
      implementation
        | leafClass typeName && maybeNullParameter funcName name
                             = parens (text "fromMaybe"
                                   <+> parens (text shortTypeName <+> text "nullForeignPtr")
                                   <+> text name)
        | leafClass typeName = text name
        | maybeNullParameter funcName name
                             = parens (text "maybe"
                                   <+> parens (text shortTypeName <+> text "nullForeignPtr")
                                   <+> text "to" <> text shortTypeName <+> text name)
        | otherwise          = parens (text "to" <> text shortTypeName <+> text name)
   in (classContext, InParam argType,
      \body -> body
            $$ nest 2 implementation)
  where typeName = init typeName'
        shortTypeName = cTypeNameToHSType typeName
        typeKind = Map.lookup typeName knownSymbols

-- Enums -------------------------------
genMarshalParameter knownSymbols _ name typeName
            | isUpper (head typeName)
           && symbolIsEnum typeKind =
	(Nothing, InParam shortTypeName,
	\body -> body
              $$ nest 2 (parens (text "(fromIntegral . fromEnum)" <+> text name)))
  where shortTypeName = cTypeNameToHSType typeName
        typeKind = Map.lookup typeName knownSymbols

-- Flags -------------------------------
genMarshalParameter knownSymbols _ name typeName
            | isUpper (head typeName)
           && symbolIsFlags typeKind =
	(Nothing, InParam ("[" ++ shortTypeName ++ "]"),
	\body -> body
              $$ nest 2 (parens (text "(fromIntegral . fromFlags)" <+> text name)))
  where shortTypeName = cTypeNameToHSType typeName
        typeKind = Map.lookup typeName knownSymbols

genMarshalParameter _ _ name textIter | textIter == "const-GtkTextIter*"
                                     || textIter == "GtkTextIter*" =
	(Nothing, InParam "TextIter",
	\body -> body
              $$ nest 2 (text name))

genMarshalParameter _ funcName name "GtkTreeIter*" =
  if maybeNullParameter funcName name
    then (Nothing, InParam "Maybe TreeIter",
         \body -> body
               $$ nest 2 (parens (text "fromMaybe (TreeIter nullForeignPtr)" <+> text name)))
    else (Nothing, InParam "TreeIter",
         \body -> body
               $$ nest 2 (text name))

genMarshalParameter _ funcName name "GtkTreePath*" =
  if maybeNullParameter funcName name
    then (Nothing, InParam "Maybe TreePath",
	 \body -> text "maybeWith withTreePath" <+> text name <+> char '$' <+> lambda (text name)
               $$ body
               $$ nest 2 (text name))
    else (Nothing, InParam "TreePath",
	 \body -> text "withTreePath" <+> text name <+> char '$' <+> lambda (text name)
               $$ body
               $$ nest 2 (text name))

genMarshalParameter _ _ name "const-GdkColor*" =
	(Nothing, InParam "Color",
	\body -> text "with" <+> text name <+> char '$' <+> lambda (ptr name)
              $$ body
              $$ nest 2 (ptr name))

-- Out parameters -------------------------------

genMarshalParameter _ _ name "gboolean*" =
	(Nothing, OutParam "Boolean",
	\body -> body
              $$ nest 2 (ptr name))

genMarshalParameter _ _ name typeName
			   | typeName == "gint*"
			  || typeName == "guint*"
			  || typeName == "glong*" =
	(Nothing, OutParam "Int",
	\body -> body
              $$ nest 2 (ptr name))

genMarshalParameter _ _ name "gfloat*" =
	(Nothing, OutParam "Float",
	\body -> body
              $$ nest 2 (ptr name))

genMarshalParameter _ _ name "gdouble*" =
	(Nothing, OutParam "Double",
	\body -> body
              $$ nest 2 (ptr name))

genMarshalParameter _ _ name "gchar**" =
	(Nothing, OutParam "String",
	\body -> body
              $$ nest 2 (ptr name))

genMarshalParameter _ _ name "GdkColor*" =
	(Nothing, OutParam "Color",
	\body -> body
              $$ nest 2 (ptr name))

-- Catch all case -------------------------------
genMarshalParameter _ _ name unknownType =
	(Nothing, InParam $ "{-" ++ unknownType ++ "-}",
	\body -> body
              $$ nest 2 (text "{-" <> text name <> text "-}"))


genMarshalOutParameter :: String -> String -> (Doc, Doc, Doc)
genMarshalOutParameter "Boolean" name = (text "alloca" <+> char '$' <+> lambda (ptr name)
                                        ,text "peek" <+> ptr name <+> text ">>=" <+> lambda (text name)
                                        ,text "toBool" <+> text name)

genMarshalOutParameter "Int"     name = (text "alloca" <+> char '$' <+> lambda (ptr name)
                                        ,text "peek" <+> ptr name <+> text ">>=" <+> lambda (text name)
                                        ,text "fromIntegral" <+> text name)

genMarshalOutParameter "Float"   name = (text "alloca" <+> char '$' <+> lambda (ptr name)
                                        ,text "peek" <+> ptr name <+> text ">>=" <+> lambda (text name)
                                        ,text "realToFrac" <+> text name)

genMarshalOutParameter "Double"  name = (text "alloca" <+> char '$' <+> lambda (ptr name)
                                        ,text "peek" <+> ptr name <+> text ">>=" <+> lambda (text name)
                                        ,text "realToFrac" <+> text name)
genMarshalOutParameter "String"  name = (text "alloca" <+> char '$' <+> lambda (ptr name)
                                        ,text "peek" <+> ptr name <+> text ">>= readUTFString >>=" <+> lambda (text name)
                                        ,text name)
genMarshalOutParameter "Color"   name = (text "alloca" <+> char '$' <+> lambda (ptr name)
                                        ,text "peek" <+> ptr name <+> text ">>=" <+> lambda (text name)
                                        ,text name)

genMarshalOutParameter _         name = (empty, empty, text name)

-- Takes the type string and returns the Haskell Type and the marshaling code
--
genMarshalResult :: 
        KnownSymbols -> --a collection of types we know to be objects or enums
        String ->       --function name (useful to lookup per-func fixup info)
        Bool ->         --is the function a constructor or ordinary method?
	String -> 	--C type decleration for the return value we will marshal
	(String,	--Haskell return type 
	Doc -> Doc)	--marshaling code (\body -> ... body ...)
genMarshalResult _ _ _ "gboolean" = ("Bool", \body -> text "liftM toBool $" $$ body)
genMarshalResult _ _ _ "gint"     = ("Int",  \body -> text "liftM fromIntegral $"   $$ body)
genMarshalResult _ _ _ "guint"    = ("Int",  \body -> text "liftM fromIntegral $"   $$ body)
genMarshalResult _ _ _ "guint16"  = ("Word16", \body -> text "liftM fromIntegral $" $$ body)
genMarshalResult _ _ _ "guint32"  = ("Word32", \body -> text "liftM fromIntegral $" $$ body)
genMarshalResult _ _ _ "glong"    = ("Int",    \body -> text "liftM fromIntegral $" $$ body)
genMarshalResult _ _ _ "gdouble"  = ("Double", \body -> text "liftM realToFrac $"   $$ body)
genMarshalResult _ _ _ "gfloat"   = ("Float",  \body -> text "liftM realToFrac $"   $$ body)
genMarshalResult _ _ _ "gunichar" = ("Char", \body -> text "liftM (chr . fromIntegral) $" $$ body)
genMarshalResult _ _ _ "void"     = ("()", id)
genMarshalResult _ funcName _ "const-gchar*" =
  if maybeNullResult funcName
    then ("(Maybe String)",
         \body -> body
               $$ text ">>= maybePeek peekUTFString")
    else ("String",
         \body -> body
               $$ text ">>= peekUTFString")
genMarshalResult _ funcName _ typeName 
                            | typeName == "gchar*"
			   || typeName == "char*" =
  if maybeNullResult funcName
    then ("(Maybe String)",
         \body -> body
               $$ text ">>= maybePeek readUTFString")
    else ("String",
         \body -> body
               $$ text ">>= readUTFString")
genMarshalResult _ _ _ "const-GSList*" =
  ("[{- element type -}]",
  \body -> body
        $$ text ">>= readGSList"
        $$ text ">>= mapM (\\elemPtr -> {-marshal elem-})")
genMarshalResult _ _ _ "GSList*" =
  ("[{- element type -}]",
  \body -> body
        $$ text ">>= fromGSList"
        $$ text ">>= mapM (\\elemPtr -> {-marshal elem-})")
genMarshalResult _ _ _ "GList*" =
  ("[{- element type -}]",
  \body -> body
        $$ text ">>= fromGList"
        $$ text ">>= mapM (\\elemPtr -> {-marshal elem-})")

genMarshalResult _ _ _ "GtkTreePath*" =
  ("TreePath",
  \body -> body
        $$ text ">>= fromTreePath")

genMarshalResult knownSymbols funcName funcIsConstructor typeName'
            | isUpper (head typeName')
           && last typeName' == '*'
           && last typeName /= '*'
           && symbolIsObject typeKind =
  if maybeNullResult funcName
    then ("(Maybe " ++ shortTypeName ++ ")",
         \body -> text "maybeNull" <+> parens (text constructor <+> text "mk" <> text shortTypeName) <+> char '$'
               $$ cast
               $$ body)
    else (shortTypeName,
         \body -> text constructor <+> text "mk" <> text shortTypeName <+> char '$'
               $$ cast
               $$ body)
  where typeName = init typeName'
        shortTypeName = cTypeNameToHSType typeName
        typeKind = Map.lookup typeName knownSymbols
        constructor | "GtkObject" `elem` sym_object_parents (fromJust typeKind)
                                = "makeNewObject"
                    | "GObject" `elem` sym_object_parents (fromJust typeKind)
                                = if funcIsConstructor then "constructNewGObject"
                                                       else "makeNewGObject"
        cast | funcIsConstructor
            && constructorReturnType /= typeName = 
            text "liftM (castPtr :: Ptr" <+> text (cTypeNameToHSType constructorReturnType)
                       <+> text "-> Ptr" <+> text (cTypeNameToHSType typeName) <> text ") $"
             | otherwise = empty
          where constructorReturnType | "GtkToolItem" `elem` sym_object_parents (fromJust typeKind)
                                                  = "GtkToolItem"
                                      | "GtkWidget" `elem` sym_object_parents (fromJust typeKind)
                                                  = "GtkWidget"                                      
                                      | otherwise = typeName
            
genMarshalResult knownSymbols _ _ typeName
            | isUpper (head typeName)
           && symbolIsEnum typeKind =
  (shortTypeName,
  \body -> text "liftM (toEnum . fromIntegral) $"
        $$ body)
  where shortTypeName = cTypeNameToHSType typeName
        typeKind = Map.lookup typeName knownSymbols

genMarshalResult knownSymbols _ _ typeName
            | isUpper (head typeName)
           && symbolIsFlags typeKind =
  ("[" ++ shortTypeName ++ "]",
  \body -> text "liftM (toFlags . fromIntegral) $"
        $$ body)
  where shortTypeName = cTypeNameToHSType typeName
        typeKind = Map.lookup typeName knownSymbols

genMarshalResult _ _ _ unknownType = ("{-" ++ unknownType ++ "-}", id)

-- Takes the type string and returns the Haskell Type and the GValue variety
--
genMarshalProperty :: KnownSymbols -> String -> (String, String)
genMarshalProperty _ "gint"      = ("Int",    "Int")
genMarshalProperty _ "guint"     = ("Int",    "UInt")
genMarshalProperty _ "gfloat"    = ("Float",  "Float")
genMarshalProperty _ "gdouble"   = ("Double", "Double")
genMarshalProperty _ "gboolean"  = ("Bool",   "Bool")
genMarshalProperty _ "gunichar"  = ("Char",   "Char")
genMarshalProperty _ "gchar*"    = ("String", "String")
genMarshalProperty _ "GStrv"     = ("[String]", "Strings")

genMarshalProperty knownSymbols typeName
            | isUpper (head typeName)
           && symbolIsObject typeKind =
  (shortTypeName, "Object")
  where shortTypeName = cTypeNameToHSType typeName
        typeKind = Map.lookup typeName knownSymbols

genMarshalProperty knownSymbols typeName
            | isUpper (head typeName)
           && symbolIsEnum typeKind =
  (shortTypeName, "Enum")
  where shortTypeName = cTypeNameToHSType typeName
        typeKind = Map.lookup typeName knownSymbols

genMarshalProperty knownSymbols typeName
            | isUpper (head typeName)
           && symbolIsFlags typeKind =
  (shortTypeName, "Flags")
  where shortTypeName = cTypeNameToHSType typeName
        typeKind = Map.lookup typeName knownSymbols

genMarshalProperty _ unknown = ("{-" ++ unknown ++ "-}", "{-" ++ unknown ++ "-}")

-- Takes the type string and returns the signal marshaing category and the
-- Haskell type
--
convertSignalType :: KnownSymbols -> String -> (String, String)
convertSignalType _ "void"     = ("NONE",   "()")
convertSignalType _ "gchar"    = ("CHAR",   "Char")
convertSignalType _ "guchar"   = ("UCHAR",  "Char")
convertSignalType _ "gboolean" = ("BOOL",   "Bool")
convertSignalType _ "gint"     = ("INT",    "Int")
convertSignalType _ "guint"    = ("UINT",   "Int")
convertSignalType _ "guint32"  = ("UINT",   "Int")
convertSignalType _ "glong"    = ("LONG",   "Int")
convertSignalType _ "gulong"   = ("ULONG",  "Int")
convertSignalType _ "gfloat"   = ("FLOAT",  "Float")
convertSignalType _ "gdouble"  = ("DOUBLE", "Double")
convertSignalType _ "gchar*"   = ("STRING", "String")
convertSignalType _ "const-gchar*" = ("STRING", "String")
convertSignalType knownSymbols typeName
  | symbolIsEnum   typeKind    = ("ENUM",  cTypeNameToHSType typeName)
  | symbolIsFlags  typeKind    = ("FLAGS", cTypeNameToHSType typeName)
  where typeKind = Map.lookup typeName knownSymbols
convertSignalType knownSymbols typeName@(_:_)
    | last typeName == '*'
   && symbolIsBoxed  typeKind  = ("BOXED",  cTypeNameToHSType (init typeName))
    | last typeName == '*'
   && symbolIsObject typeKind  = ("OBJECT", cTypeNameToHSType (init typeName))
  where typeKind = Map.lookup (init typeName) knownSymbols
convertSignalType _ typeName   =  ("{-" ++ typeName ++ "-}", "{-" ++ typeName ++ "-}")

-------------------------------------------------------------------------------
-- Now for some special cases, we can override the generation of {# call #}'s
-------------------------------------------------------------------------------

-- The ordinary case:
genCallOrdinary :: String -> Bool -> Doc
genCallOrdinary cname _unsafe@True  = c2hsHook "call unsafe" (text cname)
genCallOrdinary cname _unsafe@False = c2hsHook "call" (text cname)

-- On win32 for glib/gtk 2.6 they changed the interpretation of functions that
-- take or return system file names (as opposed to user displayable
-- representations of file names). Previously the string encoding of the file
-- name was that of the systems native 'codepage' which was usually ascii but
-- could be one of several obscure multi-byte encodings. For 2.6 they have
-- changed to always use a UTF8 encoding. However to maintain binary backwards
-- compatability they kept the old names and added new ones with a _utf8 suffix
-- for the new interpretation. However the old names are only in the binary,
-- they are not exposed through the C header files so all software building
-- against glib/gtk 2.6 on windows must use the _utf8 versions. Hence we
-- generate code uses the _utf8 version if we're building on windows and using
-- gtk version 2.6 or later. Ugh.

genCall :: String -> Bool -> Doc
genCall cname safty | cname `Set.member` win32FileNameFunctions
                           = nest (-2) (text "#if defined (WIN32) && GTK_CHECK_VERSION(2,6,0)")
                          $$ genCallOrdinary (cname ++ "_utf8") safty
                          $$ nest (-2) (text "#else")
                          $$ genCallOrdinary cname safty
                          $$ nest (-2) (text "#endif")
genCall cname unsafe = genCallOrdinary cname unsafe
