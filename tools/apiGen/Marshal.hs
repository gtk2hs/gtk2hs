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

import StringUtils
import Char (isUpper)
import Maybe (fromJust)
import Data.FiniteMap

type KnownSymbols = FiniteMap String CSymbol

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
	ShowS -> ShowS)	--marshaling code (\body -> ... body ...)

genMarshalParameter _ _ name "gboolean" =
	(Nothing, InParam "Bool",
	\body -> body.
                 indent 2. ss "(fromBool ". ss name. ss ")")

genMarshalParameter _ _ name typeName
			   | typeName == "guint"  --these two are unsigned types
			  || typeName == "gint"
			  || typeName == "glong"
			  || typeName == "int"
			  || typeName == "gsize"  --should they be Word or Int?
			  || typeName == "gssize" =
	(Nothing, InParam "Int",
	\body -> body.
                 indent 2. ss "(fromIntegral ". ss name. ss ")")

genMarshalParameter _ _ name "guint16" =
	(Nothing, InParam "Word16",
	\body -> body.
                 indent 2. ss "(fromIntegral ". ss name. ss ")")

genMarshalParameter _ _ name "guint32" =
	(Nothing, InParam "Word32",
	\body -> body.
                 indent 2. ss "(fromIntegral ". ss name. ss ")")

genMarshalParameter _ _ name typeName
                           | typeName == "gdouble"
                          || typeName == "double" =
	(Nothing, InParam "Double",
	\body -> body.
                 indent 2. ss "(realToFrac ". ss name. ss ")")

genMarshalParameter _ _ name "gfloat" =
	(Nothing, InParam "Float",
	\body -> body.
                 indent 2. ss "(realToFrac ". ss name. ss ")")

genMarshalParameter _ _ name "gunichar" =
	(Nothing, InParam "Char",
	\body -> body.
                 indent 2. ss "((fromIntegral . ord) ". ss name. ss ")")

genMarshalParameter _ funcName name typeName | typeName == "const-gchar*"
                                            || typeName == "const-char*" =
  if maybeNullParameter funcName name
    then (Nothing, InParam "Maybe String",
	 \body -> ss "maybeWith withUTFString ". ss name. ss " $ \\". ss name. ss "Ptr ->".
		  indent 1. body.
                  indent 2. ss name. ss "Ptr")
    else (Nothing, InParam "String",
	 \body -> ss "withUTFString ". ss name. ss " $ \\". ss name. ss "Ptr ->".
		  indent 1. body.
                  indent 2. ss name. ss "Ptr")

genMarshalParameter _ _ name "GError**" =
	(Nothing, UnusedParam,
	\body -> ss "propagateGError $ \\". ss name. ss "Ptr ->".
	         indent 1. body.
                 indent 2. ss name. ss "Ptr")

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
                             = ss "(fromMaybe (". ss shortTypeName. ss " nullForeignPtr) ".
                                                  ss name. ss ")"
        | leafClass typeName = ss name
        | maybeNullParameter funcName name
                             = ss "(maybe (". ss shortTypeName. ss " nullForeignPtr) to".
                                              ss shortTypeName. sc ' '. ss name. sc ')'
        | otherwise          = ss "(to". ss shortTypeName. sc ' '. ss name. sc ')'
   in (classContext, InParam argType,
      \body -> body.
               indent 2. implementation)
  where typeName = init typeName'
        shortTypeName = cTypeNameToHSType typeName
        typeKind = lookupFM knownSymbols typeName

-- Enums -------------------------------
genMarshalParameter knownSymbols _ name typeName
            | isUpper (head typeName)
           && symbolIsEnum typeKind =
	(Nothing, InParam shortTypeName,
	\body -> body.
                 indent 2. ss "((fromIntegral . fromEnum) ". ss name. ss ")")
  where shortTypeName = cTypeNameToHSType typeName
        typeKind = lookupFM knownSymbols typeName

-- Flags -------------------------------
genMarshalParameter knownSymbols _ name typeName
            | isUpper (head typeName)
           && symbolIsFlags typeKind =
	(Nothing, InParam ("[" ++ shortTypeName ++ "]"),
	\body -> body.
                 indent 2. ss "((fromIntegral . fromFlags) ". ss name. ss ")")
  where shortTypeName = cTypeNameToHSType typeName
        typeKind = lookupFM knownSymbols typeName

genMarshalParameter _ _ name textIter | textIter == "const-GtkTextIter*"
                                     || textIter == "GtkTextIter*" =
	(Nothing, InParam "TextIter",
	\body -> body.
                 indent 2. ss name)

genMarshalParameter _ _ name "GtkTreeIter*" =
	(Nothing, InParam "TreeIter",
	\body -> body.
                 indent 2. ss name)

genMarshalParameter _ funcName name "GtkTreePath*" =
  if maybeNullParameter funcName name
    then (Nothing, InParam "Maybe TreePath",
	 \body -> ss "maybeWith withTreePath ". ss name. ss " $ \\". ss name. ss " ->".
		  indent 1. body.
                  indent 2. ss name)
    else (Nothing, InParam "TreePath",
	 \body -> ss "withTreePath ". ss name. ss " $ \\". ss name. ss " ->".
		  indent 1. body.
                  indent 2. ss name)

genMarshalParameter _ _ name "const-GdkColor*" =
	(Nothing, InParam "Color",
	\body -> ss "with ". ss name. ss " $ \\". ss name. ss "Ptr ->".
		 indent 1. body.
                 indent 2. ss name. ss "Ptr")

-- Out parameters -------------------------------

genMarshalParameter _ _ name "gboolean*" =
	(Nothing, OutParam "Boolean",
	\body -> body.
                 indent 2. ss name. ss "Ptr")

genMarshalParameter _ _ name typeName
			   | typeName == "gint*"
			  || typeName == "guint*" =
	(Nothing, OutParam "Int",
	\body -> body.
                 indent 2. ss name. ss "Ptr")

genMarshalParameter _ _ name "gfloat*" =
	(Nothing, OutParam "Float",
	\body -> body.
                 indent 2. ss name. ss "Ptr")

genMarshalParameter _ _ name "gdouble*" =
	(Nothing, OutParam "Double",
	\body -> body.
                 indent 2. ss name. ss "Ptr")

genMarshalParameter _ _ name "gchar**" =
	(Nothing, OutParam "String",
	\body -> body.
                 indent 2. ss name. ss "Ptr")

genMarshalParameter _ _ name "GdkColor*" =
	(Nothing, OutParam "Color",
	\body -> body.
                 indent 2. ss name. ss "Ptr")

-- Catch all case -------------------------------
genMarshalParameter _ _ name unknownType =
	(Nothing, InParam $ "{-" ++ unknownType ++ "-}",
	\body -> body.
                 indent 2. ss "{-". ss name. ss "-}")

genMarshalOutParameter :: String -> String -> (ShowS, ShowS, ShowS)
genMarshalOutParameter "Boolean" name = (ss "alloca $ \\". ss name. ss "Ptr ->". indent 1
                                        ,indent 1. ss "peek ". ss name. ss "Ptr >>= \\". ss name. ss " ->"
                                        ,ss "toBool ". ss name)

genMarshalOutParameter "Int"     name = (ss "alloca $ \\". ss name. ss "Ptr ->". indent 1
                                        ,indent 1. ss "peek ". ss name. ss "Ptr >>= \\". ss name. ss " ->"
                                        ,ss "fromIntegral ". ss name)

genMarshalOutParameter "Float"   name = (ss "alloca $ \\". ss name. ss "Ptr ->". indent 1
                                        ,indent 1. ss "peek ". ss name. ss "Ptr >>= \\". ss name. ss " ->"
                                        ,ss "realToFrac ". ss name)

genMarshalOutParameter "Double"  name = (ss "alloca $ \\". ss name. ss "Ptr ->". indent 1
                                        ,indent 1. ss "peek ". ss name. ss "Ptr >>= \\". ss name. ss " ->"
                                        ,ss "realToFrac ". ss name)
genMarshalOutParameter "String"  name = (ss "alloca $ \\". ss name. ss "Ptr ->". indent 1
                                        ,indent 1. ss "peek ". ss name. ss "Ptr >>= readUTFString >>= \\". ss name. ss " ->"
                                        ,ss name)
genMarshalOutParameter "Color"  name = (ss "alloca $ \\". ss name. ss "Ptr ->". indent 1
                                        ,indent 1. ss "peek ". ss name. ss "Ptr >>= \\". ss name. ss " ->"
                                        ,ss name)

genMarshalOutParameter paramType name = (id, id, ss name)

-- Takes the type string and returns the Haskell Type and the marshaling code
--
genMarshalResult :: 
        KnownSymbols -> --a collection of types we know to be objects or enums
        String ->       --function name (useful to lookup per-func fixup info)
        Bool ->         --is the function a constructor or ordinary method?
	String -> 	--C type decleration for the return value we will marshal
	(String,	--Haskell return type 
	ShowS -> ShowS)	--marshaling code (\body -> ... body ...)
genMarshalResult _ _ _ "gboolean" = ("Bool", \body -> ss "liftM toBool $". indent 1. body)
genMarshalResult _ _ _ "gint"     = ("Int",  \body -> ss "liftM fromIntegral $". indent 1. body)
genMarshalResult _ _ _ "guint"    = ("Int",  \body -> ss "liftM fromIntegral $". indent 1. body)
genMarshalResult _ _ _ "guint16"  = ("Word16", \body -> ss "liftM fromIntegral $". indent 1. body)
genMarshalResult _ _ _ "guint32"  = ("Word32", \body -> ss "liftM fromIntegral $". indent 1. body)
genMarshalResult _ _ _ "gdouble"  = ("Double", \body -> ss "liftM realToFrac $". indent 1. body)
genMarshalResult _ _ _ "gfloat"   = ("Float",  \body -> ss "liftM realToFrac $". indent 1. body)
genMarshalResult _ _ _ "gunichar" = ("Char", \body -> ss "liftM (chr . fromIntegral) $". indent 1. body)
genMarshalResult _ _ _ "void"     = ("()", id)
genMarshalResult _ funcName _ "const-gchar*" =
  if maybeNullResult funcName
    then ("(Maybe String)",
         \body -> body.
                  indent 1. ss  ">>= maybePeek peekUTFString")
    else ("String",
         \body -> body.
                  indent 1. ss  ">>= peekUTFString")
genMarshalResult _ funcName _ typeName 
                            | typeName == "gchar*"
			   || typeName == "char*" =
  if maybeNullResult funcName
    then ("(Maybe String)",
         \body -> body.
                  indent 1. ss  ">>= maybePeek readUTFString")
    else ("String",
         \body -> body.
                  indent 1. ss  ">>= readUTFString")
genMarshalResult _ _ _ "const-GSList*" =
  ("[{- element type -}]",
  \body -> body.
           indent 1. ss ">>= readGSList".
           indent 1. ss ">>= mapM (\\elemPtr -> {-marshal elem-})")
genMarshalResult _ _ _ "GSList*" =
  ("[{- element type -}]",
  \body -> body.
           indent 1. ss ">>= fromGSList".
           indent 1. ss ">>= mapM (\\elemPtr -> {-marshal elem-})")
genMarshalResult _ _ _ "GList*" =
  ("[{- element type -}]",
  \body -> body.
           indent 1. ss ">>= fromGList".
           indent 1. ss ">>= mapM (\\elemPtr -> {-marshal elem-})")

genMarshalResult knownSymbols funcName funcIsConstructor typeName'
            | isUpper (head typeName')
           && last typeName' == '*'
           && last typeName /= '*'
           && symbolIsObject typeKind =
  if maybeNullResult funcName
    then ("(Maybe " ++ shortTypeName ++ ")",
         \body -> ss "maybeNull (" .ss constructor. ss " mk". ss shortTypeName. ss ") $". cast.
                  indent 1. body)
    else (shortTypeName,
         \body -> ss constructor. ss " mk". ss shortTypeName. ss " $". cast.
                  indent 1. body)
  where typeName = init typeName'
        shortTypeName = cTypeNameToHSType typeName
        typeKind = lookupFM knownSymbols typeName
        constructor | "GtkObject" `elem` sym_object_parents (fromJust typeKind)
                                = "makeNewObject"
                    | "GObject" `elem` sym_object_parents (fromJust typeKind)
                                = "makeNewGObject"
        cast | funcIsConstructor
            && constructorReturnType /= typeName = 
            indent 1. ss "liftM (castPtr :: Ptr ". ss (cTypeNameToHSType constructorReturnType).
                                    ss " -> Ptr ". ss (cTypeNameToHSType typeName). ss ") $"
             | otherwise = id
          where constructorReturnType | "GtkWidget" `elem` sym_object_parents (fromJust typeKind)
                                                  = "GtkWidget"
                                      | otherwise = typeName
            
genMarshalResult knownSymbols _ _ typeName
            | isUpper (head typeName)
           && symbolIsEnum typeKind =
  (shortTypeName,
  \body -> ss "liftM (toEnum . fromIntegral) $".
           indent 1. body)
  where shortTypeName = cTypeNameToHSType typeName
        typeKind = lookupFM knownSymbols typeName

genMarshalResult knownSymbols _ _ typeName
            | isUpper (head typeName)
           && symbolIsFlags typeKind =
  ("[" ++ shortTypeName ++ "]",
  \body -> ss "liftM (toFlags . fromIntegral) $".
           indent 1. body)
  where shortTypeName = cTypeNameToHSType typeName
        typeKind = lookupFM knownSymbols typeName

genMarshalResult _ _ _ unknownType = ("{-" ++ unknownType ++ "-}", id)

-- Takes the type string and returns the Haskell Type and the GValue constructor
--
genMarshalProperty :: KnownSymbols -> String -> (String, String)
genMarshalProperty _ "gint"      = ("Int",    "GVint")
genMarshalProperty _ "guint"     = ("Int",    "GVuint")
genMarshalProperty _ "gfloat"    = ("Float",  "GVfloat")
genMarshalProperty _ "gdouble"   = ("Double", "GVdouble")
genMarshalProperty _ "gboolean"  = ("Bool",   "GVboolean")
genMarshalProperty _ "gchar*"    = ("String", "GVstring")

genMarshalProperty knownSymbols typeName
            | isUpper (head typeName)
           && symbolIsObject typeKind =
  (shortTypeName, "GVobject")
  where shortTypeName = cTypeNameToHSType typeName
        typeKind = lookupFM knownSymbols typeName

genMarshalProperty knownSymbols typeName
            | isUpper (head typeName)
           && symbolIsEnum typeKind =
  (shortTypeName, "GVenum")
  where shortTypeName = cTypeNameToHSType typeName
        typeKind = lookupFM knownSymbols typeName

genMarshalProperty knownSymbols typeName
            | isUpper (head typeName)
           && symbolIsFlags typeKind =
  (shortTypeName, "GVflags")
  where shortTypeName = cTypeNameToHSType typeName
        typeKind = lookupFM knownSymbols typeName

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
  where typeKind = lookupFM knownSymbols typeName
convertSignalType knownSymbols typeName@(_:_)
    | last typeName == '*'
   && symbolIsBoxed  typeKind  = ("BOXED",  cTypeNameToHSType (init typeName))
    | last typeName == '*'
   && symbolIsObject typeKind  = ("OBJECT", cTypeNameToHSType (init typeName))
  where typeKind = lookupFM knownSymbols (init typeName)
convertSignalType _ typeName   =  ("{-" ++ typeName ++ "-}", "{-" ++ typeName ++ "-}")

-------------------------------------------------------------------------------
-- Now for some special cases, we can override the generation of {# call #}'s
-------------------------------------------------------------------------------

-- The ordinary case:
genCallOrdinary :: String -> Bool -> String
genCallOrdinary cname unsafe@True  = "{# call unsafe " ++ cname ++ " #}"
genCallOrdinary cname unsafe@False = "{# call "        ++ cname ++ " #}"

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

genCall :: String -> Bool -> String
genCall cname safty | cname `elem` win32FileNameFunctions
                           = "#if defined (WIN32) && GTK_CHECK_VERSION(2,6,0)\n  "
                          ++ genCallOrdinary (cname ++ "_utf8") safty
                          ++ "\n  #else\n  "
                          ++ genCallOrdinary cname safty
                          ++ "\n  #endif"
genCall cname unsafe = genCallOrdinary cname unsafe
