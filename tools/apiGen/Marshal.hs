module Marshal (
  KnownTypes,
  CTypeKind(..),
  stripKnownPrefixes,
  genMarshalParameter,
  genMarshalResult,
  genMarshalProperty
  ) where

import StringUtils
import Char (isUpper)

type KnownTypes = [(String, CTypeKind)]

data CTypeKind = GObjectKind
               | GtkObjectKind
               | EnumKind
               | FlagsKind
  deriving (Eq, Show)


stripKnownPrefixes :: String -> String
stripKnownPrefixes ('A':'t':'k':remainder) = remainder
stripKnownPrefixes ('G':'t':'k':remainder) = remainder
stripKnownPrefixes ('G':'d':'k':remainder) = remainder
stripKnownPrefixes ('P':'a':'n':'g':'o':remainder) = remainder
stripKnownPrefixes ('G':'n':'o':'m':'e':remainder) = remainder
stripKnownPrefixes other = other

-------------------------------------------------------------------------------
-- Here's the interesting bit that generates the fragments of mashaling code
-------------------------------------------------------------------------------

genMarshalParameter ::
        KnownTypes ->   --a collection of types we know to be objects or enums
	String ->	--parameter name suggestion (will be unique)
	String -> 	--C type decleration for the parameter we will marshal
	(Maybe String,	--parameter class constraints (or none)
	Maybe String,	--parameter type (or none if the arg is not exposed)
	ShowS -> ShowS)	--marshaling code (\body -> ... body ...)

genMarshalParameter _ name "gboolean" =
	(Nothing, Just "Bool",
	\body -> body.
                 indent 2. ss " (fromBool ". ss name. ss ")")

genMarshalParameter _ name typeName
			 | typeName == "guint"  --these two are unsigned types
			|| typeName == "gint"
			|| typeName == "int"
			|| typeName == "gsize"  --should they be Word or Int?
			|| typeName == "gssize" =
	(Nothing, Just "Int",
	\body -> body.
                 indent 2. ss " (fromIntegral ". ss name. ss ")")

genMarshalParameter _ name "gdouble" =
	(Nothing, Just "Double",
	\body -> body.
                 indent 2. ss " (realToFrac ". ss name. ss ")")

genMarshalParameter _ name "gfloat" =
	(Nothing, Just "Float",
	\body -> body.
                 indent 2. ss " (realToFrac ". ss name. ss ")")

genMarshalParameter _ name typeName | typeName == "const-gchar*"
                                   || typeName == "const-char*" =
	(Nothing, Just "String",
	\body -> ss "withUTFString ". ss name. ss " $ \\". ss name. ss "Ptr ->".
		 indent 1. body.
                 indent 2. sc ' '. ss name. ss "Ptr")

genMarshalParameter _ name "GError**" =
	(Nothing, Nothing,
	\body -> ss "propagateGError $ \\". ss name. ss "Ptr ->".
	         indent 1. body.
                 indent 2. sc ' '. ss name. ss "Ptr")

genMarshalParameter knownTypes name typeName'
            | isUpper (head typeName')
           && last typeName' == '*'
           && last typeName /= '*'
           && (typeKind == Just GObjectKind
           ||  typeKind == Just GtkObjectKind) =
	(Just $ shortTypeName ++ "Class " ++ name, Just name,
	\body -> body.
                 indent 2. ss " (to". ss shortTypeName. sc ' '. ss name. ss ")")
  where typeName = init typeName'
        shortTypeName = stripKnownPrefixes typeName
        typeKind = shortTypeName `lookup` knownTypes

genMarshalParameter knownTypes name typeName
            | isUpper (head typeName)
           && typeKind == Just EnumKind =
	(Nothing, Just shortTypeName,
	\body -> body.
                 indent 2. ss " ((fromIntegral . fromEnum) ". ss name. ss ")")
  where shortTypeName = stripKnownPrefixes typeName
        typeKind = shortTypeName `lookup` knownTypes

genMarshalParameter knownTypes name typeName
            | isUpper (head typeName)
           && typeKind == Just FlagsKind =
	(Nothing, Just shortTypeName,
	\body -> body.
                 indent 2. ss " ((fromIntegral . fromFlags) ". ss name. ss ")")
  where shortTypeName = stripKnownPrefixes typeName
        typeKind = shortTypeName `lookup` knownTypes

genMarshalParameter _ name unknownType =
	(Nothing, Just $ "{-" ++ unknownType ++ "-}",
	\body -> body.
                 indent 2. ss " {-". ss name. ss "-}")

-- Takes the type string and returns the Haskell Type and the marshaling code
--
genMarshalResult :: KnownTypes -> String -> (String, ShowS -> ShowS)
genMarshalResult _ "gboolean" = ("IO Bool", \body -> ss "liftM toBool $". indent 1. body)
genMarshalResult _ "gint"     = ("IO Int",  \body -> ss "liftM fromIntegral $". indent 1. body)
genMarshalResult _ "guint"    = ("IO Int",  \body -> ss "liftM fromIntegral $". indent 1. body)
genMarshalResult _ "void"     = ("IO ()", id)
genMarshalResult _ "const-gchar*"  = ("IO String", \body -> body.
                                                            indent 1. ss  ">>= peekUTFString")
genMarshalResult _ "gchar*"        = ("IO String", \body -> body.
                                                            indent 1. ss  ">>= readUTFString")
genMarshalResult _ "const-GSList*" =
  ("[{- element type -}]",
  \body -> body.
           indent 1. ss ">>= readGSList".
           indent 1. ss ">>= mapM (\\elemPtr -> {-marshal elem-})")
genMarshalResult _ "GSList*" =
  ("[{- element type -}]",
  \body -> body.
           indent 1. ss ">>= fromGSList".
           indent 1. ss ">>= mapM (\\elemPtr -> {-marshal elem-})")
genMarshalResult _ "GList*" =
  ("[{- element type -}]",
  \body -> body.
           indent 1. ss ">>= fromGList".
           indent 1. ss ">>= mapM (\\elemPtr -> {-marshal elem-})")

genMarshalResult knownTypes typeName'
            | isUpper (head typeName')
           && last typeName' == '*'
           && last typeName /= '*'
           && (typeKind == Just GObjectKind
           ||  typeKind == Just GtkObjectKind) =
  ("IO " ++ shortTypeName,
  \body -> ss constructor. ss " mk". ss shortTypeName. ss " $".
           indent 1. body)
  where typeName = init typeName'
        shortTypeName = stripKnownPrefixes typeName
        typeKind = shortTypeName `lookup` knownTypes
        constructor | typeKind == Just GObjectKind = "makeNewGObject"
                    | typeKind == Just GtkObjectKind = "makeNewObject"
        
genMarshalResult knownTypes typeName
            | isUpper (head typeName)
           && typeKind == Just EnumKind =
  ("IO " ++ shortTypeName,
  \body -> ss "liftM (toEnum . fromIntegral) $".
           indent 1. body)
  where shortTypeName = stripKnownPrefixes typeName
        typeKind = shortTypeName `lookup` knownTypes

genMarshalResult knownTypes typeName
            | isUpper (head typeName)
           && typeKind == Just FlagsKind =
  ("IO " ++ shortTypeName,
  \body -> ss "liftM (toFlags . fromIntegral) $".
           indent 1. body)
  where shortTypeName = stripKnownPrefixes typeName
        typeKind = shortTypeName `lookup` knownTypes

genMarshalResult _ unknownType = ("{-" ++ unknownType ++ "-}", id)

genMarshalProperty :: KnownTypes -> String -> (String, String)
genMarshalProperty _ "gint"      = ("Int",    "GVint")
genMarshalProperty _ "guint"     = ("Int",    "GVuint")
genMarshalProperty _ "gfloat"    = ("Float",  "GVfloat")
genMarshalProperty _ "gdouble"   = ("Double", "GVdouble")
genMarshalProperty _ "gboolean"  = ("Bool",   "GVboolean")
genMarshalProperty _ "gchar*"    = ("String", "GVstring")

genMarshalProperty knownTypes typeName
            | isUpper (head typeName)
           && (typeKind == Just GObjectKind
           ||  typeKind == Just GtkObjectKind) =
  (shortTypeName, "GVobject")
  where shortTypeName = stripKnownPrefixes typeName
        typeKind = shortTypeName `lookup` knownTypes

genMarshalProperty knownTypes typeName
            | isUpper (head typeName)
           && typeKind == Just EnumKind =
  (shortTypeName, "GVenum")
  where shortTypeName = stripKnownPrefixes typeName
        typeKind = shortTypeName `lookup` knownTypes

genMarshalProperty knownTypes typeName
            | isUpper (head typeName)
           && typeKind == Just FlagsKind =
  (shortTypeName, "GVflags")
  where shortTypeName = stripKnownPrefixes typeName
        typeKind = shortTypeName `lookup` knownTypes

genMarshalProperty _ unknown = ("{-" ++ unknown ++ "-}", "{-" ++ unknown ++ "-}")
