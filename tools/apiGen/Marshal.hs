module Marshal (
  KnownSymbols,
  CSymbol(..),
  ObjectKind(..),
  EnumKind(..),
  stripKnownPrefixes,
  knownMiscType,
  maybeNullParameter,
  maybeNullResult,
  genMarshalParameter,
  genMarshalResult,
  genMarshalProperty,
  convertSignalType,
  genCall
  ) where

import StringUtils
import Char (isUpper)
import Data.FiniteMap

type KnownSymbols = FiniteMap String CSymbol

data CSymbol = SymObjectType ObjectKind
             | SymEnumType   EnumKind
             | SymEnumValue
             | SymStructType
             | SymBoxedType
             | SymClassType
             | SymTypeAlias
             | SymCallbackType
  deriving Eq

data ObjectKind = GObjectKind | GtkObjectKind
  deriving Eq
data EnumKind = EnumKind | FlagsKind
  deriving Eq

symbolIsObject (Just (SymObjectType _)) = True
symbolIsObject _                        = False

symbolIsGObject (Just (SymObjectType GObjectKind)) = True
symbolIsGObject _                                  = False

symbolIsGtkObject (Just (SymObjectType GtkObjectKind)) = True
symbolIsGtkObject _                                    = False

symbolIsEnum (Just (SymEnumType EnumKind)) = True
symbolIsEnum _                             = False

symbolIsFlags (Just (SymEnumType FlagsKind)) = True
symbolIsFlags _                              = False

symbolIsBoxed (Just SymBoxedType) = True
symbolIsBoxed _                   = False

stripKnownPrefixes :: String -> String
stripKnownPrefixes ('A':'t':'k':remainder) = remainder
stripKnownPrefixes ('G':'t':'k':remainder) = remainder
stripKnownPrefixes ('G':'d':'k':remainder) = remainder
stripKnownPrefixes ('P':'a':'n':'g':'o':remainder) = remainder
stripKnownPrefixes ('G':'n':'o':'m':'e':remainder) = remainder
stripKnownPrefixes other = other

-- These are ones we have bound and so we can make documentation references to
-- them. Otherwise we generate FIXME messages in the docs.
knownMiscType :: String -> Bool
knownMiscType "GtkTreePath" = True
knownMiscType "GtkTreeIter" = True
knownMiscType "GdkColor"    = True
knownMiscType "GtkTextIter" = True
knownMiscType "GtkIconSet"  = True
knownMiscType _ = False

-- These are classes from which no other class derives or is ever likely to
-- derive. In this case we can use the actual type rather than the type class
-- For example: GtkAdjustment we say
-- > Adjustment -> ...
-- rather than
-- > AdjustmentClass adjustment => adjustment -> ...
leafClass :: String -> Bool
leafClass "GtkAdjustment" = True
leafClass "GdkPixbuf"     = True
leafClass _ = False

-- This is a table of fixup information. It lists function parameters that
-- can be null and so should therefore be converted to use a Maybe type.
-- The perameters are identifed by C function name and parameter name.
--
-- Note that if you set this to True for any parameter then the docs for this
-- function will use @Nothing@ in place of NULL rather than a FIXME message. So
-- make sure all possibly-null parameters have been fixed since all NULLs in
-- the function docs are suppressed (since there is no automatic way of working
-- out which function doc NULLs correspond to which parameters).
--
maybeNullParameter :: String -> String -> Bool
maybeNullParameter "gtk_entry_completion_set_model" "model" = True
maybeNullParameter "gtk_label_new" "str" = True
maybeNullParameter _ _ = False

-- similarly for method return values/types.
maybeNullResult :: String -> Bool
maybeNullResult "gtk_entry_completion_get_entry" = True
maybeNullResult "gtk_entry_completion_get_model" = True
maybeNullResult "gtk_accel_label_get_accel_widget" = True
maybeNullResult "gtk_progress_bar_get_text" = True
maybeNullResult "gtk_bin_get_child" = True
maybeNullResult _ = False

-------------------------------------------------------------------------------
-- Here's the interesting bit that generates the fragments of mashaling code
-------------------------------------------------------------------------------

genMarshalParameter ::
        KnownSymbols -> --a collection of types we know to be objects or enums
        String ->       --function name (useful to lookup per-func fixup info)
	String ->	--parameter name suggestion (will be unique)
	String -> 	--C type decleration for the parameter we will marshal
	(Maybe String,	--parameter class constraints (or none)
	Maybe String,	--parameter type (or none if the arg is not exposed)
	ShowS -> ShowS)	--marshaling code (\body -> ... body ...)

genMarshalParameter _ _ name "gboolean" =
	(Nothing, Just "Bool",
	\body -> body.
                 indent 2. ss "(fromBool ". ss name. ss ")")

genMarshalParameter _ _ name typeName
			 | typeName == "guint"  --these two are unsigned types
			|| typeName == "gint"
			|| typeName == "int"
			|| typeName == "gsize"  --should they be Word or Int?
			|| typeName == "gssize" =
	(Nothing, Just "Int",
	\body -> body.
                 indent 2. ss "(fromIntegral ". ss name. ss ")")

genMarshalParameter _ _ name "gdouble" =
	(Nothing, Just "Double",
	\body -> body.
                 indent 2. ss "(realToFrac ". ss name. ss ")")

genMarshalParameter _ _ name "gfloat" =
	(Nothing, Just "Float",
	\body -> body.
                 indent 2. ss "(realToFrac ". ss name. ss ")")

genMarshalParameter _ funcName name typeName | typeName == "const-gchar*"
                                            || typeName == "const-char*" =
  if maybeNullParameter funcName name
    then (Nothing, Just "Maybe String",
	 \body -> ss "maybeWith withUTFString ". ss name. ss " $ \\". ss name. ss "Ptr ->".
		  indent 1. body.
                  indent 2. ss name. ss "Ptr")
    else (Nothing, Just "String",
	 \body -> ss "withUTFString ". ss name. ss " $ \\". ss name. ss "Ptr ->".
		  indent 1. body.
                  indent 2. ss name. ss "Ptr")

genMarshalParameter _ _ name "GError**" =
	(Nothing, Nothing,
	\body -> ss "propagateGError $ \\". ss name. ss "Ptr ->".
	         indent 1. body.
                 indent 2. ss name. ss "Ptr")

-- Objects -----------------------------
genMarshalParameter knownSymbols funcName name typeName'
            | isUpper (head typeName')
           && last typeName' == '*'
           && last typeName /= '*'
           && symbolIsObject typeKind =
	if leafClass typeName
          then (Nothing, Just shortTypeName,
               \body -> body.
                        indent 2. ss name)
        else if maybeNullParameter funcName name
          then (Just $ shortTypeName ++ "Class " ++ name, Just ("Maybe " ++ name),
               \body -> body.
                        indent 2. ss "(maybe (". ss shortTypeName. ss " nullForeignPtr) to".
                                                  ss shortTypeName. sc ' '. ss name. ss ")")
          else (Just $ shortTypeName ++ "Class " ++ name, Just name,
               \body -> body.
                        indent 2. ss "(to". ss shortTypeName. sc ' '. ss name. ss ")")
  where typeName = init typeName'
        shortTypeName = stripKnownPrefixes typeName
        typeKind = lookupFM knownSymbols typeName

-- Enums -------------------------------
genMarshalParameter knownSymbols _ name typeName
            | isUpper (head typeName)
           && symbolIsEnum typeKind =
	(Nothing, Just shortTypeName,
	\body -> body.
                 indent 2. ss "((fromIntegral . fromEnum) ". ss name. ss ")")
  where shortTypeName = stripKnownPrefixes typeName
        typeKind = lookupFM knownSymbols typeName

-- Flags -------------------------------
genMarshalParameter knownSymbols _ name typeName
            | isUpper (head typeName)
           && symbolIsFlags typeKind =
	(Nothing, Just shortTypeName,
	\body -> body.
                 indent 2. ss "((fromIntegral . fromFlags) ". ss name. ss ")")
  where shortTypeName = stripKnownPrefixes typeName
        typeKind = lookupFM knownSymbols typeName

genMarshalParameter _ _ name textIter | textIter == "const-GtkTextIter*"
                                   || textIter == "GtkTextIter*" =
	(Nothing, Just "TextIter",
	\body -> body.
                 indent 2. ss name)

genMarshalParameter _ _ name "GtkTreeIter*" =
	(Nothing, Just "TreeIter",
	\body -> body.
                 indent 2. ss name)

genMarshalParameter _ _ name "GtkTreePath*" =
	(Nothing, Just "TreePath",
	\body -> ss "withTreePath ". ss name. ss " $ \\". ss name. ss " ->".
		 indent 1. body.
                 indent 2. ss name)

genMarshalParameter _ _ name unknownType =
	(Nothing, Just $ "{-" ++ unknownType ++ "-}",
	\body -> body.
                 indent 2. ss "{-". ss name. ss "-}")

-- Takes the type string and returns the Haskell Type and the marshaling code
--
genMarshalResult :: 
        KnownSymbols -> --a collection of types we know to be objects or enums
        String ->       --function name (useful to lookup per-func fixup info)
	String -> 	--C type decleration for the return value we will marshal
	(String,	--Haskell return type 
	ShowS -> ShowS)	--marshaling code (\body -> ... body ...)
genMarshalResult _ _ "gboolean" = ("IO Bool", \body -> ss "liftM toBool $". indent 1. body)
genMarshalResult _ _ "gint"     = ("IO Int",  \body -> ss "liftM fromIntegral $". indent 1. body)
genMarshalResult _ _ "guint"    = ("IO Int",  \body -> ss "liftM fromIntegral $". indent 1. body)
genMarshalResult _ _ "gdouble"  = ("IO Double", \body -> ss "liftM realToFrac $". indent 1. body)
genMarshalResult _ _ "gfloat"   = ("IO Float",  \body -> ss "liftM realToFrac $". indent 1. body)
genMarshalResult _ _ "void"     = ("IO ()", id)
genMarshalResult _ funcName "const-gchar*" =
  if maybeNullResult funcName
    then ("IO (Maybe String)",
         \body -> body.
                  indent 1. ss  ">>= maybePeek peekUTFString")
    else ("IO String",
         \body -> body.
                  indent 1. ss  ">>= peekUTFString")
genMarshalResult _ funcName "gchar*" =
  if maybeNullResult funcName
    then ("IO (Maybe String)",
         \body -> body.
                  indent 1. ss  ">>= maybePeek readUTFString")
    else ("IO String",
         \body -> body.
                  indent 1. ss  ">>= readUTFString")
genMarshalResult _ _ "const-GSList*" =
  ("[{- element type -}]",
  \body -> body.
           indent 1. ss ">>= readGSList".
           indent 1. ss ">>= mapM (\\elemPtr -> {-marshal elem-})")
genMarshalResult _ _ "GSList*" =
  ("[{- element type -}]",
  \body -> body.
           indent 1. ss ">>= fromGSList".
           indent 1. ss ">>= mapM (\\elemPtr -> {-marshal elem-})")
genMarshalResult _ _ "GList*" =
  ("[{- element type -}]",
  \body -> body.
           indent 1. ss ">>= fromGList".
           indent 1. ss ">>= mapM (\\elemPtr -> {-marshal elem-})")

genMarshalResult knownSymbols funcName typeName'
            | isUpper (head typeName')
           && last typeName' == '*'
           && last typeName /= '*'
           && symbolIsObject typeKind =
  if maybeNullResult funcName
    then ("IO (Maybe " ++ shortTypeName ++ ")",
         \body -> ss "maybeNull (" .ss constructor. ss " mk". ss shortTypeName. ss ") $".
                  indent 1. body)
    else ("IO " ++ shortTypeName,
         \body -> ss constructor. ss " mk". ss shortTypeName. ss " $".
                  indent 1. body)
  where typeName = init typeName'
        shortTypeName = stripKnownPrefixes typeName
        typeKind = lookupFM knownSymbols typeName
        constructor | symbolIsGObject typeKind = "makeNewGObject"
                    | symbolIsGtkObject typeKind = "makeNewObject"
        
genMarshalResult knownSymbols _ typeName
            | isUpper (head typeName)
           && symbolIsEnum typeKind =
  ("IO " ++ shortTypeName,
  \body -> ss "liftM (toEnum . fromIntegral) $".
           indent 1. body)
  where shortTypeName = stripKnownPrefixes typeName
        typeKind = lookupFM knownSymbols typeName

genMarshalResult knownSymbols _ typeName
            | isUpper (head typeName)
           && symbolIsFlags typeKind =
  ("IO " ++ shortTypeName,
  \body -> ss "liftM (toFlags . fromIntegral) $".
           indent 1. body)
  where shortTypeName = stripKnownPrefixes typeName
        typeKind = lookupFM knownSymbols typeName

genMarshalResult _ _ unknownType = ("{-" ++ unknownType ++ "-}", id)

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
  where shortTypeName = stripKnownPrefixes typeName
        typeKind = lookupFM knownSymbols typeName

genMarshalProperty knownSymbols typeName
            | isUpper (head typeName)
           && symbolIsEnum typeKind =
  (shortTypeName, "GVenum")
  where shortTypeName = stripKnownPrefixes typeName
        typeKind = lookupFM knownSymbols typeName

genMarshalProperty knownSymbols typeName
            | isUpper (head typeName)
           && symbolIsFlags typeKind =
  (shortTypeName, "GVflags")
  where shortTypeName = stripKnownPrefixes typeName
        typeKind = lookupFM knownSymbols typeName

genMarshalProperty _ unknown = ("{-" ++ unknown ++ "-}", "{-" ++ unknown ++ "-}")

-- Takes the type string and returns the signal marshaing category
--
convertSignalType :: KnownSymbols -> String -> String
convertSignalType _ "void"     = "NONE"
convertSignalType _ "gchar"    = "CHAR"
convertSignalType _ "guchar"   = "UCHAR"
convertSignalType _ "gboolean" = "BOOLEAN"
convertSignalType _ "gint"     = "INT"
convertSignalType _ "guint"    = "UINT"
convertSignalType _ "glong"    = "LONG"
convertSignalType _ "gulong"   = "ULONG"
convertSignalType _ "gfloat"   = "FLOAT"
convertSignalType _ "gdouble"  = "DOUBLE"
convertSignalType _ "gchar*"   = "STRING"
convertSignalType _ "const-gchar*" = "STRING"
convertSignalType knownSymbols typeName
  | symbolIsEnum   typeKind    = "ENUM"
  | symbolIsFlags  typeKind    = "FLAGS"
  where typeKind = lookupFM knownSymbols typeName
convertSignalType knownSymbols typeName@(_:_)
    | last typeName == '*'
   && symbolIsBoxed  typeKind  = "BOXED"
    | last typeName == '*'
   && symbolIsObject typeKind  = "OBJECT"
  where typeKind = lookupFM knownSymbols (init typeName)
convertSignalType _ typeName   =  "{-" ++ typeName ++ "-}"

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

win32FileNameFunctions =
  ["gtk_image_new_from_file"
  ,"gdk_pixbuf_new_from_file"
  ,"gtk_icon_source_get_filename"
  ,"gtk_icon_source_set_filename"
  ,"gtk_file_chooser_get_filename"
  ,"gtk_file_chooser_set_filename"
  ,"gtk_file_chooser_select_filename"
  ,"gtk_file_chooser_unselect_filename"
  ,"gtk_file_chooser_get_filenames"
  ,"gtk_file_chooser_set_current_folder"
  ,"gtk_file_chooser_get_current_folder"
  ,"gtk_file_chooser_get_preview_filename"
  ,"gtk_file_chooser_add_shortcut_folder"
  ,"gtk_file_chooser_remove_shortcut_folder"
  ,"gtk_file_chooser_list_shortcut_folders"
  ,"gtk_file_selection_set_filename"
  ,"gtk_file_selection_get_filename"]

genCall :: String -> Bool -> String
genCall cname safty | cname `elem` win32FileNameFunctions
                           = "#if defined (WIN32) && GTK_CHECK_VERSION(2,6,0)\n  "
                          ++ genCallOrdinary (cname ++ "_utf8") safty
                          ++ "\n  #else\n  "
                          ++ genCallOrdinary cname safty
                          ++ "\n  #endif"
genCall cname unsafe = genCallOrdinary cname unsafe
