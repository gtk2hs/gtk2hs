-- TypeGenerator.hs
-- Takes a hierarchical list of all objects in GTK+ and produces
-- Haskell class that reflect this hierarchy.
module Main (main) where

import Data.Char     (isAlpha, isAlphaNum, toLower, toUpper, isUpper)
import Data.List     (isPrefixOf)
import Control.Monad (when)
import System.Environment (getArgs)
import System.Exit   (exitWith, ExitCode(..))

-- The current object and its inheritence relationship is defined by all
-- ancestors and their column position.
type ObjectSpec = [(Int,String)]

-- This is a mapping from a type name to a) the type name in Haskell and
-- b) the info on this type 'TypeInfo'.
type TypeQuery  = (String, TypeInfo)

-- The information of on the type.
data TypeInfo = TypeInfo {
  tiQueryFunction :: String, -- the GTK blah_get_type function
  tiAlternateName :: Maybe String,
  tiNoEqualInst   :: Bool
  }

type TypeTable  = [TypeQuery]

-- A Tag is a string restricting the generation of type entries to
-- those lines that have the appropriate "if <tag>" at the end.
type Tag = String

data ParserState = ParserState {
  line		:: Int,
  col	        :: Int,
  hierObjs	:: ObjectSpec,
  onlyTags	:: [Tag]
  }

freshParserState :: [Tag] -> ParserState
freshParserState = ParserState 1 1 []

-- The parser returns a list of ObjectSpec and possibly a special type query
-- function. Each ObjectSpec describes one object with all its parents.

pFreshLine :: ParserState -> String -> [(ObjectSpec, TypeQuery)]
pFreshLine ps input = pFL ps input
  where
    pFL ps ('#':rem)		= pFL ps (dropWhile ((/=) '\n') rem)
    pFL ps ('\n':rem)	 	= pFL (ps {line = line ps+1, col=1}) rem
    pFL ps (' ':rem)		= pFL (ps {col=col ps+1}) rem
    pFL ps ('\t':rem)		= pFL (ps {col=col ps+8}) rem
    pFL ps all@('G':'t':'k':rem)= pGetObject ps all rem
    pFL ps all@('G':'d':'k':rem)= pGetObject ps all rem
    pFL ps all@('G':'n':'o':'m':'e':rem)= pGetObject ps all rem
    pFL ps []			= []
    pFL ps all			= pGetObject ps all all

pGetObject :: ParserState -> String -> String -> [(ObjectSpec, TypeQuery)]
pGetObject ps@ParserState { onlyTags=tags } txt txt' = 
  (if readTag `elem` tags then (:) (spec, specialQuery) else id) $
  pFreshLine (ps { hierObjs=spec}) (dropWhile ((/=) '\n') rem''')
  where
    isBlank     c = c==' ' || c=='\t'
    isAlphaNum_ c = isAlphaNum c || c=='_'
    isTagName c = isAlphaNum_ c || c=='-' || c=='.'  --to allow tag 'gtk-2.4'
    (origCName,rem) = span isAlphaNum txt
    (origHsName,_) = span isAlphaNum txt'
    (eqInst,rem') = 
       let r = dropWhile isBlank rem in
       if "noEq" `isPrefixOf` r then (True, drop 4 r) else (False, r)
    (name,specialQuery,rem'') = case (dropWhile isBlank rem') of
      ('a':'s':r) ->
        let (tyName,r') = span isAlphaNum_ (dropWhile isBlank r) in
          case (dropWhile isBlank r') of
	    (',':r) ->
	      let (tyQuery,r') = span isAlphaNum_ (dropWhile isBlank r) in
	        (tyName, (tyName, TypeInfo origCName (Just tyQuery) eqInst), r')
	    r -> (tyName, (tyName, TypeInfo origCName Nothing eqInst), r)
      r -> (origHsName, (origHsName, TypeInfo origCName Nothing eqInst), r)
    parents = dropWhile (\(c,_) -> c>=col ps) (hierObjs ps)
    spec = (col ps,name):parents
    (readTag, rem''') = case (dropWhile isBlank rem'') of
      ('i':'f':r) -> span isTagName (dropWhile isBlank r)
      r -> ("default",r)


-------------------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------------------

ss = showString
sc = showChar

indent :: Int -> ShowS
indent c = ss ("\n"++replicate (2*c) ' ')

-------------------------------------------------------------------------------
-- start of code generation
-------------------------------------------------------------------------------


main = do
  args <- getArgs
  when (length args<3) usage

  -----------------------------------------------------------------------------
  -- Parse command line parameters
  --
  let (hierFile: templateFile: goalFile: rem) = args
  let tags = map (drop 6) (filter ("--tag=" `isPrefixOf`)  rem)
  let lib = case map (drop 6) (filter ("--lib=" `isPrefixOf`)  rem) of
              [] -> "gtk"
	      (lib:_) -> lib
  let prefix = case map (drop 9) (filter ("--prefix=" `isPrefixOf`)  rem) of
                 [] -> "gtk"
                 (prefix:_) -> prefix
  let modName = case map (drop 10) (filter ("--modname=" `isPrefixOf`)  rem) of
  		  [] -> bareFName goalFile
		  (modName:_) -> modName
        where bareFName = reverse .
      			  takeWhile isAlphaNum .
			  drop 1 .
			  dropWhile isAlpha .
			  reverse
  let parentName = case map (drop 13) (filter ("--parentname=" `isPrefixOf`) rem) of
  		     [] -> ""
		     (parentName:_) -> parentName
  let forwardNames = map (drop 10) (filter ("--forward=" `isPrefixOf`) rem)
  let destrFun = case map (drop 13) (filter ("--destructor=" `isPrefixOf`) rem) of
                   [] -> "objectUnref"
                   (destrFun:_) -> destrFun
  -----------------------------------------------------------------------------
  -- Read in the input files
  --
  content <- if hierFile == "-"
               then getContents	      -- read stdin
	       else readFile hierFile
  template <- readFile templateFile

  -----------------------------------------------------------------------------
  -- Parse the contents of the hierarchy file
  --
  let (objs', specialQueries) = unzip $
				 pFreshLine (freshParserState tags) content
      objs = map (map snd) objs'

  -----------------------------------------------------------------------------
  -- Write the result file by substituting values into the template file
  --
  writeFile goalFile $
    templateSubstitute template (\var ->
      case var of
        "MODULE_NAME"    -> ss modName
        "MODULE_EXPORTS" -> generateExports parentName forwardNames objs
	"IMPORT_PARENT"  -> if null parentName
	                      then id
	                      else ss "{#import " .ss parentName .ss "#}"
        "FORWARD_IMPORTS"->
          foldl (.) id [ ss "import " . ss m . indent 0 | m <- forwardNames ]
        "DESTR_IMPORT"   -> if destrFun/="objectUnref" then id else
                            indent 0.ss "import System.Glib.GObject(objectUnref)"
	"CONTEXT_LIB"    -> ss lib
	"CONTEXT_PREFIX" -> ss prefix
	"DECLERATIONS"   -> generateDeclerations destrFun prefix objs specialQueries
	_ -> ss ""
    ) ""


usage = do
 putStr "\nProgram to generate Gtk's object hierarchy in Haskell. Usage:\n\
	\TypeGenerator <hierFile> <templateFile> <outFile> {--tag=<tag>}\n\
	\              {--lib=<lib>} {--prefix=<prefix>}\n\
	\              {--modname=<modName>} {--parentname=<parentName>}\n\
	\              {--forward=<fwdName>} {--destructor=<destrName>}\n\
	\where\n\
	\  <hierFile>      a list of all possible objects, the hierarchy is\n\
	\                  taken from the indentation\n\
	\  <templateFile>  is the name and path of the output template file\n\
	\  <outFile>       is the name and path of the output file\n\
	\  <tag>           generate entries that have the tag <tag>\n\
	\                  specify `default' for types without tags\n\
	\  <lib>           set the lib to use in the c2hs {#context #}\n\
	\                  declaration (the default is \"gtk\")\n\
	\  <prefix>        set the prefix to use in the c2hs {#context #}\n\
	\                  declaration (the default is \"gtk\")\n\
	\  <modName>       specify module name if it does not match the\n\
	\                  file name, eg a hierarchical module name\n\
	\  <parentName>    specify the name of the module that defines any\n\
	\                  parent classes eg Hierarchy (default is none)\n\
	\  <fwdName>       specify a number of modules that are imported\n\
	\                  as well as exported from the generated module\n\
	\  <destrName>     specify a non-standard C function pointer that\n\
	\                  is called to destroy the objects\n"
 exitWith $ ExitFailure 1



-------------------------------------------------------------------------------
-- generate dynamic fragments
-------------------------------------------------------------------------------

generateExports :: String -> [String] -> [[String]] -> ShowS
generateExports parent forwardNames objs =
  (if null parent
     then ss ""
     else ss "  module " .ss parent. ss ",").
  drop 1.
  foldl (\s1 s2 -> s1.ss ",".indent 1.ss "module ".s2) id
    (map ss forwardNames).
  foldl (\s1 s2 -> s1.ss ",".s2) id
    [ indent 1.ss n.ss "(".ss n.ss "), ".ss n.ss "Class,".
      indent 1.ss "to".ss n.ss ", ".
      indent 1.ss "mk".ss n.ss ", un".ss n.sc ','.
      indent 1.ss "castTo".ss n.ss ", gType".ss n
    | (n:_) <- objs
    , n /= "GObject" ]

generateDeclerations :: String -> String -> [[String]] -> TypeTable -> ShowS
generateDeclerations destr prefix objs typeTable =
  foldl (.) id
  [ makeClass destr prefix typeTable obj
  . makeUpcast obj
  . makeGType typeTable obj
  | obj <- objs ]

makeUpcast :: [String] -> ShowS
makeUpcast [obj]	   = id -- no casting for GObject
makeUpcast (obj:_:_) = 
  indent 0.ss "castTo".ss obj.ss " :: GObjectClass obj => obj -> ".ss obj.
  indent 0.ss "castTo".ss obj.ss " = castTo gType".ss obj.ss " \"".ss obj.ss "\"".
  indent 0

makeGType :: TypeTable -> [String] -> ShowS
makeGType table [obj] = id -- no GType for GObject
makeGType table (obj:_:_) = 
  indent 0.ss "gType".ss obj.ss " :: GType".
  indent 0.ss "gType".ss obj.ss " =".
  indent 1.ss "{# call fun unsafe ".
    ss (case lookup obj table of 
         (Just TypeInfo { tiAlternateName = Just get_type_func }) ->
           get_type_func
	 (Just TypeInfo { tiQueryFunction = cname}) ->
	   tail $ c2u True cname++"_get_type").
    ss " #}".
  indent 0
  where
    -- case to underscore translation: the boolean arg specifies whether
    -- the first uppercase letter X is to be replaced by _x (True) or by x.
    --
    -- translation:	HButtonBox -> hbutton_box
    c2u :: Bool -> String -> String
    c2u True  (x:xs) | isUpper x = '_':toLower x:c2u False xs
    c2u False (x:xs) | isUpper x = toLower x:c2u True xs
    c2u _     (x:xs) | otherwise = x:c2u True xs
    c2u _     []		 = []

makeOrd fill []          = id
makeOrd fill (obj:preds) = indent 1.ss "compare ".ss obj.ss "Tag ".
			   fill obj.ss obj.ss "Tag".fill obj.
			   ss " = EQ".makeGT obj preds
  where
    makeGT obj []	= id
    makeGT obj (pr:eds) = indent 1.ss "compare ".ss obj.ss "Tag ".
			  fill obj.ss pr.ss "Tag".fill pr.
			  ss " = GT".makeGT obj eds

makeClass :: String -> String -> TypeTable -> [String] -> ShowS
makeClass destr prefix table (name:[])      = id
makeClass destr prefix table (name:parents) =
  indent 0.ss "-- ".ss (replicate (75-length name) '*').sc ' '.ss name.
  indent 0.
  indent 0.ss "{#pointer *".
  (case lookup name table of
        (Just TypeInfo { tiQueryFunction = cname })
          | stripPrefix cname == name -> ss name
          | otherwise                 -> ss cname.ss " as ".ss name
	  where stripPrefix s = if uCasePrefix `isPrefixOf` s
		                  then drop (length prefix) s
				  else s
                uCasePrefix = toUpper (head prefix) : tail prefix -- gtk -> Gtk
	).
  ss " foreign newtype #}".
  (case lookup name table of
     (Just (TypeInfo { tiNoEqualInst = False })) -> ss " deriving (Eq,Ord)"
     _ -> id
     ).
  indent 0.
  indent 0.ss "mk".ss name.ss " = (".ss name.ss ", ".ss destr.ss ")".
  indent 0.ss "un".ss name.ss " (".ss name.ss " o) = o".
  indent 0.
  indent 0.ss "class ".ss (head parents).ss "Class o => ".ss name.ss "Class o".
  indent 0.ss "to".ss name.ss " :: ".ss name.ss "Class o => o -> ".ss name.
  indent 0.ss "to".ss name.ss " = unsafeCastGObject . toGObject".
  indent 0.
  makeInstance name (name:init parents).
  makeGObjectInstance name.
  indent 0

makeInstance :: String -> [String] -> ShowS
makeInstance name [] = id
makeInstance name (par:ents) =
  indent 0.ss "instance ".ss par.ss "Class ".ss name.
  makeInstance name ents

makeGObjectInstance :: String -> ShowS
makeGObjectInstance name =
  indent 0.ss "instance GObjectClass ".ss name.ss " where".
  indent 1.ss "toGObject = GObject . castForeignPtr . un".ss name.
  indent 1.ss "unsafeCastGObject = ".ss name.ss" . castForeignPtr . unGObject"

templateSubstitute :: String -> (String -> ShowS) -> ShowS
templateSubstitute template varSubst = doSubst template 
  where doSubst [] = id
        doSubst ('\\':'@':cs) = sc '@' . doSubst cs
        doSubst ('@':cs) = let (var,_:cs') = span ('@'/=) cs
                            in varSubst var . doSubst cs'
        doSubst (c:cs) = sc c . doSubst cs
