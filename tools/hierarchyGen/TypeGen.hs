-- TypeGenerator.hs
-- Takes a hierarchical list of all objects in GTK+ and produces
-- Haskell class that reflect this hierarchy.
module Main(main) where

import Char(showLitChar, isAlpha, isAlphaNum, isSpace, toLower, isUpper)
import List(nub, isPrefixOf)
import Maybe(catMaybes, fromMaybe)
import Monad(when)
import System(getArgs, exitWith, ExitCode(..))

-- The current object and its inheritence relationship is defined by all
-- ancestors and their column position.
type ObjectSpec = [(Int,String)]

-- This is a mapping from a type name to a) the type name in Haskell and
-- b) the GTK blah_get_type function.
type TypeQuery  = Maybe (String, (String, String))

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
    pFL ps ('G':'t':'k':rem) 	= pGetObject ps rem
    pFL ps []			= []
    pFL ps all			= pGetObject ps all

pGetObject :: ParserState -> String -> [(ObjectSpec, TypeQuery)]
pGetObject ps@ParserState { onlyTags=tags } txt = 
  (if readTag `elem` tags then (:) (spec, specialQuery) else id) $
  pFreshLine (ps { hierObjs=spec}) (dropWhile ((/=) '\n') rem'')
  where
    isBlank     c = c==' ' || c=='\t'
    isAlphaNum_ c = isAlphaNum c || c=='_'
    (origName,rem) = span isAlphaNum txt
    (name,specialQuery,rem') = case (dropWhile isBlank rem) of
      ('a':'s':r) ->
        let (tyName,r') = span isAlphaNum_ (dropWhile isBlank r) in
          case (dropWhile isBlank r') of
	    (',':r) ->
	      let (tyQuery,r') = span isAlphaNum_ (dropWhile isBlank r) in
	        (tyName, Just (tyName, (origName, tyQuery)), r')
	    r -> error ("line "++show (line ps)++
		        ": Expected a comma, found:"++take 5 r)
      r -> (origName, Nothing, r)
    parents = dropWhile (\(c,_) -> c>=col ps) (hierObjs ps)
    spec = (col ps,name):parents
    (readTag, rem'') = case (dropWhile isBlank rem') of
      ('i':'f':r) -> span isAlphaNum_ (dropWhile isBlank r)
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
  when (length args<2) usage
  let (hierFile: goalFile: rem) = args
  let tags = map (drop 6) (filter ("--tag=" `isPrefixOf`)  rem)
  let lib = case map (drop 6) (filter ("--lib=" `isPrefixOf`)  rem) of
              [] -> "gtk"
	      (lib:_) -> lib
  let prefix = case map (drop 9) (filter ("--prefix=" `isPrefixOf`)  rem) of
                 [] -> "gtk"
                 (prefix:_) -> prefix
  content <- readFile hierFile
  let (objs, specialQueries) = unzip $
				 pFreshLine (freshParserState tags) content
  let bareFName = reverse . 
		  takeWhile isAlphaNum .
		  drop 1 .
		  dropWhile isAlpha .
		  reverse		     
  writeFile goalFile $
    generate (bareFName goalFile) lib prefix
      (map (map snd) objs) (catMaybes specialQueries) ""


usage = do
 putStr "\nProgram to generate Gtk's object hierarchy in Haskell. Usage:\n\
	\TypeGenerator <hierFile> <outFile> {--tag=<tag>}\n\
	\              {--lib=<lib>} {--prefix=<prefix>}\n\
	\where\n\
	\  <hierFile>	   a list of all possible objects, the hierarchy is\n\
	\		   taken from the indentation\n\
	\  <outFile>	   is the name and path of the output file\n\
	\  <tag>           generate entries that have the tag <tag>\n\
	\		   specify `default' for types without tags\n\
	\  <lib>           set the lib to use in the c2hs {#context #}\n\
	\                  declaration (the default is \"gtk\")\n\
	\  <prefix>        set the prefix to use in the c2hs {#context #}\n\
	\                  declaration (the default is \"gtk\")\n"
 exitWith $ ExitFailure 1



-------------------------------------------------------------------------------
-- generate dynamic fragments
-------------------------------------------------------------------------------

generate :: String -> String -> String -> [[String]] -> [(String, (String, String))] -> ShowS
generate fname lib prefix objs typeTable = 
  let fillCol str = ss $ replicate 
		    (maximum (map (length.head) objs)-length str) ' ' 
  in
           ss "-- -*-haskell-*-".
  indent 0.ss "-- ******************** automatically generated file - do not edit **********".
  indent 0.ss "--  Object hierarchy for the GIMP Toolkit (GTK) Binding for Haskell".
  indent 0.ss "--".
  indent 0.ss "--  Author : Axel Simon".
  indent 0.ss "--".
  indent 0.ss "--  Copyright (c) 2001-2003 Axel Simon".
  indent 0.ss "--".
  indent 0.ss "--  This file is free software; you can redistribute it and/or modify".
  indent 0.ss "--  it under the terms of the GNU General Public License as published by".
  indent 0.ss "--  the Free Software Foundation; either version 2 of the License, or".
  indent 0.ss "--  (at your option) any later version.".
  indent 0.ss "--".
  indent 0.ss "--  This file is distributed in the hope that it will be useful,".
  indent 0.ss "--  but WITHOUT ANY WARRANTY; without even the implied warranty of".
  indent 0.ss "--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the".
  indent 0.ss "--  GNU General Public License for more details.".
  indent 0.ss "--".
  indent 0.ss "--- @description@ -------------------------------------------------------------".
  indent 0.ss "--".
  indent 0.ss "--  * This file reflects the Gtk object hierarchy in terms of Haskell classes.".
  indent 0.ss "--".
  indent 0.ss "--- @documentation@ -----------------------------------------------------------".
  indent 0.ss "--".
  indent 0.ss "--".
  indent 0.ss "--- @todo@ --------------------------------------------------------------------".
  indent 0.ss "--".
  indent 0.ss "--".
  indent 0.ss "module ".ss fname.sc '('.
--  indent 1.ss "ObjectTag(..)".
  foldl (\s1 s2 -> s1.ss ", ".s2) id (map (\(n:_) -> 
		indent 1.ss n.ss "(".ss n.ss "), ".ss n.ss "Class,".
		indent 1.ss "to".ss n.ss ", ".
		indent 1.ss "from".ss n.ss ", ".
		indent 1.ss "mk".ss n.ss ", un".ss n.sc ','.
		indent 1.ss "castTo".ss n) objs).
  indent 1.ss ") where".
  indent 0.
  indent 0.ss "import FFI	(ForeignPtr, castForeignPtr, foreignPtrToPtr,".  ss " CULong)".
  indent 0.ss "import GType    (typeInstanceIsA)".
  -- this is a very bad hack to get the definition of the ancestors whenever
  -- these are not created in this file
  (if fname/="Hierarchy" then indent 0.ss "{#import Hierarchy#}" else id).
  indent 0.
  indent 0.ss "{#context lib=\"".ss lib.ss "\" prefix=\"".ss prefix.ss "\" #}".
  indent 0.
  indent 0.ss "castToGObject = id".
  indent 0.
  indent 0.ss "-- The usage of foreignPtrToPtr should be safe as the evaluation will only be".
  indent 0.ss "-- forced if the object is used afterwards".
  indent 0.
  foldl (.) id (map (makeUpcast typeTable) objs).
  indent 0.
--  indent 0.ss "data ObjectTag  ".makeTypeTags '=' (map head objs).
--  indent 0.
--  indent 0.ss "instance Ord ObjectTag where".
--  foldl (.) id (map (makeOrd fillCol) objs).
--  indent 1.ss "compare ".ss "_   ".fillCol "_".ss " _   ".fillCol "_".
--    ss " = LT".
--  indent 0.
  indent 0.
  foldl (.) id (map (makeClass typeTable) objs)

makeTypeTags :: Char -> [String] -> ShowS
makeTypeTags c [] = ss "deriving Eq"
makeTypeTags c (obj:ects) = sc c.sc ' '.ss obj.ss "Tag".indent 8.
			    makeTypeTags '|' ects

makeUpcast table [obj]	   = id -- no casting for GObject
makeUpcast table (obj:_:_) = 
  indent 0.ss "castTo".ss obj.ss " :: GObjectClass obj => obj -> ".ss obj.
  indent 0.ss "castTo".ss obj.ss " obj =".
  indent 1.ss "if typeInstanceIsA ((foreignPtrToPtr.castForeignPtr.unGObject.toGObject) obj)".
  indent 2.ss "{#call fun unsafe ".
    ss (maybe ("gtk"++c2u True obj++"_get_type") snd (lookup obj table)).
    ss "#} then".
  indent 3.ss "(fromGObject.toGObject) obj else".
  indent 4.ss "error \"Cannot cast object to ".ss obj.ss ".\"".
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

makeClass :: [(String,(String, String))] -> [String] -> ShowS
makeClass table (name:parents) =
  indent 0.ss "-- ".ss (replicate (75-length name) '*').sc ' '.ss name.
  indent 0.
  indent 0.ss "{#pointer *".
  maybe (ss name) (\s -> ss (fst s).ss " as ".ss name) (lookup name table).
  ss " foreign newtype #}".
  indent 0.
  indent 0.ss "mk".ss name.ss " = ".ss name.
  indent 0.ss "un".ss name.ss " (".ss name.ss " o) = o".
  indent 0.
  (if null parents
  then
    indent 0.ss "class ".ss name.ss "Class o where".
    indent 1.ss "to".ss name.ss "   :: o -> ".ss name.
    indent 1.ss "from".ss name.ss " :: ".ss name.ss " -> o".
    indent 0.
    indent 0.ss "instance ".ss name.ss "Class ".ss name.ss " where".
    indent 1.ss "to".ss name.ss "   = id".
    indent 1.ss "from".ss name.ss " = id"
  else
    indent 0.ss "class ".ss (head parents).ss "Class o => ".ss name.ss "Class o".
    indent 0.ss "to".ss name.ss "   :: ".ss name.ss "Class o => o -> ".ss name.
    indent 0.ss "to".ss name.ss "   = from".ss (last parents).ss " . to".ss (last parents).
    indent 0.ss "from".ss name.ss " :: ".ss name.ss "Class o => ".ss name.ss " -> o".
    indent 0.ss "from".ss name.ss " = from".ss (last parents).ss " . to".ss (last parents).
    indent 0.
    makeInstance name (name:parents)).
  indent 0

makeInstance :: String -> [String] -> ShowS
makeInstance name [par] =
  indent 0.
  indent 0.ss "instance ".ss par.ss "Class ".ss name.ss " where".
  indent 1.ss "to".ss par.ss "   = mk".ss par.ss ".castForeignPtr.un".ss name.
  indent 1.ss "from".ss par.ss " = mk".ss name.ss ".castForeignPtr.un".ss par.
  indent 0
makeInstance name (par:ents) =
  indent 0.ss "instance ".ss par.ss "Class ".ss name.
  makeInstance name ents

 
  
