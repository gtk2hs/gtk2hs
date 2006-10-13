-- ApiGen: takes an xml description of a GObject-style API and produces a .chs
-- binding module. Optionally it can be supplied with an xml documentation file
-- in which case the .chs file will contain haddock-format documentation too.

-- If you want to teach ApiGen how to marshal new types, the function you want
-- to modify is either genMarshalParameter or genMarshalResult near the end of
-- this file.

module FormatDocs (
  genModuleDocumentation,
  haddocFormatDeclaration,
  cFuncNameToHsName,
  cParamNameToHsName,
  toStudlyCaps,
  haddocFormatParas,
  haddocFormatSpans,
  haddocFormatSpan,
  mungeWord,
  changeIllegalNames,
  addVersionParagraphs
) where

import Api (NameSpace(..), Object(..), Method(..))
import Docs
import Marshal (KnownSymbols, CSymbol(..))
import MarshalFixup (cTypeNameToHSType, knownMiscType, fixCFunctionName
                    ,fixModuleAvailableSince)
import StringUtils

import Maybe (isJust)
import Char (toLower, isUpper, isAlpha, isSpace)
import Data.Tree
import qualified List (lines)
import qualified Data.Map as Map

-------------------------------------------------------------------------------
-- Functions for formatting haddock documentation
-------------------------------------------------------------------------------

genModuleDocumentation :: KnownSymbols -> ModuleDoc -> ShowS
genModuleDocumentation knownSymbols moduledoc =
  (if null (moduledoc_description moduledoc)
     then id
     else comment.ss "* Detail".nl.
          comment.nl.
          comment.ss "| ".haddocFormatParas knownSymbols False (moduledoc_description moduledoc).nl).
  (if null (moduledoc_sections moduledoc)
     then id
     else nl.comment.haddocFormatSections knownSymbols (moduledoc_sections moduledoc).nl.comment.nl).
  (if null (moduledoc_hierarchy moduledoc)
     then id
     else nl.comment.ss "* Class Hierarchy".nl.
          comment.ss "|".nl.
          comment.ss "@".nl.
          comment.ss "|  ".haddocFormatHierarchy knownSymbols
	                     (moduledoc_name moduledoc) (moduledoc_altname moduledoc)
	                     (moduledoc_hierarchy moduledoc).nl.
          comment.ss "@".nl)

haddocFormatDeclaration :: KnownSymbols -> Bool -> (doc -> [DocPara]) -> Maybe doc -> ShowS
haddocFormatDeclaration knownSymbols handleNULLs doc_paragraphs Nothing = ss "-- | \n--\n"
haddocFormatDeclaration knownSymbols handleNULLs doc_paragraphs (Just doc)
  = ss "-- | ". haddocFormatParas knownSymbols handleNULLs (doc_paragraphs doc). nl.
    ss "--\n"

haddocFormatHierarchy :: KnownSymbols -> String -> String -> Forest String -> ShowS
haddocFormatHierarchy knownSymbols moduledoc_name1 moduledoc_name2 =
    sepBy "\n-- |  "
  . concatMap drawHierarchy
  . map (fmap (haddocFormatSpan knownSymbols False))
  . map (fmap (\s -> if s == moduledoc_name1 || s == moduledoc_name2
                       then DocText (cTypeNameToHSType s)
		       else DocTypeXRef s))
  . filterForest (/="GInitiallyUnowned")

drawHierarchy :: Tree String -> [String]
drawHierarchy (Node x ts0) = x : drawSubTrees ts0
  where drawSubTrees [] = []
        drawSubTrees (t:ts) =
          shift " +----" "      " (drawHierarchy t) ++ drawSubTrees ts
        shift first other = zipWith (++) (first : repeat other)

filterForest :: (a -> Bool) -> Forest a -> Forest a
filterForest p = concatMap (filterTree p)

filterTree :: (a -> Bool) -> Tree a -> Forest a
filterTree p (Node x ts) | p x       = [Node x (filterForest p ts)]
                         | otherwise = ts

addVersionParagraphs :: NameSpace -> ModuleDoc -> ModuleDoc
addVersionParagraphs namespace apiDoc =
  apiDoc {
    moduledoc_summary = moduledoc_summary apiDoc ++ moduleVersionParagraph
                                                 ++ moduleDeprecatedParagraph,
    moduledoc_functions = functionVersionParagraphs moduleVersion (moduledoc_functions apiDoc),
    moduledoc_since = moduleVersion
  }
  where functionVersionParagraphs :: String -> [FuncDoc] -> [FuncDoc]
        functionVersionParagraphs baseVersion funcdocs =
          [ if funcdoc_since funcdoc > baseVersion
              then funcdoc {
                     funcdoc_paragraphs = funcdoc_paragraphs funcdoc ++
                       let line = "Available since " ++ (let name = namespace_name namespace
                                                          in if name == "Gtk" then "Gtk+" else name)
                              ++ " version " ++ funcdoc_since funcdoc
                        in [DocParaListItem [DocText line]]
                   }
              else let method = lookup (funcdoc_name funcdoc) methodMap
                       methodDeprecated = maybe False method_deprecated method
                       objectDeprecated = maybe False object_deprecated object
                   in if methodDeprecated && not objectDeprecated
                        then funcdoc {
                               funcdoc_paragraphs = funcdoc_paragraphs funcdoc ++
                                 let line = "Warning: this function is deprecated "
                                         ++ "and should not be used in newly-written code."
                                  in [DocParaListItem [DocText line]]
                             }
                        else funcdoc
          | funcdoc <- funcdocs ]
          where methodMap = [ (method_cname method, method)
                            | method <- maybe [] object_methods object ]
  
        moduleVersionParagraph =
          case moduleVersion of
            "" -> []
            since ->
              let line = "Module available since " ++ (let name = namespace_name namespace
                                                          in if name == "Gtk" then "Gtk+" else name)
                      ++ " version " ++ since
               in [DocParaListItem [DocText line]]       
  
        -- figure out if the whole module appeared in some version of gtk later 
        -- than the original version
        moduleVersion :: String
        moduleVersion | null fixed = case [ funcdoc_since funcdoc
                                          | funcdoc <- moduledoc_functions apiDoc ] of
                                       [] -> ""
                                       versions -> minimum versions
                      | otherwise = fixed
          where fixed = fixModuleAvailableSince (moduledoc_name apiDoc)

        moduleDeprecatedParagraph =
          if maybe False object_deprecated object
            then let line = "Warning: this module is deprecated "
                         ++ "and should not be used in newly-written code."
            
                  in [DocParaListItem [DocText line]]
            else []
        
        object = lookup (moduledoc_name apiDoc)
                   [ (object_cname object, object)
                   | object <- namespace_objects namespace ]
  
haddocFormatSections :: KnownSymbols -> [DocSection] -> ShowS
haddocFormatSections knownSymbols = 
    sepBy' "\n\n-- "
  . map (\section ->
         ss "** ". ss (section_title section). nl.
         comment.nl.
         comment.ss "| ".haddocFormatParas knownSymbols False (section_paras section))

haddocFormatParas :: KnownSymbols -> Bool -> [DocPara] -> ShowS
haddocFormatParas knownSymbols handleNULLs =
    sepBy' "\n--\n-- "
  . map (haddocFormatPara knownSymbols handleNULLs)

haddocFormatPara :: KnownSymbols -> Bool -> DocPara -> ShowS
haddocFormatPara knownSymbols handleNULLs (DocParaText spans) = haddocFormatSpans knownSymbols handleNULLs 3 spans
haddocFormatPara knownSymbols _ (DocParaProgram prog) =
    ((ss "* FIXME: if the follwing is a C code example, port it to Haskell or remove it".nl.
      comment).)
  . sepBy "\n-- > "
  . List.lines
  $ prog
haddocFormatPara knownSymbols _ (DocParaTitle title) =
    ss "* ". ss title
haddocFormatPara knownSymbols handleNULLs (DocParaDefItem term spans) =
  let def = (unwords . words . escape . concatMap (haddocFormatSpan knownSymbols handleNULLs)) term in
  sc '['. ss def. ss "] ".
  haddocFormatSpans knownSymbols handleNULLs (length def + 6) spans
  where escape [] = []
        escape (']':cs) = '\\': ']' : escape cs --we must escape ] in def terms
        escape (c:cs)   =        c  : escape cs
haddocFormatPara knownSymbols handleNULLs (DocParaListItem spans) =
  ss "* ".
  haddocFormatSpans knownSymbols handleNULLs 5 spans

haddocFormatSpans :: KnownSymbols -> Bool -> Int -> [DocParaSpan] -> ShowS
haddocFormatSpans knownSymbols handleNULLs initialCol =
    sepBy' "\n-- "
  . map (sepBy " ")
  . wrapText initialCol 77
  . map (mungeWord knownSymbols handleNULLs)
  . words
  . concatMap (haddocFormatSpan knownSymbols handleNULLs)

haddocFormatSpan :: KnownSymbols -> Bool -> DocParaSpan -> String
haddocFormatSpan _ _ (DocText text)       = escapeHaddockSpecialChars text
haddocFormatSpan knownSymbols handleNULLs (DocTypeXRef text) =
  case Map.lookup text knownSymbols of
    Nothing | text == "TRUE"  -> "@True@"
            | text == "FALSE"          -> "@False@"
            | otherwise                -> "{" ++ text ++ ", FIXME: unknown type/value}"
    Just (SymObjectType _)             -> "'" ++ cTypeNameToHSType text ++ "'"
    Just (SymEnumType _)               -> "'" ++ cTypeNameToHSType text ++ "'"
    Just SymEnumValue                  -> "'" ++ cConstNameToHsName text ++ "'"
    Just SymStructType                 -> "{" ++ text ++ ", FIXME: struct type}"
    Just SymBoxedType                  -> if knownMiscType text
                                            then "'" ++ cTypeNameToHSType text ++ "'"
                                            else "{" ++ text ++ ", FIXME: boxed type}"
    Just SymClassType                  -> "{" ++ text ++ ", FIXME: class type}"
    Just SymTypeAlias                  -> "{" ++ text ++ ", FIXME: type alias}"
    Just SymCallbackType               -> "{" ++ text ++ ", FIXME: callback type}"
haddocFormatSpan _ _ (DocFuncXRef text)   = "'" ++ cFuncNameToHsName text ++ "'"
haddocFormatSpan _ _ (DocOtherXRef text)  = "'{FIXME: gtk-doc cross reference to:" ++ text ++ "}'"
haddocFormatSpan _ _ (DocEmphasis text)   = "/" ++ text ++ "/"
haddocFormatSpan _ _ (DocLiteral "TRUE")  = "@True@"
haddocFormatSpan _ _ (DocLiteral "FALSE") = "@False@"
  --likely that something should be changed to a Maybe type if this is emitted:
haddocFormatSpan _ handleNULLs (DocLiteral "NULL") = 
 if handleNULLs
   then "@Nothing@"
   else "{@NULL@, FIXME: this should probably be converted to a Maybe data type}"
haddocFormatSpan knownSymbols _ (DocLiteral text) =
  case Map.lookup text knownSymbols of
    Nothing                            -> "@" ++ escapeHaddockSpecialChars text ++ "@"
    Just SymEnumValue                  -> "'" ++ cConstNameToHsName text ++ "'"
    Just (SymObjectType _)             -> "'" ++ cTypeNameToHSType text ++ "'"
    _ -> "{" ++ text ++ ", FIXME: unknown literal value}" --TODO fill in the other cases
haddocFormatSpan _ _ (DocArg  text)       = "@" ++ cParamNameToHsName text ++ "@"

cFuncNameToHsName :: String -> String
cFuncNameToHsName =
    lowerCaseFirstChar
  . cTypeNameToHSType
  . toStudlyCapsWithFixups
  . takeWhile ('('/=)

cParamNameToHsName :: String -> String
cParamNameToHsName  =          --change "gtk_foo_bar" to "gtkFooBar"
    lowerCaseFirstChar
  . toStudlyCaps

cConstNameToHsName :: String -> String
cConstNameToHsName  =          --change "GTK_UPDATE_DISCONTINUOUS" to "UpdateDiscontinuous"
    cTypeNameToHSType
  . toStudlyCaps
  . map toLower

toStudlyCaps :: String -> String
toStudlyCaps =                 --change "gtk_foo_bar" to "GtkFooBar"
    concatMap upperCaseFirstChar
  . filter (not.null) --to ignore tailing underscores
  . splitBy '_'

toStudlyCapsWithFixups :: String -> String
toStudlyCapsWithFixups =                 --change "gtk_foo_bar" to "GtkFooBar"
    concatMap upperCaseFirstChar
  . map fixCFunctionName
  . filter (not.null) --to ignore tailing underscores
  . splitBy '_'

changeIllegalNames :: String -> String
changeIllegalNames "type" = "type_"   --these are common variable names in C but
changeIllegalNames "where" = "where_" --of course are keywords in Haskell
changeIllegalNames "data" = "data_"
changeIllegalNames other = other

escapeHaddockSpecialChars = escape
  where escape [] = []
        escape ('\'':'s':s:cs) | isSpace s = '\'' : 's' : ' ' : escape cs --often don't need to escape
        escape ('\'':'t':s:cs) | isSpace s = '\'' : 't' : ' ' : escape cs --eg it's & don't
        escape (c:cs) | c == '/' || c == '`'
                     || c == '"' || c == '@'
                     || c == '<' || c == '\''
                      = '\\': c : escape cs
        escape (c:cs) =       c : escape cs

mungeWord :: KnownSymbols -> Bool -> String -> String
mungeWord knownSymbols _ ('G':'T':'K':[])            = "Gtk+"
mungeWord knownSymbols _ ('G':'T':'K':'+':remainder) = "Gtk+" ++ remainder
mungeWord knownSymbols handleNULLs word
                 | word' == "TRUE"       = "@True@"  ++ remainder
                 | word' == "FALSE"      = "@False@" ++ remainder
                 | word' == "NULL" = if handleNULLs
                                       then "@Nothing@" ++ remainder
                                       else "{@NULL@, FIXME: this should probably "
                                         ++ "be converted to a Maybe data type}" ++ remainder
                 | word' == "G_MAXINT"   = "@('maxBound' :: Int)@" ++ remainder
                 | isJust e = case e of
                                Just (SymObjectType _) -> "'" ++ cTypeNameToHSType word' ++ "'" ++ remainder
                                Just (SymEnumType _)   -> "'" ++ cTypeNameToHSType word' ++ "'" ++ remainder
                                Just SymEnumValue      -> "'" ++ cConstNameToHsName word' ++ "'" ++ remainder
                                Just SymStructType     -> "{" ++ word' ++ ", FIXME: struct type}"
                                Just SymBoxedType      -> if knownMiscType word'
                                                            then "'" ++ cTypeNameToHSType word' ++ "'"
                                                            else "{" ++ word' ++ ", FIXME: boxed type}"
                                Just SymClassType      -> "{" ++ word' ++ ", FIXME: class type}"
                                Just SymTypeAlias      -> "{" ++ word' ++ ", FIXME: type alias}"
                                Just SymCallbackType   -> "{" ++ word' ++ ", FIXME: callback type}"
                 | otherwise = word
  where e = Map.lookup word' knownSymbols
        (word', remainder) = span (\c -> isAlpha c || c == '_') word
