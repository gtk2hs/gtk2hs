-- ApiGen: takes an xml description of a GObject-style API and produces a .chs
-- binding module. Optionally it can be supplied with an xml documentation file
-- in which case the .chs file will contain haddock-format documentation too.

-- If you want to teach ApiGen how to marshal new types, the function you want
-- to modify is either genMarshalParameter or genMarshalResult near the end of
-- this file.

module FormatDocs (
  genModuleDocumentation,
  cFuncNameToHsName,
  cParamNameToHsName,
  haddocFormatParas,
  haddocFormatSpans,
  haddocFormatSpan,
  mungeWord,
  changeIllegalNames,
  addVersionParagraphs
) where

import Api (NameSpace(namespace_name))
import Docs
import Marshal (stripKnownPrefixes, KnownSymbols, CSymbol(..))
import StringUtils

import Maybe (isJust)
import Char (toLower, isUpper, isAlpha)
import qualified List (lines)
import Data.FiniteMap

-------------------------------------------------------------------------------
-- Functions for formatting haddock documentation
-------------------------------------------------------------------------------

genModuleDocumentation :: KnownSymbols -> ModuleDoc -> ShowS
genModuleDocumentation knownSymbols moduledoc =
  (if null (moduledoc_description moduledoc)
     then id
     else comment.ss "* Description".nl.
          comment.nl.
          comment.ss "| ".haddocFormatParas knownSymbols (moduledoc_description moduledoc).nl).
  (if null (moduledoc_sections moduledoc)
     then id
     else nl.comment.haddocFormatSections knownSymbols (moduledoc_sections moduledoc).nl.comment.nl).
  (if null (moduledoc_hierarchy moduledoc)
     then id
     else nl.comment.ss "* Class Hierarchy".nl.
          comment.ss "|".nl.
          comment.ss "@".nl.
          comment.ss "|  ".haddocFormatHierarchy knownSymbols (moduledoc_hierarchy moduledoc).nl.
          comment.ss "@".nl)

haddocFormatHierarchy :: KnownSymbols -> [DocParaSpan] -> ShowS
haddocFormatHierarchy knownSymbols =
    sepBy "\n-- |"
  . Prelude.lines
  . concatMap (haddocFormatSpan knownSymbols)

addVersionParagraphs :: NameSpace -> ModuleDoc -> ModuleDoc
addVersionParagraphs namespace apiDoc =
  apiDoc {
    moduledoc_description = moduledoc_description apiDoc ++ moduleVersionParagraph,
    moduledoc_functions = functionVersionParagraphs moduleVersion (moduledoc_functions apiDoc)
  }
  where functionVersionParagraphs :: String -> [FuncDoc] -> [FuncDoc]
        functionVersionParagraphs baseVersion funcdocs =
          [ if funcdoc_since funcdoc > baseVersion
              then funcdoc {
                     funcdoc_paragraphs = funcdoc_paragraphs funcdoc ++
                       let line = "* Available since " ++ namespace_name namespace
                              ++ " version " ++ funcdoc_since funcdoc
                        in [DocParaText [DocText line]]
                   }
              else funcdoc
          | funcdoc <- funcdocs ]
  
        moduleVersionParagraph =
          case moduleVersion of
            "" -> []
            since ->
              let line = "* Module available since " ++ namespace_name namespace
                      ++ " version " ++ since
               in [DocParaText [DocText line]]
  
        -- figure out if the whole module appeared in some version of gtk later 
        -- than the original version
        moduleVersion :: String
        moduleVersion = case [ funcdoc_since funcdoc
                             | funcdoc <- moduledoc_functions apiDoc ] of
                          [] -> ""
                          versions -> minimum versions
  
haddocFormatSections :: KnownSymbols -> [DocSection] -> ShowS
haddocFormatSections knownSymbols = 
    sepBy' "\n\n-- "
  . map (\section ->
         ss "** ". ss (section_title section). nl.
         comment.nl.
         comment.ss "| ".haddocFormatParas knownSymbols (section_paras section))

haddocFormatParas :: KnownSymbols -> [DocPara] -> ShowS
haddocFormatParas knownSymbols =
    sepBy' "\n--\n-- "
  . map (haddocFormatPara knownSymbols)

haddocFormatPara :: KnownSymbols -> DocPara -> ShowS
haddocFormatPara knownSymbols (DocParaText spans) = haddocFormatSpans knownSymbols 3 spans
haddocFormatPara knownSymbols (DocParaProgram prog) =
    ((ss "* FIXME: if the follwing is a C code example, port it to Haskell or remove it".nl.
      comment).)
  . sepBy "\n-- > "
  . List.lines
  $ prog
haddocFormatPara knownSymbols (DocParaTitle title) =
    ss "* ". ss title
haddocFormatPara knownSymbols (DocParaDefItem term spans) =
  let def = (unwords . words . escape . concatMap (haddocFormatSpan knownSymbols)) term in
  sc '['. ss def. ss "] ".
  haddocFormatSpans knownSymbols (length def + 6) spans
  where escape [] = []
        escape (']':cs) = '\\': ']' : escape cs --we must escape ] in def terms
        escape (c:cs)   =        c  : escape cs
haddocFormatPara knownSymbols (DocParaListItem spans) =
  ss "* ".
  haddocFormatSpans knownSymbols 5 spans

haddocFormatSpans :: KnownSymbols -> Int -> [DocParaSpan] -> ShowS
haddocFormatSpans knownSymbols initialCol =
    sepBy' "\n-- "
  . map (sepBy " ")
  . wrapText initialCol 77
  . map (mungeWord knownSymbols)
  . words
  . concatMap (haddocFormatSpan knownSymbols)

haddocFormatSpan :: KnownSymbols -> DocParaSpan -> String
haddocFormatSpan _ (DocText text)       = escapeHaddockSpecialChars text
haddocFormatSpan knownSymbols (DocTypeXRef text) =
  case lookupFM knownSymbols text of
    Nothing -> "{" ++ text ++ ", FIXME: unknown type/value}"
    Just (SymObjectType _) -> "\"" ++ stripKnownPrefixes text ++ "\""
    Just (SymEnumType _)   -> "'" ++ stripKnownPrefixes text ++ "'"
    Just SymEnumValue      -> "'" ++ cConstNameToHsName text ++ "'"
    _ -> "{" ++ text ++ ", FIXME: unknown type/value}" --TODO fill in the other cases
--             | looksLikeConstant text = "'" ++ cConstNameToHsName text ++ "'"
--             | otherwise              = "\"" ++ stripKnownPrefixes text ++ "\""
haddocFormatSpan _ (DocFuncXRef text)   = "'" ++ cFuncNameToHsName text ++ "'"
haddocFormatSpan _ (DocOtherXRef text)  = "'{FIXME: gtk-doc cross reference to:" ++ text ++ "}'"
haddocFormatSpan _ (DocEmphasis text)   = "/" ++ text ++ "/"
haddocFormatSpan _ (DocLiteral "TRUE")  = "@True@"
haddocFormatSpan _ (DocLiteral "FALSE") = "@False@"
  --likely that something should be changed to a Maybe type if this is emitted:
haddocFormatSpan _ (DocLiteral "NULL")  = "{@NULL@, FIXME: this should probably be converted"
                                                     ++ " to a Maybe data type}"
haddocFormatSpan _ (DocLiteral text) = "@" ++ escapeHaddockSpecialChars text ++ "@"
haddocFormatSpan _ (DocArg  text)    = "@" ++ cParamNameToHsName text ++ "@"

cFuncNameToHsName :: String -> String
cFuncNameToHsName =
    lowerCaseFirstChar
  . stripKnownPrefixes
  . toStudlyCaps
  . takeWhile ('('/=)

cParamNameToHsName :: String -> String
cParamNameToHsName  =          --change "gtk_foo_bar" to "gtkFooBar"
    lowerCaseFirstChar
  . toStudlyCaps

cConstNameToHsName :: String -> String
cConstNameToHsName  =          --change "GTK_UPDATE_DISCONTINUOUS" to "updateDiscontinuous"
    lowerCaseFirstChar
  . stripKnownPrefixes
  . toStudlyCaps
  . map toLower

toStudlyCaps :: String -> String
toStudlyCaps =                 --change "gtk_foo_bar" to "GtkFooBar"
    concatMap upperCaseFirstChar
  . filter (not.null) --to ignore tailing underscores
  . splitBy '_'

changeIllegalNames :: String -> String
changeIllegalNames "type" = "type_"  --this is a common variable name in C but of
                                     --course a keyword in Haskell
changeIllegalNames other = other

escapeHaddockSpecialChars = escape
  where escape [] = []
        escape (''':'s':cs) = ''' : 's' : escape cs --often don't need to escape
        escape (c:cs) | c == '/' || c == '`'
                     || c == '"' || c == '@'
                     || c == '<' || c == '''
                      = '\\': c : escape cs
        escape (c:cs) =       c : escape cs

mungeWord :: KnownSymbols -> String -> String
mungeWord knownSymbols ('G':'T':'K':'+':remainder) = "Gtk+" ++ remainder
mungeWord knownSymbols word
                 | word' == "TRUE"       = "@True@"  ++ remainder
                 | word' == "FALSE"      = "@False@" ++ remainder
                 | word' == "NULL" = "{@NULL@, FIXME: this should probably be converted to a Maybe data type}"
                                                     ++ remainder
                 | isJust e = case e of
                                Just (SymObjectType _) -> "\"" ++ stripKnownPrefixes word' ++ "\"" ++ remainder
                                Just (SymEnumType _)   -> "'" ++ stripKnownPrefixes word' ++ "'" ++ remainder
                                Just SymEnumValue      -> "'" ++ cConstNameToHsName word' ++ "'" ++ remainder
                 | otherwise = word
  where e = lookupFM knownSymbols word'
        (word', remainder) = span (\c -> isAlpha c || c == '_') word  
{-
mungeWord _ "GTK+"  = "Gtk+"
mungeWord _ "GTK+,"  = "Gtk+,"
mungeWord _ "GTK+."  = "Gtk+."
mungeWord _ "TRUE"  = "@True@"
mungeWord _ "FALSE" = "@False@"
mungeWord _ "TRUE,"  = "@True@,"
mungeWord _ "FALSE," = "@False@,"
mungeWord _ "NULL" = "{@NULL@, FIXME: this should probably be converted to a Maybe data type}"
mungeWord knownSymbols word | isJust e = case e of
                              Just (SymObjectType _) -> "\"" ++ stripKnownPrefixes word' ++ "\"" ++ remainder
                              Just (SymEnumType _)   -> "'" ++ stripKnownPrefixes word' ++ "'" ++ remainder
                              Just SymEnumValue      -> "'" ++ cConstNameToHsName word' ++ "'" ++ remainder
  where e = lookupFM knownSymbols word'
        (word', remainder) = span (\c -> isAlpha c || c == '_') word
mungeWord _ word = word
-}

-- eg C constants with names like GTK_UPDATE_DISCONTINUOUS
looksLikeConstant :: String -> Bool
looksLikeConstant ('G':'T':'K':'_':rest) = all (\c -> isUpper c || c == '_')  rest
looksLikeConstant ('G':'D':'K':'_':rest) = all (\c -> isUpper c || c == '_')  rest
looksLikeConstant ('P':'A':'N':'G':'O':'_':rest) = all (\c -> isUpper c || c == '_')  rest
looksLikeConstant _ = False
