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
  toStudlyCaps,
  haddocFormatParas,
  haddocFormatSpans,
  haddocFormatSpan,
  mungeWord,
  changeIllegalNames,
  addVersionParagraphs
) where

import Api (NameSpace(namespace_name))
import Docs
import Marshal (stripKnownPrefixes, knownMiscType, KnownSymbols, CSymbol(..))
import StringUtils

import Maybe (isJust)
import Char (toLower, isUpper, isAlpha, isSpace)
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
          comment.ss "| ".haddocFormatParas knownSymbols False (moduledoc_description moduledoc).nl).
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
  . map haddocTweakHierarchy
  . Prelude.lines
  . concatMap (haddocFormatSpan knownSymbols False)

haddocTweakHierarchy :: String -> String
haddocTweakHierarchy ('+':'-':'-':'-':'-':cs@(c:_)) | c /= ''' =
  case span isAlpha cs of (word, rest) -> "+----" ++ stripKnownPrefixes word ++ rest
haddocTweakHierarchy (c:cs) = c : haddocTweakHierarchy cs
haddocTweakHierarchy [] = []

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
  case lookupFM knownSymbols text of
    Nothing | text == "TRUE"  -> "@True@"
            | text == "FALSE"          -> "@False@"
            | otherwise                -> "{" ++ text ++ ", FIXME: unknown type/value}"
    Just (SymObjectType _)             -> "'" ++ stripKnownPrefixes text ++ "'"
    Just (SymEnumType _)               -> "'" ++ stripKnownPrefixes text ++ "'"
    Just SymEnumValue                  -> "'" ++ cConstNameToHsName text ++ "'"
    Just SymStructType                 -> "{" ++ text ++ ", FIXME: struct type}"
    Just SymBoxedType                  -> if knownMiscType text
                                            then "'" ++ stripKnownPrefixes text ++ "'"
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
  case lookupFM knownSymbols text of
    Nothing                            -> "@" ++ escapeHaddockSpecialChars text ++ "@"
    Just SymEnumValue                  -> "'" ++ cConstNameToHsName text ++ "'"
    _ -> "{" ++ text ++ ", FIXME: unknown literal value}" --TODO fill in the other cases
haddocFormatSpan _ _ (DocArg  text)       = "@" ++ cParamNameToHsName text ++ "@"

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
cConstNameToHsName  =          --change "GTK_UPDATE_DISCONTINUOUS" to "UpdateDiscontinuous"
    stripKnownPrefixes
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
        escape (''':'s':s:cs) | isSpace s = ''' : 's' : ' ' : escape cs --often don't need to escape
        escape (''':'t':s:cs) | isSpace s = ''' : 't' : ' ' : escape cs --eg it's & don't
        escape (c:cs) | c == '/' || c == '`'
                     || c == '"' || c == '@'
                     || c == '<' || c == '''
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
                                Just (SymObjectType _) -> "'" ++ stripKnownPrefixes word' ++ "'" ++ remainder
                                Just (SymEnumType _)   -> "'" ++ stripKnownPrefixes word' ++ "'" ++ remainder
                                Just SymEnumValue      -> "'" ++ cConstNameToHsName word' ++ "'" ++ remainder
                                Just SymStructType     -> "{" ++ word' ++ ", FIXME: struct type}"
                                Just SymBoxedType      -> if knownMiscType word'
                                                            then "'" ++ stripKnownPrefixes word' ++ "'"
                                                            else "{" ++ word' ++ ", FIXME: boxed type}"
                                Just SymClassType      -> "{" ++ word' ++ ", FIXME: class type}"
                                Just SymTypeAlias      -> "{" ++ word' ++ ", FIXME: type alias}"
                                Just SymCallbackType   -> "{" ++ word' ++ ", FIXME: callback type}"
                 | otherwise = word
  where e = lookupFM knownSymbols word'
        (word', remainder) = span (\c -> isAlpha c || c == '_') word
