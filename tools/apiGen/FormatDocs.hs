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
  haddocFormatSpan,
  changeIllegalNames,
  addVersionParagraphs
) where

import Api (NameSpace(namespace_name))
import Docs
import Marshal (stripKnownPrefixes)
import StringUtils

import qualified List (lines)

-------------------------------------------------------------------------------
-- Functions for formatting haddock documentation
-------------------------------------------------------------------------------

genModuleDocumentation :: ModuleDoc -> ShowS
genModuleDocumentation moduledoc =
  (if null (moduledoc_description moduledoc)
     then id
     else comment.ss "* Description".nl.
          comment.nl.
          comment.ss "| ".haddocFormatParas (moduledoc_description moduledoc).nl).
  (if null (moduledoc_sections moduledoc)
     then id
     else nl.comment.haddocFormatSections (moduledoc_sections moduledoc).nl.comment.nl).
  (if null (moduledoc_hierarchy moduledoc)
     then id
     else nl.comment.ss "* Class Hierarchy".nl.
          comment.ss "|".nl.
          comment.ss "@".nl.
          comment.ss "|  ".haddocFormatHierarchy (moduledoc_hierarchy moduledoc).nl.
          comment.ss "@".nl)

haddocFormatHierarchy :: [DocParaSpan] -> ShowS
haddocFormatHierarchy =
    sepBy "\n-- |"
  . Prelude.lines
  . concatMap haddocFormatSpan

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
  
haddocFormatSections :: [DocSection] -> ShowS
haddocFormatSections = 
    sepBy' "\n\n-- "
  . map (\section ->
         ss "** ". ss (section_title section). nl.
         comment.nl.
         comment.ss "| ".haddocFormatParas (section_paras section))

haddocFormatParas :: [DocPara] -> ShowS
haddocFormatParas =
    sepBy' "\n--\n-- "
  . map haddocFormatPara

haddocFormatPara :: DocPara -> ShowS
haddocFormatPara (DocParaText spans) = haddocFormatSpans 3 spans
haddocFormatPara (DocParaProgram prog) =
    ((ss "* FIXME: if the follwing is a C code example, port it to Haskell or remove it".nl.
      comment).)
  . sepBy "\n-- > "
  . List.lines
  $ prog
haddocFormatPara (DocParaTitle title) =
    ss "* ". ss title
haddocFormatPara (DocParaDefItem term spans) =
  let def = (unwords . words . escape . concatMap haddocFormatSpan) term in
  sc '['. ss def. ss "] ".
  haddocFormatSpans (length def + 6) spans
  where escape [] = []
        escape (']':cs) = '\\': ']' : escape cs --we must escape ] in def terms
        escape (c:cs)   =        c  : escape cs
haddocFormatPara (DocParaListItem spans) =
  ss "* ".
  haddocFormatSpans 5 spans

haddocFormatSpans :: Int -> [DocParaSpan] -> ShowS
haddocFormatSpans initialCol =
    sepBy' "\n-- "
  . map (sepBy " ")
  . wrapText initialCol 77
  . words
  . concatMap haddocFormatSpan

haddocFormatSpan :: DocParaSpan -> String
haddocFormatSpan (DocText text)    = escapeHaddockSpecialChars text
haddocFormatSpan (DocTypeXRef text)    = "\"" ++ stripKnownPrefixes text ++ "\""
haddocFormatSpan (DocFuncXRef text)    = "'" ++ cFuncNameToHsName text ++ "'"
haddocFormatSpan (DocOtherXRef text)    = "'{FIXME: gtk-doc cross reference to:" ++ text ++ "}'"
haddocFormatSpan (DocEmphasis text)  = "/" ++ text ++ "/"
haddocFormatSpan (DocLiteral "TRUE")  = "@True@"
haddocFormatSpan (DocLiteral "FALSE") = "@False@"
  --likely that something should be changed to a Maybe type if this is emitted:
haddocFormatSpan (DocLiteral "NULL")  = "{@NULL@, FIXME: this should probably be converted"
                                                     ++ " to a Maybe data type}"
haddocFormatSpan (DocLiteral text) = "@" ++ escapeHaddockSpecialChars text ++ "@"
haddocFormatSpan (DocArg  text)    = "@" ++ cParamNameToHsName text ++ "@"

cFuncNameToHsName :: String -> String
cFuncNameToHsName =
    lowerCaseFirstChar
  . stripKnownPrefixes
  . concatMap upperCaseFirstChar
  . filter (not.null) --to ignore leading underscores
  . splitBy '_'
  . takeWhile ('('/=)

cParamNameToHsName :: String -> String
cParamNameToHsName  =          --change "gtk_foo_bar" to "gtkFooBar"
    lowerCaseFirstChar
  . concatMap upperCaseFirstChar
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
