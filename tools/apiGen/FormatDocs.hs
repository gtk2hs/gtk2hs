{-# OPTIONS_GHC -fglasgow-exts #-}
-- ApiGen: takes an xml description of a GObject-style API and produces a .chs
-- binding module. Optionally it can be supplied with an xml documentation file
-- in which case the .chs file will contain haddock-format documentation too.

-- If you want to teach ApiGen how to marshal new types, the function you want
-- to modify is either genMarshalParameter or genMarshalResult near the end of
-- this file.

module FormatDocs (
  genModuleDocumentation,
  haddocFormatDeclaration,
  haddocFormatDescription,
  cFuncNameToHsName,
  cParamNameToHsName,
  cAttrNametoHsName,
  cFuncNameToHsPropName,
  haddocFormatParas,
  haddocFormatPara,
  haddocFormatSpan,
  mungeWord,
  changeIllegalNames,
) where

import Docs
import Marshal (KnownSymbols, CSymbol(..))
import MarshalFixup (cTypeNameToHSType, knownMiscType, fixCFunctionName)
import Utils

import Prelude hiding (span)
import Data.Char (toLower, isAlpha, isSpace)
import Data.Tree
import qualified Data.List as List (lines, span, intersperse)
import qualified Data.Map as Map


-------------------------------------------------------------------------------
-- Extra pretty printing bits
-------------------------------------------------------------------------------

prependToFirstLine :: Doc -> [Doc] -> [Doc]
prependToFirstLine start [] = []
prependToFirstLine start (line:lines) =
  start <+> line : lines

haddockSection :: Doc -> [Doc] -> Doc
haddockSection start = commentBlock . prependToFirstLine start

-------------------------------------------------------------------------------
-- Functions for formatting haddock documentation
-------------------------------------------------------------------------------

genModuleDocumentation :: KnownSymbols -> String -> [DocPara] -> [DocSection] -> Forest String -> Doc
genModuleDocumentation knownSymbols name description sections hierarchy =
      formattedDetail
  $+$ formattedSections
  $+$ formattedHierarchy
 
  where formattedDetail
          | null description = empty
          | otherwise =
                comment <+> text "* Detail"
             $$ comment <> space
             $$ haddocFormatParas knownSymbols False description

        formattedSections
          | null sections = empty
          | otherwise =
                haddocFormatSections knownSymbols sections
             $$ comment <> space

        formattedHierarchy
          | null hierarchy = empty
          | otherwise =
                comment <+> text "* Class Hierarchy"
             $$ comment <+> char '|'
             $$ comment <+> char '@'
             $$ haddocFormatHierarchy knownSymbols name hierarchy
             $$ comment <+> char '@'

haddocFormatDeclaration :: KnownSymbols -> Bool -> [DocPara] -> Doc
haddocFormatDeclaration _            _           [] = comment <+> char '|' <> space
                                                   $$ comment
haddocFormatDeclaration knownSymbols handleNULLs paragraphs =
      haddocFormatParas knownSymbols handleNULLs paragraphs
   $$ comment

haddocFormatHierarchy :: KnownSymbols -> String -> Forest String -> Doc
haddocFormatHierarchy knownSymbols module_cname =
    vcat
  . map (\line -> comment <+> text "|  " <> text line)
  . concatMap drawHierarchy
  . map (fmap (haddocFormatSpan knownSymbols False))
  . map (fmap (\s -> if s == module_cname
                       then DocText (cTypeNameToHSType s)
		       else DocTypeXRef s))

drawHierarchy :: Tree String -> [String]
drawHierarchy (Node x ts0) = x : drawSubTrees ts0
  where drawSubTrees [] = []
        drawSubTrees (t:ts) =
          shift " +----" "      " (drawHierarchy t) ++ drawSubTrees ts
        shift first other = zipWith (++) (first : repeat other)

haddocFormatDescription :: KnownSymbols -> [DocPara] -> Doc
haddocFormatDescription _ [] = comment <+> space
haddocFormatDescription knownSymbols paras =
    haddockSection empty
  . concat
  . List.intersperse [empty]
  . map (haddocFormatPara knownSymbols False)
  $ paras

haddocFormatSections :: KnownSymbols -> [DocSection] -> Doc
haddocFormatSections knownSymbols = 
    vcat
  . List.intersperse emptyLine
  . map (\section ->
         comment <+> text "**" <+> text (section_title section)
      $$ comment <> space
      $$ haddocFormatParas knownSymbols False (section_paras section))

haddocFormatParas :: KnownSymbols -> Bool -> [DocPara] -> Doc
haddocFormatParas knownSymbols handleNULLs =
    haddockSection (char '|')
  . concat
  . List.intersperse [empty]
  . map (haddocFormatPara knownSymbols handleNULLs)

haddocFormatPara :: KnownSymbols -> Bool -> DocPara -> [Doc]
haddocFormatPara knownSymbols handleNULLs (DocParaText spans) =
  haddocFormatSpans knownSymbols handleNULLs 3 spans

haddocFormatPara _ _ (DocParaProgram prog) =
     [text "* FIXME: if the follwing is a C code example, port it to Haskell or remove it"
     ,emptyLine]
  ++ code
  where code = map (\line -> char '>' <+> text line)
             . reverse
             . dropWhile (all isSpace)
             . reverse
             . dropWhile (all isSpace)
             . List.lines
             $ prog
haddocFormatPara _ _ (DocParaTitle title) =
    [char '*' <+> text title]

haddocFormatPara knownSymbols handleNULLs (DocParaDefItem term spans) =
  let def = unwords . words . escape . concatMap (haddocFormatSpan knownSymbols handleNULLs) $ term
   in prependToFirstLine (brackets (text def)) (haddocFormatSpans knownSymbols handleNULLs (length def + 6) spans)
  where escape [] = []
        escape (']':cs) = '\\': ']' : escape cs --we must escape ] in def terms
        escape (c:cs)   =        c  : escape cs

haddocFormatPara knownSymbols handleNULLs (DocParaListItem spans) =
  case haddocFormatSpans knownSymbols handleNULLs 5 spans of
    [] -> []
    (line:lines) -> char '*' <+> line : lines


haddocFormatSpans :: KnownSymbols -> Bool -> Int -> [DocParaSpan] -> [Doc]
haddocFormatSpans knownSymbols handleNULLs initialCol =
    map (hsep . map text)
  . wrapText initialCol 77
  . map (mungeWord knownSymbols handleNULLs)
  . words
  . concatMap (haddocFormatSpan knownSymbols handleNULLs)
  . concatMap fixSpan

fixSpan :: DocParaSpan -> [DocParaSpan]
fixSpan span@(DocTypeXRef text) =
  case List.span (/= ':') text of
    (text', remainder@(':':_)) -> DocTypeXRef text' : DocText remainder : []
    _                          -> [span]
fixSpan span = [span]

haddocFormatSpan :: KnownSymbols -> Bool -> DocParaSpan -> String
haddocFormatSpan _            _ (DocText text)     = escapeHaddockSpecialChars text
haddocFormatSpan knownSymbols _ (DocTypeXRef text) =
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

cAttrNametoHsName :: String -> String
cAttrNametoHsName  =          --change "label-xalign" to "LabelXAlign"
    toStudlyCapsWithFixups
  . map dashToUnderscore
  where dashToUnderscore '-' = '_'
        dashToUnderscore  c  =  c

cFuncNameToHsPropName :: String -> String
cFuncNameToHsPropName =
    concatMap upperCaseFirstChar
  . map fixCFunctionName
  . tail
  . dropWhile (/="get")
  . filter (not.null)
  . splitBy '_'

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

escapeHaddockSpecialChars :: String -> String
escapeHaddockSpecialChars = escape
  where escape [] = []
        escape ('\'':'s':s:cs) | isSpace s = '\'' : 's' : ' ' : escape cs --often don't need to escape
        escape ('\'':'t':s:cs) | isSpace s = '\'' : 't' : ' ' : escape cs --eg it's & don't
        escape (c:cs) | c == '/' || c == '`'
                     || c == '"' || c == '@'
                     || c == '<' || c == '\''
                      = '\\': c : escape cs
        escape ('\226':'\128':'\148':cs) = '-' : '-' : escape cs  -- UTF8 for EM dash
        escape (c:cs) =       c : escape cs

mungeWord :: KnownSymbols -> Bool -> String -> String
mungeWord _ _ ('G':'T':'K':[])            = "Gtk+"
mungeWord _ _ ('G':'T':'K':'+':remainder) = "Gtk+" ++ remainder
mungeWord knownSymbols handleNULLs word
                 | word' == "TRUE"       = "@True@"  ++ remainder
                 | word' == "FALSE"      = "@False@" ++ remainder
                 | word' == "NULL" = if handleNULLs
                                       then "@Nothing@" ++ remainder
                                       else "{@NULL@, FIXME: this should probably "
                                         ++ "be converted to a Maybe data type}" ++ remainder
                 | word' == "G_MAXINT"   = "@('maxBound' :: Int)@" ++ remainder
                 | Just e <- Map.lookup word' knownSymbols =
                     case e of
                       (SymObjectType _) -> "'" ++ cTypeNameToHSType word' ++ "'" ++ remainder
                       (SymEnumType _)   -> "'" ++ cTypeNameToHSType word' ++ "'" ++ remainder
                       SymEnumValue      -> "'" ++ cConstNameToHsName word' ++ "'" ++ remainder
                       SymStructType     -> "{" ++ word' ++ ", FIXME: struct type}"
                       SymBoxedType      -> if knownMiscType word'
                                              then "'" ++ cTypeNameToHSType word' ++ "'"
                                              else "{" ++ word' ++ ", FIXME: boxed type}"
                       SymClassType      -> "{" ++ word' ++ ", FIXME: class type}"
                       SymTypeAlias      -> "{" ++ word' ++ ", FIXME: type alias}"
                       SymCallbackType   -> "{" ++ word' ++ ", FIXME: callback type}"
                 | otherwise = word
  where (word', remainder) = List.span (\c -> isAlpha c || c == '_') word
