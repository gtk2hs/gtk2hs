module HaddockDocs (
  Section(..),
  formatSections,
  
  Para(..),
  formatParas,
  formatParasFragment,
  
  Span(..),
  formatSpans,
  
  haddockSection,
  ) where

import Utils

import Data.List as List (lines, words, intersperse)
import Data.Char (isSpace)

data Section = Section Int String [Para]

data Para =
    ParaText           [Span]    -- an ordinary word-wrapped paragraph
  | ParaCode           [[Span]]  -- a @...@ code block
  | ParaVerbatm        [String]  -- a > bird-track verbatum code block
  | ParaTitle          String    -- a title to a subsection eg an example
  | ParaDefItem [Span] [Span]    -- a definition list item
  | ParaListItem       [Span]    -- a itemisted list item

data Span = SpanText      String -- just simple text
          | SpanIdent     String -- hyperlinked Identifiers
          | SpanModule    String -- hyperlinked module
          | SpanEmphasis  String -- emphasised text /.../
          | SpanMonospace [Span] -- monospace text @...@
          | SpanSpace     Int    -- Just a bunch of spaces
--        | SpanURL
--        | SpanImage
  deriving Show

formatSections :: [Section] -> Doc
formatSections =
    vsep
  . map (\(Section level title paras) ->
      if null paras
        then empty
        else comment <+> text (replicate level '*') <+> text title
          $$ comment
          $$ formatParas 77 paras)

formatParasFragment :: [Para] -> Doc
formatParasFragment =
    haddockSection empty
  . concat
  . intersperse [empty]
  . map (formatPara 77)

formatParas :: Int -> [Para] -> Doc
formatParas width =
    haddockSection (char '|')
  . concat
  . intersperse [empty]
  . map (formatPara width)

formatPara :: Int -> Para -> [Doc]
formatPara width para = case para of
  ParaText       spans -> formatSpans 3 width spans
  ParaCode       lines -> [empty, char '@']
                       ++ map (hcat . map (text . formatSpan)) lines
                       ++ [char '@']
  ParaVerbatm     code -> map (\line -> char '>' <+> text line) code
  ParaTitle      title -> [char '*' <+> text title]
  ParaDefItem term def -> let term' = escape (concatMap formatSpan term)
                              indent = length term' + 6
                           in prependToFirstLine (brackets (text term'))
                                (formatSpans indent width def)
  ParaListItem   spans -> prependToFirstLine (char '*')
                            (formatSpans 5 width spans)
  where escape [] = []
        escape (']':cs) = '\\': ']' : escape cs --we must escape ] in def terms
        escape (c:cs)   =        c  : escape cs

formatSpans :: Int -> Int -> [Span] -> [Doc]
formatSpans initialIndent width =
    map (hsep . map text)
  . wrapText initialIndent width
  . words
  . concatMap formatSpan
  . coalesceTextSpans

formatSpan :: Span -> String
formatSpan (SpanText      text) = escapeHaddockSpecialChars text
formatSpan (SpanIdent     text) = "'" ++ text ++ "'"
formatSpan (SpanModule    text) = "\"" ++ text ++ "\""
formatSpan (SpanEmphasis  text) = "/" ++ escapeHaddockSpecialChars text ++ "/"
formatSpan (SpanMonospace spans) = "@" ++ concatMap formatSpan spans ++ "@"
formatSpan (SpanSpace      len) = replicate len ' '

coalesceTextSpans :: [Span] -> [Span]
coalesceTextSpans = coalesce []
  where coalesce texts (SpanText text : spans) = coalesce (text:texts) spans
        coalesce texts (SpanSpace len : spans) = coalesce (text':texts) spans
                                   where text' = replicate len ' '

        coalesce []    (span : spans) = span : coalesce [] spans
        coalesce texts (span : spans) = SpanText (concat (reverse texts))
                                      : span : coalesce [] spans

        coalesce []    [] = []
        coalesce texts [] = [SpanText (concat (reverse texts))]

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

-------------------------------------------------------------------------------
-- Extra pretty printing bits
-------------------------------------------------------------------------------

prependToFirstLine :: Doc -> [Doc] -> [Doc]
prependToFirstLine start [] = []
prependToFirstLine start (line:lines) = (start <+> line) : lines

haddockSection :: Doc -> [Doc] -> Doc
haddockSection start = commentBlock . prependToFirstLine start
