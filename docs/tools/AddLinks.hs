{-# OPTIONS -fglasgow-exts #-}
-- glasgow-exts for pattern gaurds

--  Program for adding hyperlinks and syntax highlighting to txt2tags output
--  from variable names to the corresponding Haddock html documentation.

--  Copyright (C) 2005  Duncan Coutts
--  derived in part from the program "hscolour" which is
--  Copyright (C) 2003,2005  Dr Malcolm Wallace
--  ftp://ftp.cs.york.ac.uk/pub/haskell/contrib/hscolour-1.1.tar.gz

--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.

--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.

module Main (main) where

import Char   (isUpper, isLower, isSpace, isDigit)
import List   (isPrefixOf)
import System (getArgs)

import qualified Data.Map as Map

-------------------------------------------------------------------------------
-- Top level stuff
-------------------------------------------------------------------------------

main = do
  args <- getArgs
--  when (length args < 2) usage

  -----------------------------------------------------------------------------
  -- Parse command line parameters
  --
  let (exportsFile: rem) = args
  
      target = case map (drop 9) (filter ("--target=" `isPrefixOf`)  rem) of
                    ("tex":_) -> LaTeX
                    ("html":_) -> HTML
                    ("xhtml":_) -> XHTML
                    [] -> error "please specify --target=<target>"
                    _  -> error "supported targets: tex, xhtml"

      baseUrl = case map (drop 10) (filter ("--baseurl=" `isPrefixOf`)  rem) of
                    [] -> ""
                    (baseUrl:_) -> baseUrl

  -----------------------------------------------------------------------------
  -- Read in the input files
  --
  content <- getContents
  exports <- readFile exportsFile
      
  -----------------------------------------------------------------------------
  -- Parse the contents of the exports file
  --
  let concordance = parseExports exports

  -----------------------------------------------------------------------------
  -- Write the result to stdout
  --
  putStr . concat . substitute target concordance baseUrl . tokenise $ content


-----------------------------------------------------------------------------
-- Firstly, building the concordance,
-- mapping exported symbols to their defining modules
--
type Concordance = (String -> Maybe String)

parseExports :: String -> Concordance
parseExports content = flip Map.lookup concordance
  where concordance = Map.fromList
                    . concat
                    . map (extractLine . words)
                    . lines
                    $ content
        extractLine ("export":moduleName:symbols) =
          [ (symbol, moduleName) | symbol <- symbols ]
        extractLine line = error $ "parseExports: cannot parse: " ++ show line

-----------------------------------------------------------------------------
-- Secondly, scanning, munging stuff
-- to find the code snippets and to do all necesary conversion/escaping
-- and add in the links and call the next bit to do any syntax highliting
--
data DocType = LaTeX | XHTML | HTML

substitute :: DocType -> Concordance -> String -> [String] -> [String]
substitute dt conc baseUrl = substNormal dt
  where substNormal _ [] = []
        substNormal LaTeX ("\\":"begin":"{":"verbatim":"}":('\n':sp)
                          :ts) = let (codeArea, ts') = findCodeArea dt (sp:ts)
                                  in "\\begin{tabbing}\\tt\n"
                                    : substCodeArea dt codeArea
                                   ++ "\\end{tabbing}"
                                    : substNormal dt ts'
        substNormal XHTML ("<":"pre":">":('\n':s)
                          :ts) = let (codeArea, ts') = findCodeArea dt (s:ts)
                                  in "<pre class=\"haskellcode\">"
                                   : substCodeArea dt codeArea
                                  ++ "</pre>"
                                   : substNormal dt ts'
        substNormal  HTML ("<":"pre":">":('\n':s)
                          :ts) = let (codeArea, ts') = findCodeArea dt (s:ts)
                                  in "<pre>"
                                   : substCodeArea dt codeArea
                                  ++ "</pre>"
                                   : substNormal dt ts'
        substNormal dt (t:ts)  = t : substNormal dt ts
        
        findCodeArea LaTeX ("\\":"end":"{":"verbatim":"}":ts) = ([], ts)
        findCodeArea XHTML ("</":"pre":">"               :ts) = ([], ts)
        findCodeArea  HTML ("</":"pre":">"               :ts) = ([], ts)
        findCodeArea dt (t:ts)  = (t:ts',ts'')
                              where (ts',ts'') = findCodeArea dt ts
        
        substCodeArea LaTeX = map (addLinks dt)
        substCodeArea XHTML = map (\s -> renderTag dt (classifyToken s) (addLinks dt s)) . glue
        substCodeArea  HTML = map (\s -> renderTag dt (classifyToken s) (addLinks dt s)) . glue
        
        addLinks LaTeX str | Just modName <- conc str = "\\href{"
                                           ++ haddockUrl baseUrl modName str
                                           ++ "}{" ++ str ++ "}"
                           | all isSpace str  = convertLatexSpace str
                           | otherwise        = escapeLatexSymbol str
        addLinks XHTML str | Just modName <- conc str = "<a href=\""
                                           ++ haddockUrl baseUrl modName str
                                           ++ "\">" ++ str ++ "</a>"
        addLinks  HTML str | Just modName <- conc str = "<a href=\""
                                           ++ haddockUrl baseUrl modName str
                                           ++ "\">" ++ str ++ "</a>"
        addLinks _     str = str

haddockUrl baseUrl modName varName =
  baseUrl ++ modName ++ ".html#v:" ++ varName

convertLatexSpace :: String -> String
convertLatexSpace []        = []
convertLatexSpace (' ':s)   = '~' : convertLatexSpace s
convertLatexSpace ('\n':[]) = "\n"
convertLatexSpace ('\n':s)  = "\\\\\n\\tt " ++ convertLatexSpace s

escapeLatexSymbol [] = []
escapeLatexSymbol (c:cs) | c `elem` symbols = "\\char" ++ show (fromEnum c) ++ " "
                                           ++ escapeLatexSymbol cs
                         | otherwise = c : escapeLatexSymbol cs
  where symbols = "{}#&%^$_\\"

-----------------------------------------------------------------------------
-- Finally, all the lexing, syntax recognition and html tagging
--
tokenise :: String -> [String]
tokenise = checkSpace
  where checkSpace s =
          case span isSpace s of
            ("","") -> []
            (sp,"") -> [sp]
            ("",s') -> checkLexable s'
            (sp,s') -> sp : checkLexable s'
        
        checkLexable s =
          case Prelude.lex s of
            ((tok,s'):_) -> tok : tokenise s'
            _ -> case span isBoundary s of
                  ("", s') -> case break isSpaceOrBoundary s' of
                                (w,s'') -> w : tokenise s''
                  (w , s') -> w : tokenise s'

        isBoundary c = c `elem` ".,[]{}()<>#@%+*^$-=_!?:|\\/`\""
        isSpaceOrBoundary c = isSpace c || isBoundary c

data Classification = Space
  | Keyword | Keyglyph | Layout | Comment
  | ConId | VarId | ConOp | VarOp
  | String | Char | Number
  | Selection

-- Glue sequences of tokens into more useful blobs
glue ("`":rest) =				-- `varid` -> varop
  case glue rest of
    (qn:"`":rest) -> ("`"++qn++"`"): glue rest
    _             -> ("`": rest)
glue (s:ss)       | all (=='-') s		-- eol comment
                  = (s++concat c): glue rest
                  where (c,rest) = break ('\n'`elem`) ss
glue ("{":"-":ss)  = ("{-"++c): glue rest	-- nested comment
                  where (c,rest) = nestcomment 0 ss
 -- make escaped '->', '=>' and '<-' into a single token
glue ("-&":"gt":";":ss)    = "-&gt;" : glue ss
glue ("=&":"gt":";":ss)    = "=&gt;" : glue ss
glue ("&":"lt":";":"-":ss) = "&lt;-" : glue ss
glue (s:ss)       = s: glue ss
glue []           = []

nestcomment :: Int -> [String] -> (String,[String])
nestcomment n ("{":"-":ss) | n>=0 = (("{-"++cs),rm)
                                  where (cs,rm) = nestcomment (n+1) ss
nestcomment n ("-":"}":ss) | n>0  = (("-}"++cs),rm)
                                  where (cs,rm) = nestcomment (n-1) ss
nestcomment n ("-":"}":ss) | n==0 = ("-}",ss)
nestcomment n (s:ss)       | n>=0 = ((s++cs),rm)
                                  where (cs,rm) = nestcomment n ss
nestcomment n [] = error "no closing comment -}"

-- Classify tokens
classifyToken :: String -> Classification
classifyToken s@(h:_)
    | isSpace h              = Space
    | all (=='-') s          = Comment
    | "--" `isPrefixOf` s
      && any isSpace s       = Comment
    | "{-" `isPrefixOf` s    = Comment
    | s `elem` keywords      = Keyword
    | s `elem` keyglyphs     = Keyglyph
    | s `elem` layoutchars   = Layout
    | isUpper h              = ConId
    | isLower h              = VarId
    | h `elem` symbols       = VarOp
    | h==':'                 = ConOp
    | h=='`'                 = VarOp
    | h=='"'                 = String
    | h=='\''                = Char
    | isDigit h              = Number
    | otherwise              = Selection

renderTag :: DocType -> Classification -> String -> String
renderTag _ Space = id
renderTag XHTML Keyword    = spanTag "keyword"
renderTag XHTML Keyglyph   = spanTag "keyglyph"
renderTag XHTML Layout     = spanTag "layout"
renderTag XHTML Comment    = spanTag "comment"
renderTag XHTML ConId      = spanTag "conid"
renderTag XHTML VarId      = spanTag "varid"
renderTag XHTML ConOp      = spanTag "conop"
renderTag XHTML VarOp      = spanTag "varop"
renderTag XHTML String     = spanTag "string"
renderTag XHTML Char       = spanTag "char"
renderTag XHTML Number     = spanTag "number"
renderTag XHTML Selection  = spanTag "selection"

renderTag HTML Keyword    = tag "strong"
renderTag HTML Keyglyph   = tag "strong"
renderTag HTML Layout     = id
renderTag HTML Comment    = tag "emphasis"
renderTag HTML ConId      = id
renderTag HTML VarId      = id
renderTag HTML ConOp      = id
renderTag HTML VarOp      = id
renderTag HTML String     = id
renderTag HTML Char       = id
renderTag HTML Number     = id
renderTag HTML Selection  = id

spanTag className s = "<span class=\"haskellcode-" ++ className ++ "\">" ++ s ++ "</span>"

tag tagName s = "<" ++ tagName ++ ">" ++ s ++ "</" ++ tagName ++ ">"

-- Haskell keywords
keywords =
  ["case","class","data","default","deriving","do","else"
  ,"if","import","in","infix","infixl","infixr","instance","let","module"
  ,"newtype","of","then","type","where","_","foreign","ccall","as"]
keyglyphs =
  ["..","::","=","\\","|","&lt;-","-&gt;","@","~","=&gt;","[","]"]
layoutchars =
  map (:[]) ";{}(),"
symbols =
  "!#$%&*+./<=>?@\\^|-~"
