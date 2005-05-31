--  C -> Haskell Compiler: Lexer for C Header Files
--
--  Author : Manuel M T Chakravarty, Duncan Coutts
--  Created: 6 March 99
--
--  Version $Revision: 1.1 $ from $Date: 2005/05/31 18:17:37 $
--
--  Copyright (c) [1999..2004] Manuel M T Chakravarty
--  Copyright (c) 2005 Duncan Coutts
--
--  This file is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This file is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--- DESCRIPTION ---------------------------------------------------------------
--
--  Lexer for C header files after being processed by the C preprocessor
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--  We assume that the input already went through cpp.  Thus, we do not handle 
--  comments and preprocessor directives here.  The lexer recognizes all tokens
--  of ANCI C except those occuring only in function bodies.  It supports the
--  C99 `restrict' extension: <http://www.lysator.liu.se/c/restrict.html> as
--  well as inline functions.
--
--  Comments:
--
--  * There is no support for the optional feature of extended characters (see
--    K&R A2.5.2) or the corresponding strings (A2.6). 
--
--  * We add `typedef-name' (K&R 8.9) as a token, as proposed in K&R A13.
--    However, as these tokens cannot be recognized lexically, but require a
--    context analysis, they are never produced by the lexer, but instead have 
--    to be introduced in a later phase (by converting the corresponding
--    identifiers). 
--
--  * We also recognize GNU C `__attribute__', `__extension__', `__const', 
--    `__const__', `__inline', `__inline__', `__restrict', and `__restrict__'.
--
--  * Any line starting with `#pragma' is ignored.
--
--  With K&R we refer to ``The C Programming Language'', second edition, Brain
--  W. Kernighan and Dennis M. Ritchie, Prentice Hall, 1988.
--
--- TODO ----------------------------------------------------------------------
--
--  * `showsPrec' of `CTokCLit' should produce K&R-conforming escapes;
--    same for `CTokSLit'
--
--  * There are more GNU C specific keywords.  Add them and change `CParser'
--    correspondingly (in particular, most tokens within __attribute ((...))
--    expressions are actually keywords, but we handle them as identifiers at
--    the moment).
--

{

module CLexer2 (CToken(..), GnuCTok(..), lexC,
                P, parse, getPos, getNewName, addTypedef) where 

import Char      (isDigit)
--import List	     ((\\))
--import Monad	   (liftM)
import Numeric   (readDec, readOct, readHex)

import Common    (Position(Position), Pos(posOf))
import Utils     (Tag(tag))
--import Errors    (Error)
import UNames	 (NameSupply, Name, names, rootSupply)
import Idents    (Ident, lexemeToIdent, identToLexeme)

import Data.Set (Set)
import qualified Data.Set as Set (mkSet, addToSet, elementOf)

import Data.FiniteMap (FiniteMap)
import qualified Data.FiniteMap as Map

--import C2HSState (CST, raise, getNameSupply)

--for debug
import Debug.Trace (trace)
import qualified Data.Set as Set (emptySet)

}

$space = [ \ \t ] -- horazontal white space
$eol   = \n

$letter   = [a-zA-Z_]
$octdigit = 0-7
$digit    = 0-9
$digitNZ  = 1-9
$hexdigit = [0-9a-fA-F]

$inchar   = \0-\255 # [ \\ \' \n \f \r \v ]
$instr    = \0-\255 # [ \\ \" \n \f \r \v ]
$anyButNL = \0-\255 # \n
$infname  = \ -\127 # [ \\ \' \" ]
$visible  = \ -\127

@int = $digitNZ$digit*
@sp  = $space*

-- character escape sequence (follows K&R A2.5.2)
--
-- * also used for strings
--
@charesc  = \\([ntvbrfa\\\?\'\"]|$octdigit{1,3}|x$hexdigit+)

-- components of float constants (follows K&R A2.5.3)
--
@digits    = $digit+
@intpart   = @digits
@fractpart = @digits
@mantpart  = @intpart?\.@fractpart|@intpart\.
@exppart   = [eE]\-?@digits
@suffix    = [fFlL]


tokens :-

-- whitespace (follows K&R A2.1) 
--
-- * horizontal and vertical tabs, newlines, and form feeds are filter out by
--   `Lexers.ctrlLexer' 
--
-- * comments are not handled, as we assume the input already went through cpp
--
$white+					;

-- #line directive (K&R A12.6)
--
-- * allows further ints after the file name a la GCC; as the GCC CPP docu
--   doesn't say how many ints there can be, we allow an unbound number
--
\#$space*@int$space*(\"$infname*\"$space*)?(@int$space*)*$eol	{ \pos len str -> setPos (adjustPos (take len str) pos) >> lexToken }

-- #pragma directive (K&R A12.8)
--
-- * we simply ignore any #pragma (but take care to update the position
--   information)
--
\#$space*pragma$anyButNL*$eol		;

-- #itent directive, eg used by rcs/cvs
--
-- * we simply ignore any #itent (but take care to update the position
--   information)
--
\#$space*ident$anyButNL*$eol		;

-- identifiers and keywords (follows K&R A2.3 and A2.4)
--
$letter($letter|$digit)*		{ \pos len str -> getNewName >>= \name ->
                                                          getTypedefs >>= \tdefs ->
                                                          return $ idkwtok pos (take len str) name tdefs }

-- constants (follows K&R A2.5) 
--
-- * K&R explicit mentions `enumeration-constants'; however, as they are
--   lexically identifiers, we do not have an extra case for them
--

-- integer constants (follows K&R A2.5.1)
--
0$octdigit*[uUlL]{0,2}			{ \pos len -> return . CTokILit pos . fst . head . readOct . take len }
$digitNZ$digit*[uUlL]{0,2}		{ \pos len -> return . CTokILit pos . fst . head . readDec . take len }
0[xX]$hexdigit*[uUlL]{0,2}		{ \pos len -> return . CTokILit pos . fst . head . readHex . drop 2 . take len }

-- character constants (follows K&R A2.5.2)
--
\'($inchar|@charesc)\'			{ \pos len -> return . CTokCLit pos . fst . oneChar . tail . take len }

-- float constants (follows K&R A2.5.3)
--
(@mantpart@exppart?|@intpart@exppart)@suffix?	{ \pos len -> return . CTokFLit pos . take len }

-- string literal (follows K&R A2.6)
--
\"($instr|@charesc)*\"			{ \pos len -> return . CTokSLit pos . normalizeEscapes . take len }


-- operators and separators
--
"("	{ token CTokLParen }
")"	{ token CTokRParen  }
"["	{ token CTokLBracket }
"]"	{ token CTokRBracket }
"->"	{ token CTokArrow }
"."	{ token CTokDot }
"!"	{ token CTokExclam }
"~"	{ token CTokTilde }
"++"	{ token CTokInc }
"--"	{ token CTokDec }
"+"	{ token CTokPlus }
"-"	{ token CTokMinus }
"*"	{ token CTokStar }
"/"	{ token CTokSlash }
"%"	{ token CTokPercent }
"&"	{ token CTokAmper }
"<<"	{ token CTokShiftL }
">>"	{ token CTokShiftR }
"<"	{ token CTokLess }
"<="	{ token CTokLessEq }
">"	{ token CTokHigh }
">="	{ token CTokHighEq }
"=="	{ token CTokEqual }
"!="	{ token CTokUnequal }
"^"	{ token CTokHat }
"|"	{ token CTokBar }
"&&"	{ token CTokAnd }
"||"	{ token CTokOr }
"?"	{ token CTokQuest }
":"	{ token CTokColon }
"="	{ token CTokAssign }
"+="	{ token CTokPlusAss }
"-="	{ token CTokMinusAss }
"*="	{ token CTokStarAss }
"/="	{ token CTokSlashAss }
"%="	{ token CTokPercAss }
"&="	{ token CTokAmpAss }
"^="	{ token CTokHatAss }
"|="	{ token CTokBarAss }
"<<="	{ token CTokSLAss }
">>="	{ token CTokSRAss }
","	{ token CTokComma }
\;	{ token CTokSemic }
"{"	{ token CTokLBrace }
"}"	{ token CTokRBrace }
"..."	{ token CTokEllipsis }


{

main = do
  contents <- getContents
--  let tokens = alexScanTokens contents
----  putStr $ unlines $ map show $ tokens
--  print (length tokens)
  let loop :: Int -> P Int
      loop n = do tok <- lexToken
                  case tok of
		       CTokEof -> return n
		       _ -> let n' = n + 1 
		             in n' `seq` loop n'
  let initialState = PState { 
	alex_pos = Position "<stdin>" 1 1,
	alex_inp = contents,
	alex_names = names rootSupply,
	alex_tdefs = Set.emptySet
      }
  case unP (loop 0) initialState of
    POk _ len -> print len
    PFailed message -> print message


-- token definition
-- ----------------

-- possible tokens (EXPORTED)
--
data CToken = CTokLParen   Position		-- `('
	    | CTokRParen   Position		-- `)'
	    | CTokLBracket Position		-- `['
	    | CTokRBracket Position		-- `]'
	    | CTokArrow	   Position		-- `->'
	    | CTokDot	   Position		-- `.'
	    | CTokExclam   Position		-- `!'
	    | CTokTilde	   Position		-- `~'
	    | CTokInc	   Position		-- `++'
	    | CTokDec	   Position		-- `--'
	    | CTokPlus	   Position		-- `+'
	    | CTokMinus	   Position		-- `-'
	    | CTokStar	   Position		-- `*'
	    | CTokSlash	   Position		-- `/'
	    | CTokPercent  Position		-- `%'
	    | CTokAmper	   Position		-- `&'
	    | CTokShiftL   Position		-- `<<'
	    | CTokShiftR   Position		-- `>>'
	    | CTokLess	   Position		-- `<'
	    | CTokLessEq   Position		-- `<='
	    | CTokHigh	   Position		-- `>'
	    | CTokHighEq   Position		-- `>='
	    | CTokEqual	   Position		-- `=='
	    | CTokUnequal  Position		-- `!='
	    | CTokHat	   Position		-- `^'
	    | CTokBar	   Position		-- `|'
	    | CTokAnd	   Position		-- `&&'
	    | CTokOr	   Position		-- `||'
	    | CTokQuest	   Position		-- `?'
	    | CTokColon	   Position		-- `:'
	    | CTokAssign   Position		-- `='
	    | CTokPlusAss  Position		-- `+='
	    | CTokMinusAss Position		-- `-='
	    | CTokStarAss  Position		-- `*='
	    | CTokSlashAss Position		-- `/='
	    | CTokPercAss  Position		-- `%='
	    | CTokAmpAss   Position		-- `&='
	    | CTokHatAss   Position		-- `^='
	    | CTokBarAss   Position		-- `|='
	    | CTokSLAss	   Position		-- `<<='
	    | CTokSRAss	   Position		-- `>>='
	    | CTokComma	   Position		-- `,'
	    | CTokSemic	   Position		-- `;'
	    | CTokLBrace   Position		-- `{'
	    | CTokRBrace   Position		--
	    | CTokEllipsis Position		-- `...'
	    | CTokAlignof  Position		-- `alignof' 
						-- (or `__alignof', 
						-- `__alignof__')
	    | CTokAuto     Position		-- `auto'
	    | CTokBreak    Position		-- `break'
	    | CTokCase     Position		-- `case'
	    | CTokChar     Position		-- `char'
	    | CTokConst    Position		-- `const' 
						-- (or `__const', `__const__')
	    | CTokContinue Position		-- `continue' 
	    | CTokDefault  Position		-- `default'
	    | CTokDo       Position		-- `do'
	    | CTokDouble   Position		-- `double'
	    | CTokElse     Position		-- `else'
	    | CTokEnum     Position		-- `enum'
	    | CTokExtern   Position		-- `extern'
 	    | CTokFloat    Position		-- `float'
 	    | CTokFor      Position		-- `for'
 	    | CTokGoto     Position		-- `goto'
 	    | CTokIf       Position		-- `if'
	    | CTokInline   Position		-- `inline'
						-- (or `__inline', 
						-- `__inline__')
	    | CTokInt      Position		-- `int'
	    | CTokLong     Position		-- `long'
	    | CTokRegister Position		-- `register'
	    | CTokRestrict Position		-- `restrict'
						-- (or `__restrict', 
						-- `__restrict__')
	    | CTokReturn   Position		-- `return'
	    | CTokShort    Position		-- `short'
	    | CTokSigned   Position		-- `signed'
						-- (or `__signed', 
						-- `__signed__')
	    | CTokSizeof   Position		-- `sizeof'
	    | CTokStatic   Position		-- `static'
	    | CTokStruct   Position		-- `struct'
	    | CTokSwitch   Position		-- `switch'
	    | CTokTypedef  Position		-- `typedef'
	    | CTokUnion    Position		-- `union'
	    | CTokUnsigned Position		-- `unsigned'
	    | CTokVoid     Position		-- `void'
	    | CTokVolatile Position		-- `volatile'
						-- (or `__volatile', 
						-- `__volatile__')
	    | CTokWhile    Position		-- `while'
	    | CTokCLit	   Position Char	-- character constant
	    | CTokILit	   Position Integer	-- integer constant
	    | CTokFLit	   Position String	-- float constant
	    | CTokSLit	   Position String	-- string constant (no escapes)
	    | CTokIdent	   Position Ident	-- identifier

	      -- not generated here, but in `CParser.parseCHeader'
	    | CTokTyIdent  Position Ident	-- `typedef-name' identifier
	    | CTokGnuC GnuCTok Position		-- special GNU C tokens
	    | CTokEof				-- end of file
  deriving Show

-- special tokens used in GNU C extensions to ANSI C
--
data GnuCTok = GnuCAttrTok		-- `__attribute__'
	     | GnuCExtTok		-- `__extension__'
	     deriving (Eq, Show)
-- TODO eliminate extension token, just ignore them

instance Pos CToken where
  posOf (CTokLParen   pos  ) = pos
  posOf (CTokRParen   pos  ) = pos
  posOf (CTokLBracket pos  ) = pos
  posOf (CTokRBracket pos  ) = pos
  posOf (CTokArrow    pos  ) = pos
  posOf (CTokDot      pos  ) = pos
  posOf (CTokExclam   pos  ) = pos
  posOf (CTokTilde    pos  ) = pos
  posOf (CTokInc      pos  ) = pos
  posOf (CTokDec      pos  ) = pos
  posOf (CTokPlus     pos  ) = pos
  posOf (CTokMinus    pos  ) = pos
  posOf (CTokStar     pos  ) = pos
  posOf (CTokSlash    pos  ) = pos
  posOf (CTokPercent  pos  ) = pos
  posOf (CTokAmper    pos  ) = pos
  posOf (CTokShiftL   pos  ) = pos
  posOf (CTokShiftR   pos  ) = pos
  posOf (CTokLess     pos  ) = pos
  posOf (CTokLessEq   pos  ) = pos
  posOf (CTokHigh     pos  ) = pos
  posOf (CTokHighEq   pos  ) = pos
  posOf (CTokEqual    pos  ) = pos
  posOf (CTokUnequal  pos  ) = pos
  posOf (CTokHat      pos  ) = pos
  posOf (CTokBar      pos  ) = pos
  posOf (CTokAnd      pos  ) = pos
  posOf (CTokOr	      pos  ) = pos
  posOf (CTokQuest    pos  ) = pos
  posOf (CTokColon    pos  ) = pos
  posOf (CTokAssign   pos  ) = pos
  posOf (CTokPlusAss  pos  ) = pos
  posOf (CTokMinusAss pos  ) = pos
  posOf (CTokStarAss  pos  ) = pos
  posOf (CTokSlashAss pos  ) = pos
  posOf (CTokPercAss  pos  ) = pos
  posOf (CTokAmpAss   pos  ) = pos
  posOf (CTokHatAss   pos  ) = pos
  posOf (CTokBarAss   pos  ) = pos
  posOf (CTokSLAss    pos  ) = pos
  posOf (CTokSRAss    pos  ) = pos
  posOf (CTokComma    pos  ) = pos
  posOf (CTokSemic    pos  ) = pos
  posOf (CTokLBrace   pos  ) = pos
  posOf (CTokRBrace   pos  ) = pos
  posOf (CTokEllipsis pos  ) = pos
  posOf (CTokAlignof  pos  ) = pos
  posOf (CTokAuto     pos  ) = pos
  posOf (CTokBreak    pos  ) = pos
  posOf (CTokCase     pos  ) = pos
  posOf (CTokChar     pos  ) = pos
  posOf (CTokConst    pos  ) = pos
  posOf (CTokContinue pos  ) = pos
  posOf (CTokDefault  pos  ) = pos
  posOf (CTokDo       pos  ) = pos
  posOf (CTokDouble   pos  ) = pos
  posOf (CTokElse     pos  ) = pos
  posOf (CTokEnum     pos  ) = pos
  posOf (CTokExtern   pos  ) = pos
  posOf (CTokFloat    pos  ) = pos
  posOf (CTokFor      pos  ) = pos
  posOf (CTokGoto     pos  ) = pos
  posOf (CTokInt      pos  ) = pos
  posOf (CTokInline   pos  ) = pos
  posOf (CTokIf       pos  ) = pos
  posOf (CTokLong     pos  ) = pos
  posOf (CTokRegister pos  ) = pos
  posOf (CTokRestrict pos  ) = pos
  posOf (CTokReturn   pos  ) = pos
  posOf (CTokShort    pos  ) = pos
  posOf (CTokSigned   pos  ) = pos
  posOf (CTokSizeof   pos  ) = pos
  posOf (CTokStatic   pos  ) = pos
  posOf (CTokStruct   pos  ) = pos
  posOf (CTokSwitch   pos  ) = pos
  posOf (CTokTypedef  pos  ) = pos
  posOf (CTokUnion    pos  ) = pos
  posOf (CTokUnsigned pos  ) = pos
  posOf (CTokVoid     pos  ) = pos
  posOf (CTokVolatile pos  ) = pos
  posOf (CTokWhile    pos  ) = pos
  posOf (CTokCLit     pos _) = pos
  posOf (CTokILit     pos _) = pos
  posOf (CTokFLit     pos _) = pos
  posOf (CTokSLit     pos _) = pos
  posOf (CTokIdent    pos _) = pos
  posOf (CTokTyIdent  pos _) = pos
  posOf (CTokGnuC   _ pos  ) = pos

{-
instance Show CToken where
  showsPrec _ (CTokLParen   _  ) = showString "("
  showsPrec _ (CTokRParen   _  ) = showString ")"
  showsPrec _ (CTokLBracket _  ) = showString "["
  showsPrec _ (CTokRBracket _  ) = showString "]"
  showsPrec _ (CTokArrow    _  ) = showString "->"
  showsPrec _ (CTokDot	    _  ) = showString "."
  showsPrec _ (CTokExclam   _  ) = showString "!"
  showsPrec _ (CTokTilde    _  ) = showString "~"
  showsPrec _ (CTokInc	    _  ) = showString "++"
  showsPrec _ (CTokDec	    _  ) = showString "--"
  showsPrec _ (CTokPlus	    _  ) = showString "+"
  showsPrec _ (CTokMinus    _  ) = showString "-"
  showsPrec _ (CTokStar	    _  ) = showString "*"
  showsPrec _ (CTokSlash    _  ) = showString "/"
  showsPrec _ (CTokPercent  _  ) = showString "%"
  showsPrec _ (CTokAmper    _  ) = showString "&"
  showsPrec _ (CTokShiftL   _  ) = showString "<<"
  showsPrec _ (CTokShiftR   _  ) = showString ">>"
  showsPrec _ (CTokLess	    _  ) = showString "<"
  showsPrec _ (CTokLessEq   _  ) = showString "<="
  showsPrec _ (CTokHigh	    _  ) = showString ">"
  showsPrec _ (CTokHighEq   _  ) = showString ">="
  showsPrec _ (CTokEqual    _  ) = showString "=="
  showsPrec _ (CTokUnequal  _  ) = showString "!="
  showsPrec _ (CTokHat	    _  ) = showString "^"
  showsPrec _ (CTokBar	    _  ) = showString "|"
  showsPrec _ (CTokAnd	    _  ) = showString "&&"
  showsPrec _ (CTokOr	    _  ) = showString "||"
  showsPrec _ (CTokQuest    _  ) = showString "?"
  showsPrec _ (CTokColon    _  ) = showString ":"
  showsPrec _ (CTokAssign   _  ) = showString "="
  showsPrec _ (CTokPlusAss  _  ) = showString "+="
  showsPrec _ (CTokMinusAss _  ) = showString "-="
  showsPrec _ (CTokStarAss  _  ) = showString "*="
  showsPrec _ (CTokSlashAss _  ) = showString "/="
  showsPrec _ (CTokPercAss  _  ) = showString "%="
  showsPrec _ (CTokAmpAss   _  ) = showString "&="
  showsPrec _ (CTokHatAss   _  ) = showString "^="
  showsPrec _ (CTokBarAss   _  ) = showString "|="
  showsPrec _ (CTokSLAss    _  ) = showString "<<="
  showsPrec _ (CTokSRAss    _  ) = showString ">>="
  showsPrec _ (CTokComma    _  ) = showString ","
  showsPrec _ (CTokSemic    _  ) = showString ";"
  showsPrec _ (CTokLBrace   _  ) = showString "{"
  showsPrec _ (CTokRBrace   _  ) = showString "}"
  showsPrec _ (CTokEllipsis _  ) = showString "..."
  showsPrec _ (CTokAlignof  _  ) = showString "alignof"
  showsPrec _ (CTokAuto     _  ) = showString "auto"
  showsPrec _ (CTokBreak    _  ) = showString "break"
  showsPrec _ (CTokCase     _  ) = showString "case"
  showsPrec _ (CTokChar     _  ) = showString "char"
  showsPrec _ (CTokConst    _  ) = showString "const"
  showsPrec _ (CTokContinue _  ) = showString "continue"
  showsPrec _ (CTokDefault  _  ) = showString "default"
  showsPrec _ (CTokDouble   _  ) = showString "double"
  showsPrec _ (CTokDo       _  ) = showString "do"
  showsPrec _ (CTokElse     _  ) = showString "else"
  showsPrec _ (CTokEnum     _  ) = showString "enum"
  showsPrec _ (CTokExtern   _  ) = showString "extern"
  showsPrec _ (CTokFloat    _  ) = showString "float"
  showsPrec _ (CTokFor      _  ) = showString "for"
  showsPrec _ (CTokGoto     _  ) = showString "goto"
  showsPrec _ (CTokIf       _  ) = showString "if"
  showsPrec _ (CTokInline   _  ) = showString "inline"
  showsPrec _ (CTokInt      _  ) = showString "int"
  showsPrec _ (CTokLong     _  ) = showString "long"
  showsPrec _ (CTokRegister _  ) = showString "register"
  showsPrec _ (CTokRestrict _  ) = showString "restrict"
  showsPrec _ (CTokReturn   _  ) = showString "return"
  showsPrec _ (CTokShort    _  ) = showString "short"
  showsPrec _ (CTokSigned   _  ) = showString "signed"
  showsPrec _ (CTokSizeof   _  ) = showString "sizeof"
  showsPrec _ (CTokStatic   _  ) = showString "static"
  showsPrec _ (CTokStruct   _  ) = showString "struct"
  showsPrec _ (CTokSwitch   _  ) = showString "switch"
  showsPrec _ (CTokTypedef  _  ) = showString "typedef"
  showsPrec _ (CTokUnion    _  ) = showString "union"
  showsPrec _ (CTokUnsigned _  ) = showString "unsigned"
  showsPrec _ (CTokVoid     _  ) = showString "void"
  showsPrec _ (CTokVolatile _  ) = showString "volatile"
  showsPrec _ (CTokWhile    _  ) = showString "while"
  showsPrec _ (CTokCLit     _ c) = showChar c
  showsPrec _ (CTokILit     _ i) = (showString . show) i
  showsPrec _ (CTokFLit     _ s) = showString s
  showsPrec _ (CTokSLit     _ s) = showString s
  showsPrec _ (CTokIdent    _ i) = (showString . identToLexeme) i
  showsPrec _ (CTokTypeName _ i) = (showString . identToLexeme) i
  showsPrec _ (CTokGnuC   g _  ) = case g of
				     GnuCAttrTok -> showString "__attribute__"
				     GnuCExtTok  -> showString "__extension__"
-}
{-
instance Tag CToken where
  tag CTokLParen   = 1
  tag CTokRParen   = 2
  tag CTokLBracket = 3
  tag CTokRBracket = 4
  tag CTokArrow    = 5
  tag CTokDot      = 6
  tag CTokExclam   = 7
  tag CTokTilde    = 8
  tag CTokInc      = 9
  tag CTokDec      = 10
  tag CTokPlus     = 11
  tag CTokMinus    = 12
  tag CTokStar     = 13
  tag CTokSlash    = 14
  tag CTokPercent  = 15
  tag CTokAmper    = 16
  tag CTokShiftL   = 17
  tag CTokShiftR   = 18
  tag CTokLess     = 19
  tag CTokLessEq   = 20
  tag CTokHigh     = 21
  tag CTokHighEq   = 22
  tag CTokEqual    = 23
  tag CTokUnequal  = 24
  tag CTokHat      = 25
  tag CTokBar      = 26
  tag CTokAnd      = 27
  tag CTokOr	   = 28
  tag CTokQuest    = 29
  tag CTokColon    = 30
  tag CTokAssign   = 31
  tag CTokPlusAss  = 32
  tag CTokMinusAss = 33
  tag CTokStarAss  = 34
  tag CTokSlashAss = 35
  tag CTokPercAss  = 36
  tag CTokAmpAss   = 37
  tag CTokHatAss   = 38
  tag CTokBarAss   = 39
  tag CTokSLAss    = 40
  tag CTokSRAss    = 41
  tag CTokComma    = 42
  tag CTokSemic    = 43
  tag CTokLBrace   = 44
  tag CTokRBrace   = 45
  tag CTokEllipsis = 46
  tag CTokAlignof  = 74
  tag CTokAuto     = 76
  tag CTokBreak    = 78
  tag CTokCase     = 79
  tag CTokChar     = 47
  tag CTokConst    = 48
  tag CTokContinue = 80
  tag CTokDefault  = 81
  tag CTokDo       = 82
  tag CTokDouble   = 49
  tag CTokElse     = 83
  tag CTokEnum     = 50
  tag CTokExtern   = 51
  tag CTokFloat    = 52
  tag CTokFor      = 84
  tag CTokGoto     = 85
  tag CTokIf       = 86
  tag CTokInline   = 75
  tag CTokInt      = 53
  tag CTokLong     = 54
  tag CTokRegister = 77
  tag CTokRestrict = 55
  tag CTokReturn   = 87
  tag CTokShort    = 56
  tag CTokSigned   = 57
  tag CTokSizeof   = 58
  tag CTokStatic   = 59
  tag CTokStruct   = 60
  tag CTokSwitch   = 88
  tag CTokTypedef  = 61
  tag CTokUnion    = 62
  tag CTokUnsigned = 63
  tag CTokVoid     = 64
  tag CTokVolatile = 65
  tag CTokWhile    = 89
  tag (CTokCLit _)  = 66
  tag (CTokILit _)  = 67
  tag (CTokFLit _)  = 68
  tag (CTokSLit _)  = 69
  tag (CTokIdent _) = 70
  tag (CTokTyIdent _) = 71
  tag (CTokGnuC GnuCAttrTok) = 72
  tag (CTokGnuC GnuCExtTok)  = 73
  -- current max is 89 (see `CTokWhile')
-}
  
idkwtok :: Position -> String -> Name -> Set Ident -> CToken
idkwtok pos ('a':'l':'i':'g':'n':'o':'f':[])			_ _  = CTokAlignof  pos
idkwtok pos ('_':'_':'a':'l':'i':'g':'n':'o':'f':[])		_ _  = CTokAlignof  pos
idkwtok pos ('_':'_':'a':'l':'i':'g':'n':'o':'f':'_':'_':[])	_ _  = CTokAlignof  pos
idkwtok pos ('a':'u':'t':'o':[])				_ _  = CTokAuto     pos
idkwtok pos ('b':'r':'e':'a':'k':[])				_ _  = CTokBreak    pos
idkwtok pos ('c':'a':'s':'e':[])				_ _  = CTokCase     pos
idkwtok pos ('c':'h':'a':'r':[])				_ _  = CTokChar     pos
idkwtok pos ('c':'o':'n':'s':'t':[])				_ _  = CTokConst    pos
idkwtok pos ('_':'_':'c':'o':'n':'s':'t':[])			_ _  = CTokConst    pos
idkwtok pos ('_':'_':'c':'o':'n':'s':'t':'_':'_':[])		_ _  = CTokConst    pos
idkwtok pos ('c':'o':'n':'t':'i':'n':'u':'e':[])		_ _  = CTokContinue pos
idkwtok pos ('d':'e':'f':'a':'u':'l':'t':[])			_ _  = CTokDefault  pos
idkwtok pos ('d':'o':[])					_ _  = CTokDo	    pos
idkwtok pos ('d':'o':'u':'b':'l':'e':[])			_ _  = CTokDouble   pos
idkwtok pos ('e':'l':'s':'e':[])				_ _  = CTokElse     pos
idkwtok pos ('e':'n':'u':'m':[])				_ _  = CTokEnum     pos
idkwtok pos ('e':'x':'t':'e':'r':'n':[])			_ _  = CTokExtern   pos
idkwtok pos ('f':'l':'o':'a':'t':[])				_ _  = CTokFloat    pos
idkwtok pos ('f':'o':'r':[])					_ _  = CTokFor	    pos
idkwtok pos ('g':'o':'t':'o':[])				_ _  = CTokGoto     pos
idkwtok pos ('i':'f':[])					_ _  = CTokIf	    pos
idkwtok pos ('i':'n':'l':'i':'n':'e':[])			_ _  = CTokInline   pos
idkwtok pos ('_':'_':'i':'n':'l':'i':'n':'e':[])		_ _  = CTokInline   pos
idkwtok pos ('_':'_':'i':'n':'l':'i':'n':'e':'_':'_':[])	_ _  = CTokInline   pos
idkwtok pos ('i':'n':'t':[])					_ _  = CTokInt      pos
idkwtok pos ('l':'o':'n':'g':[])				_ _  = CTokLong     pos
idkwtok pos ('r':'e':'g':'i':'s':'t':'e':'r':[])		_ _  = CTokRegister pos
idkwtok pos ('r':'e':'s':'t':'r':'i':'c':'t':[])		_ _  = CTokRestrict pos
idkwtok pos ('_':'_':'r':'e':'s':'t':'r':'i':'c':'t':[])	_ _  = CTokRestrict pos
idkwtok pos ('_':'_':'r':'e':'s':'t':'r':'i':'c':'t':'_':'_':[])_ _  = CTokRestrict pos
idkwtok pos ('r':'e':'t':'u':'r':'n':[])			_ _  = CTokReturn   pos
idkwtok pos ('s':'h':'o':'r':'t':[])				_ _  = CTokShort    pos
idkwtok pos ('s':'i':'g':'n':'e':'d':[])			_ _  = CTokSigned   pos
idkwtok pos ('_':'_':'s':'i':'g':'n':'e':'d':[])		_ _  = CTokSigned   pos
idkwtok pos ('_':'_':'s':'i':'g':'n':'e':'d':'_':'_':[])	_ _  = CTokSigned   pos
idkwtok pos ('s':'i':'z':'e':'o':'f':[])			_ _  = CTokSizeof   pos
idkwtok pos ('s':'t':'a':'t':'i':'c':[])			_ _  = CTokStatic   pos
idkwtok pos ('s':'t':'r':'u':'c':'t':[])			_ _  = CTokStruct   pos
idkwtok pos ('s':'w':'i':'t':'c':'h':[])			_ _  = CTokSwitch   pos
idkwtok pos ('t':'y':'p':'e':'d':'e':'f':[])			_ _  = CTokTypedef  pos
idkwtok pos ('u':'n':'i':'o':'n':[])				_ _  = CTokUnion    pos
idkwtok pos ('u':'n':'s':'i':'g':'n':'e':'d':[])		_ _  = CTokUnsigned pos
idkwtok pos ('v':'o':'i':'d':[])				_ _  = CTokVoid     pos
idkwtok pos ('v':'o':'l':'a':'t':'i':'l':'e':[])		_ _  = CTokVolatile pos
idkwtok pos ('_':'_':'v':'o':'l':'a':'t':'i':'l':'e':[])	_ _  = CTokVolatile pos
idkwtok pos ('_':'_':'v':'o':'l':'a':'t':'i':'l':'e':'_':'_':[])_ _  = CTokVolatile pos
idkwtok pos ('w':'h':'i':'l':'e':[])				_ _  = CTokWhile    pos
idkwtok pos ('_':'_':'a':'t':'t':'r':'i':'b':'u':'t':'e':'_':'_':[])	_ _  = CTokGnuC GnuCAttrTok pos
idkwtok pos ('_':'_':'e':'x':'t':'e':'n':'s':'i':'o':'n':'_':'_':[]) 	_ _  = CTokGnuC GnuCExtTok pos
idkwtok pos cs name tdefs 
       | ident `Set.elementOf` tdefs = CTokTyIdent  pos ident
       | otherwise                   = CTokIdent    pos ident
  where ident = lexemeToIdent pos cs name


-- converts the first character denotation of a C-style string to a character
-- and the remaining string
--
oneChar             :: String -> (Char, String)
oneChar ('\\':c:cs)  = case c of
			 'n'  -> ('\n', cs)
			 't'  -> ('\t', cs)
			 'v'  -> ('\v', cs)
			 'b'  -> ('\b', cs)
			 'r'  -> ('\r', cs)
			 'f'  -> ('\f', cs)
			 'a'  -> ('\a', cs)
			 '\\' -> ('\\', cs)
			 '?'  -> ('?', cs)
			 '\'' -> ('\'', cs)
			 '"'  -> ('"', cs)
			 'x'  -> let (i, cs') = (head . readHex) cs
				 in
				 (toEnum i, cs')
			 _    -> let (i, cs') = (head . readOct) (c:cs)
				 in
				 (toEnum i, cs')
oneChar (c   :cs)    = (c, cs)

normalizeEscapes [] = []
normalizeEscapes cs = let (c, cs') = oneChar cs
                       in c : normalizeEscapes cs'


-- -----------------------------------------------------------------------------
-- The input type


type AlexInput = (Position, 	-- current position,
		  String)	-- current input string

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar _ = error "alexInputPrevChar not used"

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (p,[]) = Nothing
alexGetChar (p,(c:s))  = let p' = alexMove p c in p' `seq`
                           Just (c, (p', s))

alexStartPos :: Position
alexStartPos = Position "" 1 1

alexMove :: Position -> Char -> Position
alexMove (Position f l c) '\t' = Position f  l    (((c+7) `div` 8)*8+1)
alexMove (Position f l c) '\n' = Position f (l+1)  1
alexMove (Position f l c) _    = Position f  l    (c+1)

{-
alexScanTokens :: String -> [CToken]
alexScanTokens str = go (alexStartPos,str)
  where go inp@(pos,str) =
	  case alexScan inp 0 of
		AlexEOF -> []
		AlexError _ -> error $ "lexical error at: " ++ show pos
		AlexSkip  inp' len     -> go inp'
		AlexToken inp' len act -> act pos len str : go inp'
-}

token :: (Position -> CToken) -> Position -> Int -> String -> P CToken
token tok pos _ _ = return (tok pos)


adjustPos :: String -> Position -> Position
adjustPos str (Position fname row _) = (Position fname' row' 0)
  where
    str'            = dropWhite . drop 1 $ str
    (rowStr, str'') = span isDigit str'
    row'		 = read rowStr
    str'''		 = dropWhite str''
    fnameStr	 = takeWhile (/= '"') . drop 1 $ str'''
    fname'		 = if (null str''' || head str''' /= '"')
		      then fname
		      else fnameStr --TODO check if same and share old value if possible
    --
    dropWhite = dropWhile (\c -> c == ' ' || c == '\t')


-- -----------------------------------------------------------------------------
-- The Parse Monad

data ParseResult a
  = POk !PState a
  | PFailed String		-- The error message

data PState = PState { 
	alex_pos :: !Position,	-- position at current input location
	alex_inp :: String,	-- the current input
	alex_names :: [Name],	-- the name unique supply
	alex_tdefs :: Set Ident	-- the set of typedef'ed identifiers
     }

newtype P a = P { unP :: PState -> ParseResult a }

instance Monad P where
  return = returnP
  (>>=) = thenP
  fail = failP

parse :: Monad m => P a -> [Ident] -> String -> m a
parse (P parser) builtins input =
  case parser initialState of
    POk _ result -> return result
    PFailed message -> fail message
  where initialState = PState {
          alex_pos = Position "" 1 1,
	  alex_inp = input,
	  alex_names = names rootSupply,
	  alex_tdefs = Set.mkSet builtins
        }

{-# INLINE returnP #-}
returnP :: a -> P a
returnP a = P $ \s -> POk s a

{-# INLINE thenP #-}
thenP :: P a -> (a -> P b) -> P b
(P m) `thenP` k = P $ \s ->
	case m s of
		POk s' a    -> (unP (k a)) s'
		PFailed err -> PFailed err

failP :: String -> P a
failP msg = P $ \_ -> PFailed msg

getNewName :: P Name
getNewName = P $ \s@PState{alex_names=(n:ns)} -> POk s{alex_names=ns} n

setPos :: Position -> P ()
setPos pos = P $ \s -> POk s{alex_pos=pos} ()

--debugging version:
--setPos :: Position -> P ()
--setPos p = returnP ()

getPos :: P Position
getPos = P $ \s@PState{alex_pos=pos} -> POk s pos

getTypedefs :: P (Set Ident)
getTypedefs = P $ \s@PState{alex_tdefs=tdefs} -> POk s tdefs

addTypedef :: Ident -> P ()
addTypedef ident = {-("addTypedef: " ++ show ident) `trace` -} (P $ \s@PState{alex_tdefs=tdefs} -> POk s{alex_tdefs = tdefs `Set.addToSet` ident} ())

getInput :: P AlexInput
getInput = P $ \s@PState{alex_pos=p, alex_inp=i} -> POk s (p,i)

setInput :: AlexInput -> P ()
setInput (p,i) = P $ \s -> POk s{alex_pos=p, alex_inp=i} ()

lexToken :: P CToken
lexToken = do
  inp@(pos, str) <- getInput
  case alexScan inp 0 of
    AlexEOF -> return CTokEof
    AlexError inp' -> fail $ "lexical error at: " ++ show pos
    AlexSkip  inp' len -> do
	setInput inp'
	lexToken
    AlexToken inp' len action -> do
	setInput inp'
	action pos len str

lexC :: (CToken -> P a) -> P a
lexC cont = do
  tok <- lexToken
  cont tok
}
