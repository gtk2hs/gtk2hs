-- The purpose of this scanner is to extract only the necessary bits
-- to generate documentation. We therefore skip function bodies and most
-- other things which are not top-level. The scanner is stateful, i.e. it
-- differentiates between top-level, in-definition and comments.
module Lexer(Token(..), Loc, lexer) where

import Abstract(Loc)
import State(SymKind(..), ConKind(..))
import Data.Char
import Data.PackedString

type Lexer = String -> [Token]

lexer :: Lexer
lexer str = lexToplevel str

getErrors :: [Token] -> [Token]
getErrors [] = []
getErrors ((all@(Error _)):xs) = all:getErrors xs
getErrors (x:xs) = getErrors xs


data Token
  = NewLine
  | TLComment
  | TLImport
  | TLType ConKind
  | TLInstance
  | TLClass
  -- Token for commentary
  | ComHook
  | ComWord PackedString
  | ComBullet
  | ComEnd
  -- Token for hooks
  | HookArg
  | HookSymbol SymKind
  | HookType ConKind
  | HookVariant		-- a data constructor
  | HookDescription
  | HookSynopsis
  | HookImplementation
  | HookLiteral Bool PackedString -- true if multiline
  | HookWidget
  | HookModule
  | HookRef
  | HookEnd
  | HookFollow PackedString
  -- Expressions for definitions
  | DefFunArrow
  | DefClArrow
  | DefParRoundOp
  | DefParRoundCl
  | DefParSquareOp
  | DefParSquareCl
  | DefParCurlyOp
  | DefParCurlyCl
  | DefEquals
  | DefComma
  | DefDoubleColon
  | DefCons
  | DefAt
  | DefUnderscore
  | DefMid
  | DefDeriving
  | DefVar PackedString
  | DefCon PackedString
  | DefEnd
  -- Lexical error
  | Error PackedString
  deriving (Eq)

instance Show Token where
  show (NewLine)        = "\\n"
  show (TLComment)	= "-- "
  show (TLImport)	= "import "
  show (TLType Data)  = "data "
  show (TLType Newtype)= "newtype "
  show (TLType Type)  = "type "
  show (TLInstance)	= "instance "
  show (TLClass)	= "class "
  show (ComHook)	= "@"
  show (ComWord str)	= "'"++unpackPS str++"' "
  show (ComBullet)	= "* "
  show (ComEnd)	= "<EOComm>"
  show (HookArg)	= "arg "
  show (HookSymbol Constructor) = "constructor "
  show (HookSymbol Method) = "method "
  show (HookSymbol Function) = "function "
  show (HookSymbol Constant) = "constant "
  show (HookSymbol Signal) = "signal "
  show (HookType Data)  = "data "
  show (HookType Newtype)= "newtype "
  show (HookType Type)  = "type "
  show (HookVariant)	= "variant "
  show (HookDescription)= "description"
  show (HookSynopsis)   = "synopsis"
  show (HookImplementation) = "implementation"
  show (HookLiteral True _)  = "verbatim"
  show (HookLiteral False _) = "literal"
  show (HookWidget)	= "widget "
  show (HookModule)	= "module "
  show (HookRef)	= "ref "
  show (HookEnd)	= "@ "
  show (HookFollow str) = unpackPS str++"' "
  show (DefFunArrow)	= "-> " 
  show (DefClArrow)	= "=> "
  show (DefParRoundOp)	= "("
  show (DefParRoundCl)	= ")"
  show (DefParSquareOp)	= "["
  show (DefParSquareCl)	= "]"
  show (DefParCurlyOp)	= "{"
  show (DefParCurlyCl)	= "}"
  show (DefEquals)	= "="
  show (DefComma)	= ", "
  show (DefDoubleColon)	= " :: "
  show (DefCons)	= ";"
  show (DefAt)		= "@"
  show (DefUnderscore)	= "_"
  show (DefMid)		= "|"
  show (DefDeriving)	= " deriving "
  show (DefVar var)	= "'"++unpackPS var++"' "
  show (DefCon con)	= "'"++unpackPS con++"' "
  show (DefEnd)		= "<EODef>"
  show (Error _)	= "<lexer error>"


data StopInfo
  = SIHook
  | SIImport
  | SIData
  | SIFunction
  | SIInstance
  | SIRecord StopInfo
  deriving Eq

-- Determine the kind of a toplevel entity.
lexToplevel :: Lexer
lexToplevel [] = []
lexToplevel ('-':'-':xs) = TLComment: lexComment xs
lexToplevel ('{':'-':xs) = dropComment 1 xs
lexToplevel ('#':xs) = lexToplevel (skipTillEOL xs)
lexToplevel ('d':'a':'t':'a':' ':xs) = TLType Data:lexDefn SIData xs
lexToplevel ('t':'y':'p':'e':' ':xs) = TLType Type:lexDefn SIData xs
lexToplevel ('n':'e':'w':'t':'y':'p':'e':' ':xs) = TLType Newtype:
						   lexDefn SIData xs 
lexToplevel ('i':'m':'p':'o':'r':'t':' ':xs) = TLImport:
					       lexDefn SIImport xs
lexToplevel ('i':'n':'s':'t':'a':'n':'c':'e':' ':xs) = TLInstance:
						       lexDefn SIInstance xs
lexToplevel ('c':'l':'a':'s':'s':' ':xs) = TLClass:lexDefn SIInstance xs
lexToplevel ('m':'o':'d':'u':'l':'e':' ':xs) = lexToplevel (skipTillEOL xs)
lexToplevel ('f':'o':'r':'e':'i':'g':'n':' ':xs) = lexToplevel (skipTillEOL xs)
lexToplevel ('\n':xs) = NewLine: lexToplevel xs
lexToplevel (' ':xs) = lexToplevel (skipTillEOL xs)
lexToplevel ('\t':xs) = lexToplevel (skipTillEOL xs)
lexToplevel xs = lexDefn SIFunction xs

-- Skip till EOL.
skipTillEOL :: String -> String
skipTillEOL = dropWhile (/='\n')

-- Skip all blank space without newline.
skipBlank :: String -> String
skipBlank = dropWhile (`elem` " \t")

-- Discard a multiline comment
dropComment :: Int -> Lexer
dropComment 1 ('-':'}':xs) = lexToplevel xs
dropComment n ('-':'}':xs) = dropComment (n-1) xs
dropComment n ('\n':xs) = NewLine: dropComment n xs
dropComment n (x:xs) = dropComment n xs
dropComment n [] = illChar "multiline comment" []

-- Read a single line comment honouring the contained hooks.
lexComment :: Lexer
lexComment ('\n':'-':'-':' ':'*':xs) = NewLine: ComBullet: lexComment xs
lexComment ('\n':'-':'-':xs) = NewLine: lexComment xs
lexComment ('\n':xs) = ComEnd : NewLine: lexToplevel xs
lexComment ('@':xs) = ComHook:lexHook xs
lexComment ('-':'-':'-':xs) = case dropWhile (=='-') xs of
  (all@('\n':_)) -> lexComment all
  all -> illegal ("too many dashes within comment") all 
lexComment (' ':xs) = lexComment xs
lexComment ('\t':xs) = lexComment xs
lexComment (x:xs) | isPrint x = 
  let (remWord, remProg) = span (`notElem` " @\n") xs in
  ComWord (packString (x:remWord)) : lexComment remProg
lexComment xs = illChar "comment" xs


-- Read the interior of the hook.
lexHook :: Lexer
lexHook (' ':xs) = lexHook xs
lexHook ('\t':xs) = lexHook xs
lexHook ('@':xs) = HookEnd: lexFollow xs
lexHook ('a':'r':'g':xs) = HookArg: lexDefn SIHook xs
lexHook ('c':'o':'n':'s':'t':'a':'n':'t':xs) = 
  HookSymbol Constant: lexDefn SIHook xs
lexHook ('c':'o':'n':'s':'t':'r':'u':'c':'t':'o':'r':xs) = 
  HookSymbol Constructor: lexDefn SIHook xs
lexHook ('d':'a':'t':'a':xs) = HookType Data: lexDefn SIHook xs
lexHook ('d':'u':'n':'n':'o':xs) = lexHook xs
lexHook ('f':'u':'n':'c':'t':'i':'o':'n':xs) = HookSymbol Function: 
					       lexDefn SIHook xs
lexHook ('t':'y':'p':'e':xs) = HookType Data: lexDefn SIHook xs
lexHook ('n':'e':'w':'t':'y':'p':'e':xs) = 
  HookType Data: lexDefn SIHook xs
lexHook ('d':'o':'c':'u':'m':'e':'n':'t':'a':'t':'i':'o':'n':xs) = 
  HookDescription: lexHook xs
lexHook ('t':'o':'d':'o':xs) = HookImplementation: lexHook xs
lexHook ('p':'r':'o':'g':' ':xs) = lexLiteral True xs
lexHook ('p':'r':'o':'g':xs) = lexLiteral True xs
lexHook ('v':'e':'r':'b':'a':'t':'i':'m':' ':xs) = lexLiteral True xs
lexHook ('v':'e':'r':'b':'a':'t':'i':'m':xs) = lexLiteral True xs
lexHook ('l':'i':'t':'e':'r':'a':'l':' ':xs) = lexLiteral False xs
lexHook ('l':'i':'t':'e':'r':'a':'l':xs) = lexLiteral False xs
lexHook ('m':'e':'t':'h':'o':'d':xs) = 
  HookSymbol Method: lexDefn SIHook xs
lexHook ('e':'n':'t':'r':'y':' ':'W':'i':'d':'g':'e':'t':xs) =
  HookWidget: lexDefn SIHook xs
lexHook ('e':'n':'t':'r':'y':xs) = HookModule: lexDefn SIHook xs
lexHook ('r':'e':'f':xs) = HookRef: lexHook xs
-- backwards compatibility:
lexHook ('s':'i':'g':'n':'a':'l':' ':'c':'o':'n':'n':'e':'c':'t':'T':'o':xs) = 
  HookSymbol Signal: lexDefn SIHook ('o':'n':xs)
lexHook ('s':'i':'g':'n':'a':'l':xs) = 
  HookSymbol Signal: lexDefn SIHook xs
lexHook ('v':'a':'r':'i':'a':'n':'t':xs) = HookVariant: lexDefn SIHook xs
lexHook ('d':'e':'s':'c':'r':'i':'p':'t':'i':'o':'n':xs) = 
  HookSynopsis: lexDefn SIHook xs
lexHook xs = illChar "doc hook" xs

-- Try to read immediate text after a hook.
lexFollow :: Lexer
lexFollow xs = case span (\x -> isPrint x && not (isSpace x)) xs of
  ("",rem) -> lexComment rem
  (fol,rem) -> HookFollow (packString fol): lexComment rem

-- Scan anything which resembles a definition, declaration, etc.
lexDefn :: StopInfo -> Lexer
lexDefn si = lD
  where
    lD :: Lexer
    lD (' ':xs) = lD xs
    lD ('\t':xs) = lD xs
    lD ('\n':' ':xs) = NewLine: lD xs
    lD ('\n':'\t':xs) = NewLine: lD xs
    lD ('\n':xs) = DefEnd: NewLine: lexToplevel xs
    lD ('-':'-':xs) = lD (skipTillEOL xs)
    lD ('-':'>':xs) = DefFunArrow: lD xs
    lD ('=':'>':xs) = DefClArrow: lD xs
    lD ('(':xs) | si==SIImport = DefEnd: lexToplevel (skipTillEOL xs)
		| otherwise = DefParRoundOp: lD xs
    lD (')':xs) = DefParRoundCl: lD xs
    lD ('[':xs) = DefParSquareOp: lD xs
    lD (']':xs) = DefParSquareCl: lD xs
    lD ('{':xs) = DefParCurlyOp: lexDefn (SIRecord si) xs
    lD ('}':xs) = DefParCurlyCl: case si of
      (SIRecord oldsi) -> lexDefn oldsi xs
      _		       -> lD xs --shouldn't happen
    lD ('=':xs) | si==SIFunction = DefEnd: lexToplevel (skipTillEOL xs)
		| otherwise = DefEquals: lD xs
    lD (',':xs) = DefComma: lD xs
    lD (':':':':xs) = DefDoubleColon: lD xs
    lD (':':xs) = DefCons: lD xs
    lD ('@':xs) | si==SIHook = HookEnd: lexFollow xs
		| otherwise = DefAt: lD xs
    lD ('_':xs) = DefUnderscore: lD xs
    lD ('|':xs) | si==SIFunction = DefEnd: lexToplevel (skipTillEOL xs)
		| otherwise = DefMid: lD xs
    lD ('w':'h':'e':'r':'e':xs) = DefEnd: lexToplevel (skipTillEOL xs)
    lD ('d':'e':'r':'i':'v':'i':'n':'g':xs) = DefDeriving: lD xs
    lD ('h':'i':'d':'i':'n':'g':xs)
       | si==SIImport = DefEnd: lexToplevel (skipTillEOL xs)
    lD ('q':'u':'a':'l':'i':'f':'i':'e':'d':xs) | si==SIImport = lD xs
    lD (x:xs)	| isLower x = lexName DefVar [x] xs
		| isUpper x = lexName DefCon [x] xs
    lD xs = illChar "declaration" xs

    lexName :: (PackedString -> Token) -> String -> Lexer
    lexName con = lN
      where
	lN var []		     = illChar "name" []
        lN var (x:xs) | isAlphaNum x = lN (x:var) xs
		      | x=='\''	     = lN (x:var) xs
		      | x=='_'	     = lN (x:var) xs
		      | otherwise    = con (packString (reverse var)):
				       lD (x:xs)

-- Extract verbatim text.
--
-- The boolean is true if we should return Verbatim instead of Literal.
lexLiteral :: Bool -> Lexer
lexLiteral ty = lL 0 ""
  where
    lL :: Int -> String -> Lexer
    lL newlines txt ('@':'@':xs) = lL newlines ('@':txt) xs
    lL newlines txt ('@':xs) = HookLiteral ty (packString (reverse txt)):
      HookEnd:replicate newlines NewLine++lexFollow xs
    lL newlines txt ('\n':'-':'-':' ':xs) = lL (newlines+1) ('\n':txt) xs
    lL newlines txt ('\n':'-':'-':xs) = lL (newlines+1) ('\n':txt) xs
    lL newlines txt xs@('\n':_) = illChar "verbatim program" xs
    lL newlines txt (x:xs) = lL newlines (x:txt) xs
    lL newlines txt [] = illChar "verbatim program" []

-- Report an illegal character.
illChar :: String -> Lexer
illChar context "" = illegal 
  ("unexpected end of file in "++context++".") ""
illChar context (xs) = illegal  
  ("Syntax error in "++context++": "++show (take 10 xs)++"...") (tail xs)

illegal :: String -> Lexer
illegal context xs = Error (packString context): lexToplevel (skipTillEOL xs)




