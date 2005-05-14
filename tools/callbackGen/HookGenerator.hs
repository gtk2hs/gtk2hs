{-# OPTIONS -cpp #-}
-- HookGenerator.hs -*-haskell-*-
-- Takes a type list of possible hooks from the GTK+ distribution and produces
-- Haskell functions to connect to these callbacks.
module Main(main) where

#include "gtk2hs-config.h"

import Char(showLitChar)
import List(nub, partition)
import Maybe(catMaybes)
import System(getArgs, exitWith, ExitCode(..))

-- Define all possible data types the GTK will supply in callbacks.
--
data Types = Tunit		-- ()
	   | Tbool		-- Bool
	   | Tchar
	   | Tuchar
	   | Tint		-- Int
	   | Tuint
	   | Tlong
	   | Tulong
	   | Tenum
	   | Tflags
	   | Tfloat
	   | Tdouble
	   | Tstring
	   | Tboxed  		-- a struct which is passed by value
	   | Tptr		-- pointer
	   | Tobject		-- foreign with GObjectClass context
	   deriving Eq

type Signature = (Types,[Types])
type Signatures = [Signature]

-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------
     
parseSignatures :: String -> Signatures
parseSignatures content = (nub.parseSig 1.scan) content

data Token = TokColon
	   | TokType Types
	   | TokComma
	   | TokEOL

instance Show Token where
  showsPrec _ TokColon = shows ":"
  showsPrec _ (TokType _) = shows "<type>"
  showsPrec _ TokComma = shows ","
  showsPrec _ TokEOL = shows "<EOL>"

parseSig :: Int -> [Token] -> Signatures
parseSig l [] = []
parseSig l (TokEOL: rem) = parseSig (l+1) rem
parseSig l (TokType ret: TokColon: TokType Tunit:rem) =
  (ret,[]):parseSig l rem
parseSig l (TokType ret: TokColon: rem) =
  let (args,rem') = parseArg l rem in
  (ret,args): parseSig (l+1) rem'
parseSig l rem = error ("parse error on line "++show l++
		       ": expected type and colon, found\n"++
		       concatMap show (take 5 rem))

parseArg :: Int -> [Token] -> ([Types],[Token])
parseArg l [TokType ty] = ([ty],[])
parseArg l (TokType ty: TokEOL:rem) = ([ty],rem)
parseArg l (TokType ty: TokComma:rem) =
  let (args,rem') = parseArg l rem in
  (ty:args, rem')
parseArg l rem = error ("parse error on line "++show l++": expected type"++
                        " followed by comma or EOL, found\n "++ 
		       concatMap show (take 5 rem))

scan :: String -> [Token]
scan "" = []
scan ('#':xs) = (scan.dropWhile (/='\n'))  xs
scan ('\n':xs) = TokEOL:scan xs
scan (' ':xs) = scan xs
scan ('\t':xs) = scan xs
scan (':':xs) = TokColon:scan xs
scan (',':xs) = TokComma:scan xs
scan ('V':'O':'I':'D':xs) = TokType Tunit:scan xs
scan ('B':'O':'O':'L':'E':'A':'N':xs) = TokType Tbool:scan xs
scan ('C':'H':'A':'R':xs) = TokType Tchar:scan xs
scan ('U':'C':'H':'A':'R':xs) = TokType Tuchar:scan xs
scan ('I':'N':'T':xs) = TokType Tint:scan xs
scan ('U':'I':'N':'T':xs) = TokType Tuint:scan xs
scan ('L':'O':'N':'G':xs) = TokType Tuint:scan xs
scan ('U':'L':'O':'N':'G':xs) = TokType Tulong:scan xs
scan ('E':'N':'U':'M':xs) = TokType Tenum:scan xs
scan ('F':'L':'A':'G':'S':xs) = TokType Tflags:scan xs
scan ('F':'L':'O':'A':'T':xs) = TokType Tfloat:scan xs
scan ('D':'O':'U':'B':'L':'E':xs) = TokType Tdouble:scan xs
scan ('S':'T':'R':'I':'N':'G':xs) = TokType Tstring:scan xs
scan ('B':'O':'X':'E':'D':xs) = TokType Tboxed:scan xs
scan ('P':'O':'I':'N':'T':'E':'R':xs) = TokType Tptr:scan xs
scan ('O':'B':'J':'E':'C':'T':xs) = TokType Tobject:scan xs
scan ('N':'O':'N':'E':xs) = TokType Tunit:scan xs
scan ('B':'O':'O':'L':xs) = TokType Tbool:scan xs
scan str = error ("Invalid character in input file:\n"++
	   concatMap ((flip showLitChar) "") (take 5 str))


-------------------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------------------

ss = showString
sc = showChar

indent :: Int -> ShowS
indent c = ss ("\n"++replicate (2*c) ' ')

-------------------------------------------------------------------------------
-- Tables of code fragments
-------------------------------------------------------------------------------

identifier :: Types -> ShowS
identifier Tunit    = ss "NONE"
identifier Tbool    = ss "BOOL"
identifier Tchar    = ss "CHAR"
identifier Tuchar   = ss "UCHAR"
identifier Tint	    = ss "INT"
identifier Tuint    = ss "WORD"
identifier Tlong    = ss "LONG"
identifier Tulong   = ss "ULONG"
identifier Tenum    = ss "ENUM"
identifier Tflags   = ss "FLAGS"
identifier Tfloat   = ss "FLOAT"
identifier Tdouble  = ss "DOUBLE"
identifier Tstring  = ss "STRING"
identifier Tboxed   = ss "BOXED"
identifier Tptr	    = ss "PTR"
identifier Tobject  = ss "OBJECT"

#ifdef USE_GCLOSUE_SIGNALS_IMPL

-- The monomorphic type which is used to export the function signature.
rawtype :: Types -> ShowS
rawtype Tunit    = ss "()"
rawtype Tbool    = ss "Bool"
rawtype Tchar    = ss "Char"
rawtype Tuchar   = ss "Char"
rawtype Tint	 = ss "Int"
rawtype Tuint    = ss "Word"
rawtype Tlong    = ss "Int"
rawtype Tulong   = ss "Word"
rawtype Tenum    = ss "Int"
rawtype Tflags   = ss "Word"
rawtype Tfloat   = ss "Float"
rawtype Tdouble  = ss "Double"
rawtype Tstring  = ss "CString"
rawtype Tboxed   = ss "Ptr ()"
rawtype Tptr	 = ss "Ptr ()"
rawtype Tobject  = ss "Ptr GObject"


#else

-- The monomorphic type which is used to export the function signature.
rawtype :: Types -> ShowS
rawtype Tunit    = ss "()"
rawtype Tbool    = ss "{#type gboolean#}"
rawtype Tchar    = ss "{#type gchar#}"
rawtype Tuchar   = ss "{#type guchar#}"
rawtype Tint	 = ss "{#type gint#}"
rawtype Tuint    = ss "{#type guint#}"
rawtype Tlong    = ss "{#type glong#}"
rawtype Tulong   = ss "{#type gulong#}"
rawtype Tenum    = ss "{#type gint#}"
rawtype Tflags   = ss "{#type guint#}"
rawtype Tfloat   = ss "{#type gfloat#}"
rawtype Tdouble  = ss "{#type gdouble#}"
rawtype Tstring  = ss "CString"
rawtype Tboxed   = ss "Ptr ()"
rawtype Tptr	 = ss "Ptr ()"
rawtype Tobject  = ss "Ptr GObject"

#endif

-- The possibly polymorphic type which 
usertype :: Types -> [Char] -> (ShowS,[Char])
usertype Tunit	  cs = (ss "()",cs)
usertype Tbool	  (c:cs) = (ss "Bool",cs)
usertype Tchar	  (c:cs) = (ss "Char",cs)
usertype Tuchar	  (c:cs) = (ss "Char",cs)
usertype Tint	  (c:cs) = (ss "Int",cs) 
usertype Tuint	  (c:cs) = (ss "Word",cs)
usertype Tlong	  (c:cs) = (ss "Int",cs)
usertype Tulong	  (c:cs) = (ss "Int",cs)
usertype Tenum	  (c:cs) = (sc c,cs)
usertype Tflags   cs = usertype Tenum cs
usertype Tfloat	  (c:cs) = (ss "Float",cs)
usertype Tdouble  (c:cs) = (ss "Double",cs)
usertype Tstring  (c:cs) = (ss "String",cs)
usertype Tboxed   (c:cs) = (sc c,cs)
usertype Tptr	  (c:cs) = (ss "Ptr ".sc c,cs)
usertype Tobject  (c:cs) = (sc c.sc '\'',cs)


-- type declaration: only consume variables when they are needed
--
-- * Tint is used as return value as well. Therefore Integral has to be added
--   to the context. Grrr.
--
context :: [Types] -> [Char] -> [ShowS]
context (Tenum:ts)    (c:cs) = ss "Enum ".sc c: context ts cs
context (Tflags:ts)   (c:cs) = ss "Flags ".sc c: context ts cs
context (Tobject:ts)  (c:cs) = ss "GObjectClass ".sc c.sc '\'': context ts cs
context (_:ts)	      (c:cs) = context ts cs
context []	      _	     = []


marshType :: [Types] -> [Char] -> [ShowS]
marshType (Tint:ts)     (c:cs) = marshType ts cs
marshType (Tuint:ts)	(c:cs) = marshType ts cs
marshType (Tenum:ts)	(c:cs) = marshType ts cs
marshType (Tflags:ts)	cs     = marshType (Tenum:ts) cs
marshType (Tboxed:ts)	(c:cs) = ss "(Ptr ".sc c.ss " -> IO ".
				 sc c.ss ") -> ":
				 marshType ts cs
marshType (Tptr:ts)	(c:cs) = marshType ts cs
marshType (Tobject:ts)	(c:cs) = marshType ts cs
marshType (_:ts)        (c:cs) = marshType ts cs
marshType []		_      = []

-- arguments for user defined marshalling

type ArgNo = Int

marshArg :: Types -> ArgNo -> ShowS
marshArg Tboxed   c = ss "boxedPre".shows c.sc ' '
marshArg _	  _ = id

-- generate a name for every passed argument,
nameArg :: Types -> ArgNo -> ShowS
nameArg Tunit	 _ = id
nameArg Tbool	 c = ss "bool".shows c
nameArg Tchar    c = ss "char".shows c
nameArg Tuchar   c = ss "char".shows c
nameArg Tint	 c = ss "int".shows c
nameArg Tuint	 c = ss "int".shows c
nameArg Tlong	 c = ss "long".shows c
nameArg Tulong	 c = ss "long".shows c
nameArg Tenum	 c = ss "enum".shows c
nameArg Tflags	 c = ss "flags".shows c
nameArg Tfloat	 c = ss "float".shows c
nameArg Tdouble	 c = ss "double".shows c
nameArg Tstring	 c = ss "str".shows c
nameArg Tboxed   c = ss "box".shows c
nameArg Tptr     c = ss "ptr".shows c
nameArg Tobject  c = ss "obj".shows c


-- describe marshalling between the data passed from the registered function
-- to the user supplied Haskell function

#ifdef USE_GCLOSUE_SIGNALS_IMPL

marshExec :: Types -> ShowS -> Int -> (ShowS -> ShowS)
marshExec Tbool	  arg _ body = body. sc ' '. arg
marshExec Tchar	  arg _ body = body. sc ' '. arg
marshExec Tuchar  arg _ body = body. sc ' '. arg
marshExec Tint	  arg _ body = body. sc ' '. arg
marshExec Tuint	  arg _ body = body. sc ' '. arg
marshExec Tlong	  arg _ body = body. sc ' '. arg
marshExec Tulong  arg _ body = body. sc ' '. arg
marshExec Tenum	  arg _ body = body. ss " (toEnum ". arg. sc ')'
marshExec Tflags  arg _ body = body. ss " (toFlags ". arg. sc ')'
marshExec Tfloat  arg _ body = body. sc ' '. arg
marshExec Tdouble arg _ body = body. sc ' '. arg
marshExec Tstring arg _ body = indent 5. ss "peekUTFString ". arg. ss " >>= \\". arg. ss "\' ->".
                               body. sc ' '. arg. sc '\''
marshExec Tboxed  arg n body = indent 5. ss "boxedPre". ss (show n). ss " (castPtr ". arg. ss ") >>= \\". arg. ss "\' ->".
                               body. sc ' '. arg. sc '\''
marshExec Tptr	  arg _ body = body. ss " (castPtr ". arg. sc ')'
marshExec Tobject arg _ body = indent 5.ss "makeNewGObject mkGObject (return ". arg. ss ") >>= \\". arg. ss "\' ->".
                               body. ss " (fromGObject ". arg. ss "\')"

marshRet :: Types -> (ShowS -> ShowS)
marshRet Tunit	 body = body
marshRet Tbool	 body = body
marshRet Tint	 body = body
marshRet Tuint	 body = body
marshRet Tlong	 body = body
marshRet Tulong	 body = body
marshRet Tenum	 body = indent 5. ss "liftM fromEnum $ ". body
marshRet Tflags	 body = indent 5. ss "liftM fromFlags $ ". body
marshRet Tfloat	 body = body
marshRet Tdouble body = body
marshRet Tstring body = body. indent 5. ss ">>= newUTFString"
marshRet _       _    = error "Signal handlers cannot return structured types."

#else

marshExec :: Types -> ArgNo -> ShowS
marshExec Tbool	  n = indent 4.ss "let bool".shows n.
		      ss "' = toBool bool".shows n
marshExec Tchar	  n = indent 4.ss "let char".shows n.
		      ss "' = (toEnum.fromEnum) char".shows n
marshExec Tuchar  n = indent 4.ss "let char".shows n.
		      ss "' = (toEnum.fromEnum) char".shows n
marshExec Tint	  n = indent 4.ss "let int".shows n.
		      ss "' = fromIntegral int".shows n
marshExec Tuint	  n = indent 4.ss "let int".shows n.
		      ss "' = fromIntegral int".shows n
marshExec Tlong	  n = indent 4.ss "let long".shows n.
		      ss "' = toInteger long".shows n
marshExec Tulong  n = indent 4.ss "let long".shows n.
		      ss "' = toInteger long".shows n
marshExec Tenum	  n = indent 4.ss "let enum".shows n.
		      ss "' = (toEnum.fromEnum) enum".shows n
marshExec Tflags  n = indent 4.ss "let flags".shows n.
		      ss "' = (toEnum.fromEnum) flags".shows n
marshExec Tfloat  n = indent 4.ss "let float".shows n.
		      ss "' = (fromRational.toRational) float".shows n
marshExec Tdouble n = indent 4.ss "let double".shows n.
		      ss "' = (fromRational.toRational) double".shows n
marshExec Tstring n = indent 4.ss "str".shows n.
		      ss "' <- peekCString str".shows n
marshExec Tboxed  n = indent 4.ss "box".shows n.ss "' <- boxedPre".
		      shows n.ss " $ castPtr box".shows n
marshExec Tptr	  n = indent 4.ss "let ptr".shows n.ss "' = castPtr ptr".
		      shows n
marshExec Tobject n = indent 4.ss "objectRef obj".shows n.
		      indent 4.ss "obj".shows n.
		      ss "' <- liftM (fromGObject.mkGObject) $".
		      indent 5.ss "newForeignPtr obj".shows n.
		      ss " (objectUnref obj".shows n.sc ')'
marshExec _	  _ = id

marshRet :: Types -> ShowS
marshRet Tunit	  = ss "id"
marshRet Tbool	  = ss "fromBool"
marshRet Tint	  = ss "fromIntegral"
marshRet Tuint	  = ss "fromIntegral"
marshRet Tlong	  = ss "fromIntegral"
marshRet Tulong	  = ss "fromIntegral"
marshRet Tenum	  = ss "(toEnum.fromEnum)"
marshRet Tflags	  = ss "fromFlags"
marshRet Tfloat	  = ss "(toRational.fromRational)"
marshRet Tdouble  = ss "(toRational.fromRational)"
marshRet _  = ss "(error \"Signal handlers cannot return structured types.\")"

#endif

-------------------------------------------------------------------------------
-- generation of parameterized fragments
-------------------------------------------------------------------------------

mkUserType :: Signature -> ShowS
mkUserType (ret,ts) = let
  (str,cs) = foldl (\(str,cs) t -> 
	    let (str',cs') = usertype t cs in (str.str'.ss " -> ",cs'))
	    (sc '(',['a'..]) ts
  (str',_) = usertype ret cs
  in str.ss "IO ".str'.sc ')'

mkContext :: Signature -> ShowS
mkContext (ret,ts) = let ctxts = context (ts++[ret]) ['a'..] in
  if null ctxts then ss "GObjectClass obj =>" else sc '('.
    foldl1 (\a b -> a.ss ", ".b) ctxts.ss ", GObjectClass obj) =>"

mkMarshType :: Signature -> [ShowS]
mkMarshType (ret,ts) = marshType (ts++[ret]) ['a'..]
  
mkType sig = let types = mkMarshType sig in
  if null types then id else foldl (.) (indent 1) types

mkMarshArg :: Signature -> [ShowS]
mkMarshArg (ret,ts) = zipWith marshArg (ts++[ret]) [1..]

mkArg sig = foldl (.) (sc ' ') $ mkMarshArg sig

#ifdef USE_GCLOSUE_SIGNALS_IMPL

mkMarshExec :: Signature -> ShowS
mkMarshExec (ret,ts) = foldl (\body marshaler -> marshaler body) (indent 5.ss "user")
                             (paramMarshalers++[returnMarshaler])
  where paramMarshalers = [ marshExec t (nameArg t n) n | (t,n) <- zip ts [1..] ]
        returnMarshaler = marshRet ret

#else

mkMarshExec :: Signature -> ShowS
mkMarshExec (_,ts) = foldl (.) id $ 
		     zipWith marshExec ts [1..]

#endif

mkIdentifier :: Signature -> ShowS
mkIdentifier (ret,[]) = identifier Tunit . ss "__".identifier ret
mkIdentifier (ret,ts) = foldl1 (\a b -> a.sc '_'.b) (map identifier ts).
			ss "__".identifier ret

mkRawtype :: Signature -> ShowS
mkRawtype (ret,ts) = 
  foldl (.) id (map (\ty -> rawtype ty.ss " -> ") ts).
  (case ret of
      Tboxed  -> ss "IO (".rawtype ret.sc ')'
      Tptr    -> ss "IO (".rawtype ret.sc ')'
      Tobject -> ss "IO (".rawtype ret.sc ')'
      _       -> ss "IO ".rawtype ret)

mkLambdaArgs :: Signature -> ShowS
mkLambdaArgs (_,ts) = foldl (.) id $ 
		      zipWith (\a b -> nameArg a b.sc ' ') ts [1..]

#ifndef USE_GCLOSUE_SIGNALS_IMPL

mkFuncArgs :: Signature -> ShowS
mkFuncArgs (_,ts) = foldl (.) id $ 
		    zipWith (\a b -> sc ' '.nameArg a b.sc '\'') ts [1..]

mkMarshRet :: Signature -> ShowS
mkMarshRet (ret,_) = marshRet ret

#endif

-------------------------------------------------------------------------------
-- start of code generation
-------------------------------------------------------------------------------


usage = do
 putStr $ "Program to generate callback hook for Gtk signals. Usage:\n"++
   "HookGenerator <signatureFile> <templateFile> <outFile> <moduleName>\n"++
   "where\n"++
   "  <signatureFile> is gtkmarshal.list from the the source Gtk+ tree\n"++
   "  <templateFile>  the name and path of the Signal.chs.template file\n"++
   "  <outFile>	      is the name and path of the output file.\n"++
   "  <moduleName>    the module name for <outFile>\n"
 exitWith $ ExitFailure 1

main = do
  args <- getArgs
  if (length args /= 4) then usage else do
    let [typesFile, templateFile, outFile, outModuleName] = args
    generateHooks typesFile templateFile outFile outModuleName

generateHooks :: String -> String -> String -> String -> IO ()
generateHooks typesFile templateFile outFile outModuleName = do
    content <- readFile typesFile
    let sigs = parseSignatures content
    template <- readFile templateFile
    writeFile outFile $
      templateSubstitute template (\var ->
        case var of
	  "MODULE_NAME"    -> ss outModuleName
          "MODULE_EXPORTS" -> genExport sigs
          "MODULE_BODY"    -> foldl (.) id (map generate sigs)
          _ -> error var 
      ) ""

templateSubstitute :: String -> (String -> ShowS) -> ShowS
templateSubstitute template varSubst = doSubst template 
  where doSubst [] = id
        doSubst ('\\':'@':cs) = sc '@' . doSubst cs
        doSubst ('@':cs) = let (var,_:cs') = span ('@'/=) cs
                            in varSubst var . doSubst cs'
        doSubst (c:cs) = sc c . doSubst cs

-------------------------------------------------------------------------------
-- generate dynamic fragments
-------------------------------------------------------------------------------

genExport :: Signatures -> ShowS
genExport sigs = foldl (.) id (map mkId sigs)
  where
    mkId sig = ss "connect_".mkIdentifier sig.sc ','.indent 1

#ifdef USE_GCLOSUE_SIGNALS_IMPL

generate :: Signature -> ShowS
generate sig = let ident = mkIdentifier sig in
  indent 0.ss "connect_".ident.ss " :: ".
  indent 1.mkContext sig.ss " SignalName ->".
  mkType sig.
  indent 1.ss "ConnectAfter -> obj ->".
  indent 1.mkUserType sig.ss " ->".
  indent 1.ss "IO (ConnectId obj)".
  indent 0.ss "connect_".ident.ss " signal". mkArg sig. ss "after obj user =".
  indent 1.ss "connectGeneric signal after obj action".
  indent 1.ss "where action :: Ptr GObject -> ".mkRawtype sig.
  indent 1.ss "      action _ ".mkLambdaArgs sig. sc '='.
  indent 5.ss "failOnGError $".
  mkMarshExec sig.
  indent 0


#else

generate :: Signature -> ShowS
generate sig = let ident = mkIdentifier sig in
  indent 0.ss "type Tag_".ident.ss " = Ptr () -> ".
  indent 1.mkRawtype sig.
  indent 0.
  indent 0.ss "foreign".ss " import ccall \"wrapper\" ".ss "mkHandler_".ident.ss " ::".
  indent 1.ss "Tag_".ident.ss " -> ".
  indent 1.ss "IO (FunPtr ".ss "Tag_".ident.sc ')'.
  indent 0.
  indent 0.ss "connect_".ident.ss " :: ".
  indent 1.mkContext sig.ss " SignalName ->".
  mkType sig.
  indent 1.ss "ConnectAfter -> obj ->".
  indent 1.mkUserType sig.ss " ->".
  indent 1.ss "IO (ConnectId obj)".
  indent 0.ss "connect_".ident.ss " signal".
  mkArg sig.
  indent 1.ss "after obj user =".
  indent 1.ss "do".
  indent 2.ss "hPtr <- mkHandler_".ident.
  indent 3.ss "(\\_ ".mkLambdaArgs sig.ss "-> failOnGError $ do".
  mkMarshExec sig.
  indent 4.ss "liftM ".mkMarshRet sig.ss " $".
  indent 5.ss "user".mkFuncArgs sig.
  indent 3.sc ')'.
  indent 2.ss "dPtr <- mkFunPtrClosureNotify hPtr".
  indent 2.ss "sigId <- withCString signal $ \\nPtr ->".
  indent 3.ss "withForeignPtr ((unGObject.toGObject) obj) $ \\objPtr ->".
  indent 4.ss "{#call unsafe g_signal_connect_data#} (castPtr objPtr)".
  indent 5.ss "nPtr (castFunPtr hPtr) nullPtr dPtr (fromBool after)".
  indent 2.ss "return $ ConnectId sigId obj".
  indent 0

#endif
