-- The parse received a list of Token which we more concicely refer to as:
--
--	'--'		TLComment
--	import		TLImport
--	type		TLType _ Type
--	data		TLType _ Data
--	newtype		TLType _ Newtype
--	instance	TLInstance
--	class		TLClass
--	'@..'		ComHook
--	blah		ComWord
--	para		ComBullet
--	'/--'		ComEnd
--	arg		HookArg
--	hSymKind	HookSymbol
--	hConKind	HookType
--	variant		HookVariant
--	description	HookDescription
--	synopsis	HookSynopsis
--	implementation	HookImplementation
--	literal		HookLiteral
--	hWidget		HookWidget
--	hModule		HookModule
--	ref		HookRef
--	'..@'		HookEnd
--	'@blah'		HookFollow
--	'->'		DefFunArrow
--	'=>'		DefClArrow
--	'('		DefParRoundOp
--	')'		DefParRoundCl
--	'['		DefParSquareOp
--	']'		DefParSquareCl
--	'{'		DefParCurlyOp
--	'}'		DefParCurlyCl
--	'='		DefEquals
--	','		DefComma
--	'::'		DefDoubleColon
--	':'		DefCons
--	'@'		DefAt
--	'_'		DefUnderscore
--	'|'		DefMid
--	deriving	DefDeriving
--	var		DefVar
--	con		DefCon
--	end		DefEnd
--	synErr		Error
--	EOF		[]
--
-- The Parser recognizes the following grammar, starting at Toplevel:
--
-- Toplevel	: EOF
--		| '--' ComIgnore Toplevel
--		| import con end Toplevel
--		| type Type '=' Type end Toplevel
--		| (data | newtype) MContext Type '=' Constrs MDeriving 
--		  Toplevel
--		| class MContext con var end Toplevel
--		| instance MContext con end Toplevel
--		| var Function end Toplevel
--
-- Comments are either ignored (state ComIgnore) or parsed as part of a
-- definition (state ComExtract).
--
-- ComIgnore	: blah ComIgnore
--		| para ComIgnore
--		| '@..' description '..@' ComExtract
--		| '@..' synopsis '..@' ComExtract
--		| '@..' implementation '..@' ComExtract
--		| '@..' hWidget con '..@' ComIgnore
--		| '@..' hModule con '..@' ComIgnore
--		| '@..' hModule con con'..@' ComIgnore
--		| '@..' hSymKind var '..@' ComExtract
--		| '@..' hConKind con '..@' ComExtract
--		| '@..' variant con '..@' ComExtract
--		| '/--'
--
-- ComExtract	: blah ComExtract
--		| para ComExtract
--		| '@..' ref hSymKind var '..@' '@blah' ComExtract
--		| '@..' ref hSymKind var '..@' ComExtract
--		| '@..' ref hConKind con '..@' '@blah' ComExtract
--		| '@..' ref hConKind con '..@' ComExtract
--		| '@..' ref arg var '..@' '@blah' ComExtract
--		| '@..' ref arg var '..@' ComExtract
--		| '@..' ref variant con '..@' '@blah' ComExtract
--		| '@..' ref variant con '..@' ComExtract
--		| '@..' literal '..@' ComExtract
--		| ComIgnore
--
-- Type		: BType
--		| BType -> Type
--
-- BType	: AType
--		| AType BType
--
-- AType	: con AType
--		| var AType
--		| '(' Tuple<',';')';Type> AType
--		| '[' Type ']'
--
-- Tuple<s;f;T>	: T s Tuple<s;f;T>
--		| T f
--
-- MContext	: 
--		| con var '=>'
--		| '(' Tuple<',';')';con var> '=>'
--
-- Constrs	: Constr '|' Constrs
--		| Constr
--
-- Constr	: con AType*
--		| con '{' Tuple<',';'}'; var '::' Type>
--		
-- MDeriving	: end
--		| deriving con end
--		| deriving '(' Tuple<',';')';con> end
--
-- Function	: ',' Tuple<',';'::',var> MContext Type
--		| '::' MContext Type
--		| APat*
--
-- APat		: var '@' APat
--		| var
--		| '{' Tuple<',';'}',var '=' Pat>
--		| con
--		| literal *******************************not yet
--		| '_'
--		| '(' Tuple<',';')',Pat>
--		| '[' Tuple<',';']',Pat>
--
-- Pat		: con APat*
--		| APat
--
-- 
module Parser where

import State
import Abstract
import Update
import Lexer
import Monad
import Data.PackedString
import Data.Set
import Data.List	(intersperse)

type BareParser a = [Token] -> State -> Status a
newtype Parser a = Parser { unParser :: BareParser a}

data Status a = Running a [Token] State
	      | Failed State

instance Monad Parser where
  p >>= k = Parser $ \tok state -> case (unParser p) tok state of
    (Running val (NewLine: tok') state') -> 
      (unParser (k val)) tok' (state' { curPos=curPos state'+1 })
    (Running val tok' state') -> (unParser (k val)) tok' state'
    (Failed state') -> Failed state'
  return val = Parser $ Running val
  fail msg = Parser $ \tok state -> 
	     Failed (addError ("parsing error in "++msg) state)

instance MonadPlus Parser where
  mzero = Parser $ \tok -> Failed 
  p1 `mplus` p2 = Parser $ \tok state -> 
    case (unParser p1) tok state of
      all@(Running _ _ _) -> all
      (Failed state) -> (unParser p2) tok state

processFiles :: State -> IO State
processFiles state = do
  let todo = filesTodo state
  let files = setToList todo
  case files of
    [] -> return state
    (file:_) -> do
      putStr ("\ncompiling module "++file++":")
      state' <- parseFile (state { filesTodo = todo `delFromSet` file}) file
      processFiles (state' { filesDone = (filesDone state') `addToSet` file})

parseFile :: State -> HsModule -> IO State
parseFile state mName = do
  let searchPaths = case setToList (inclPath state) of
		      [] -> ["."]
		      paths -> paths
  mCon <- tryReadFile searchPaths (mName++".hs")
  return $ case mCon of
    Nothing -> addError ("module "++mName++" not found in "++
			 concat (intersperse ":" searchPaths)) state
    Just (filePath,con) -> (runParser parseToplevel (lexer con) 
			   (state { curFile=filePath, curPos=1 }))
			   { curFile="", curPos= -1 }

tryReadFile :: [String] -> FilePath -> IO (Maybe (FilePath,String))
tryReadFile (pa:ths) fname = catch (do
    let filePath = pa++'/':fname
    con <- readFile filePath
    return (Just (filePath, con)))
  (\_ -> tryReadFile ths fname)
tryReadFile [] fname = return Nothing

runParser :: Parser () -> [Token] -> State -> State
runParser p tok state = case (unParser p) tok state of
  (Running () [] state) -> state
  (Running () ts state) -> addError ("runParser: didn't consume all input:"
    ++concatMap show (take 5 ts)) state
  (Failed state) -> addError "parsing finished unsuccessful." state

-- Test the parser on a piece of input.
--
-- After successfully running the scanner on a piece of input this
-- function can be used to check parsing. We drop b symbols in front
-- of the stream and e symbols at the end.
--
tP :: Show a => Int -> Int -> Parser a -> String -> IO ()
tP b e p inp = putStrLn (testParser b e p inp)

testParser :: Show a => Int -> Int -> Parser a -> String -> String
testParser b e p inp = case (unParser p) tok initialState of
  (Running x _ state) -> "Token: "++show tok++
			 "\nResult:\n"++show x++"\nErrors:\n"++
			 show (errors state)
  (Failed state) -> "Token:\n"++show tok++
		    "\nFailed:\n"++show (errors state)
  where
    tok = (drop b (reverse (drop e (reverse (lexer inp)))))

-- Consume the next Token if possible.
token :: Token -> Parser ()
token tok = Parser $ checkToken
  where
    checkToken (NewLine: ts) = \s -> checkToken ts (s { curPos=curPos s+1})
    checkToken (t:ts) | t==tok    = Running () ts
		      | otherwise = Failed
    checkToken []                 = Failed

-- Parse a constructor.
parseCon :: Parser PackedString
parseCon = Parser pC
  where
    pC :: BareParser PackedString
    pC (NewLine: ts) = \s -> pC ts (s { curPos=curPos s+1})
    pC (DefCon con: ts) = Running con ts
    pC ts = Failed

-- Parse a name.
parseVar :: Parser PackedString
parseVar = Parser pV
  where
    pV :: BareParser PackedString
    pV (NewLine: ts) = \s -> pV ts (s { curPos=curPos s+1})
    pV (DefVar var: ts) = Running var ts
    pV ts = Failed

-- Take a Parser and strip it to a BareParser.
stripParser :: [Token] -> Parser a -> State -> Status a
stripParser tok p = (unParser p) tok

-- Update the state in the parser monad.
update :: (State -> State) -> Parser a
update trans = Parser upd
  where
    upd ts state = Running undefined ts (trans state)

type Toplevel = ()

-- Look for synopsis, description, module changes and imports
parseToplevel :: Parser Toplevel
parseToplevel = Parser toplevel

toplevel :: BareParser Toplevel
toplevel [] = Running () []
toplevel (NewLine: ts) = \s -> toplevel ts (s { curPos=curPos s+1})
toplevel (TLComment:ts) = stripParser ts $
  parseComIgnore >> parseToplevel
toplevel (TLImport: DefCon mod: ts) = \state ->
  toplevel (dropWhile (`elem` [DefEnd,NewLine]) ts)
    (addImport (unpackPS mod) state)
toplevel (TLType Type:ts) = 
  stripParser ts (tTS `mplus` parseCheck "type synonym")
    where
      tTS = do
        t1 <- parseType `mplus` fail "new type synonym"
	token DefEquals	`mplus` fail "equal sign"
	t2 <- parseType `mplus` fail "right hand type expression"
	token DefEnd	`mplus` fail "excess token after declaration"
	update $ addType Type t1 [CNorm nilPS [t2]]
	parseToplevel
toplevel (TLType kind:ts) =
  stripParser ts (tND `mplus` parseCheck "newtype/data declaration")
    where
      tND = do
        ctxt <- parseMContext
        ty <- parseType `mplus` fail "type to be declared"
        token DefEquals `mplus` fail "equal sign"
        cons <- parseConstrs `mplus` fail "constructors"
        der <- parseMDeriving `mplus` fail "deriving clause"
	update $ addType kind ty cons
        parseToplevel
toplevel (TLClass: ts) = 
  stripParser ts $ pCl `mplus` parseCheck "class definition"
    where
      pCl = do
        ctxt <- parseMContext
        con <- parseCon `mplus` fail "class name"
        var <- parseVar	`mplus` fail "class type argument"
        token DefEnd	`mplus` fail "too many arguments to class"
        parseToplevel
toplevel (TLInstance: ts) = 
  stripParser ts $ pIn `mplus` parseCheck "instance declaration"
    where
      pIn = do
        ctxt <- parseMContext
	cls <- parseCon `mplus` fail "instance: type class expected"
	con <- parseCon `mplus` fail "constructor (no arguments allowed)"
	token DefEnd	`mplus` fail "instance: too many arguments to class"
	parseToplevel
toplevel (DefVar var: ts) =
  stripParser ts $ pFu `mplus`
  parseCheck "function declaration/definition"
    where
      pFu = do
        fun <- parseFunction var
	token DefEnd `mplus` fail "excess token after function def/decl"
	update $ insertSymbol fun
	parseToplevel
toplevel ts = check "toplevel declaration: " ts

parseComIgnore :: Parser Toplevel
parseComIgnore = Parser cI
  where
    cI :: BareParser Toplevel
    cI (NewLine: ts) = \s -> cI ts (s { curPos=curPos s+1})
    cI (ComWord _:ts) = cI ts
    cI (ComBullet:ts) = cI ts
    cI (ComHook: HookDescription: HookEnd: ts) = stripParser ts $ do
      doc <- parseComExtract
      update $ setIntro doc
      parseComIgnore
    cI (ComHook: HookSynopsis: HookEnd: ts) = stripParser ts $ do
      doc <- parseComExtract
      update $ setSynop doc
      parseComIgnore
    cI (ComHook: HookImplementation: HookEnd: ts) = stripParser ts $ do
      doc <- parseComExtract
      update $ setTodo doc
      parseComIgnore
    cI (ComHook: HookWidget: DefCon mod: HookEnd: ts) = \state ->
      cI ts (changeModule mod Widget state)
    cI (ComHook: HookModule: DefCon mod: DefParRoundOp: DefCon to:
	DefParRoundCl: HookEnd: ts) = \state ->
      cI ts (changeModule mod (Auxilliary to) state)
    cI (ComHook: HookModule: DefCon mod: DefCon to: HookEnd: ts) = \state ->
      cI ts (changeModule mod (Auxilliary to) state)
    cI (ComHook: HookModule: DefCon mod: HookEnd: ts) = \state ->
      cI ts (changeModule mod Independent state)
    cI (ComHook: HookSymbol kind: DefVar var: HookEnd: ts) = 
      stripParser ts $ do
        doc <- parseComExtract
	update $ addSymComm kind var doc
        parseComIgnore
    cI (ComHook: HookType kind: DefCon con: HookEnd: ts) = 
      stripParser ts $ do
        doc <- parseComExtract
        update $ addTypeComm con doc
        parseComIgnore
    cI (ComHook: HookVariant: DefCon con: HookEnd: ts) =
      stripParser ts $ do
        doc <- parseComExtract
	--update $ addVariantComm con doc
	parseComIgnore
    cI (ComEnd: ts) = Running () ts
    cI ts = check "comment" ts

-- Extract the body of a comment.
parseComExtract :: Parser [Docu]
parseComExtract = Parser $ \ts -> pCE ts [] []
  where
    pCE :: [Token] -> [DWord] -> [Docu] -> State -> Status [Docu]
    pCE (NewLine: ts) ws = \docs s -> pCE ts ws docs (s { curPos=curPos s+1})
    pCE (ComWord w: ts) ws = pCE ts (w:ws)
    pCE (ComBullet: ts) [] = \docs -> pCE ts [] (Paragraph: docs)
    pCE (ComHook: HookRef: HookSymbol kind: DefVar var: HookEnd:
	HookFollow fol:ts) [] =
      \docs -> pCE ts [] (RefSym kind var fol:docs)
    pCE (ComHook: HookRef: HookSymbol kind: DefVar var: HookEnd:ts) [] =
      \docs -> pCE ts [] (RefSym kind var nilPS:docs)
    pCE (ComHook: HookRef: HookType kind: DefCon con: HookEnd: 
	HookFollow fol:ts) [] =
      \docs -> pCE ts [] (RefTyp kind con fol:docs)
    pCE (ComHook: HookRef: HookType kind: DefCon con: HookEnd:ts) [] =
      \docs -> pCE ts [] (RefTyp kind con nilPS:docs)
    pCE (ComHook: HookRef: HookArg: DefVar var: HookEnd: HookFollow fol:ts)
      [] = \docs -> pCE ts [] (RefArg var fol:docs)
    pCE (ComHook: HookRef: HookArg: DefVar var: HookEnd:ts) [] =
      \docs -> pCE ts [] (RefArg var nilPS:docs)
    pCE (ComHook: HookRef: HookVariant: DefCon con: HookEnd: HookFollow fol:ts)
      [] = \docs -> pCE ts [] (RefVariant con fol:docs)
    pCE (ComHook: HookRef: HookVariant: DefCon con: HookEnd:ts) [] =
      \docs -> pCE ts [] (RefVariant con nilPS:docs)
    pCE (ComHook: HookLiteral multi txt: HookEnd: HookFollow fol:ts) [] =
      \docs -> pCE ts [] (Verb multi txt fol:docs)
    pCE (ComHook: HookLiteral multi txt: HookEnd:ts) [] =
      \docs -> pCE ts [] (Verb multi txt nilPS:docs)
    pCE ts [] = \docs -> Running (reverse docs) ts
    pCE ts ws = \docs -> pCE ts [] (Words (reverse ws):docs)
      


-- Parse a non-function or a function type.
parseType :: Parser HType
parseType = do
    t1 <- parseBType
    isAbs t1 `mplus` return t1
  where
    isAbs t1 = do
      token DefFunArrow
      t2 <- parseType
      return (TyFun t1 t2)

-- Parse a type application or not.
parseBType :: Parser HType
parseBType = do
    t1 <- parseAType
    parseBType' t1
  where
    parseBType' :: HType -> Parser HType
    parseBType' t1 = do
      t2 <- parseAType
      parseBType' (TyApp t1 t2)
     `mplus` return t1

-- Parse a basic type.
parseAType :: Parser HType
parseAType = Parser pAT
  where
    pAT :: BareParser HType
    pAT (NewLine: ts) = \s -> pAT ts (s { curPos=curPos s+1})
    pAT (DefCon con:ts) = Running (TyCon con) ts
    pAT (DefVar var:ts) = Running (TyVar var) ts
    pAT (DefParRoundOp: DefParRoundCl: ts) = Running (TyPar []) ts
    pAT (DefParRoundOp:ts) = stripParser ts $ liftM TyPar $
			     parseTupel DefComma DefParRoundCl parseType
    pAT (DefParSquareOp:ts) = stripParser ts $ do
      t <- parseType
      token DefParSquareCl
      return (TyLst t)
    pAT ts = Failed

-- Parse a generic enumeration.
parseTupel :: Token -> Token -> Parser a -> Parser [a]
parseTupel sep final parser = pT
  where
    pT = do
      r <- parser
      oneMore r `mplus` noMore r
    
    oneMore r = do
      token sep
      rs <- pT
      return (r:rs)
    noMore r = do
      token final
      return [r]

-- Parse an optional context.
parseMContext :: Parser HContext
parseMContext = Parser pMC
  where
    pMC :: BareParser HContext
    pMC (NewLine: ts) = \s -> pMC ts (s { curPos=curPos s+1})
    pMC (DefCon con: DefVar var: DefClArrow: ts) = Running [(con, var)] ts
    pMC (DefParRoundOp: ts) = stripParser ts $ do
      res <- parseTupel DefComma DefParRoundCl $ Parser pCV
      token DefClArrow
      return res
    pMC ts = Running [] ts

    pCV :: BareParser (TyCon, TyVar)
    pCV (DefCon con: DefVar var: ts) = Running (con,var) ts
    pCV ts = Failed

many :: Parser a -> Parser [a]
many p = do
    oneMore `mplus` return []
  where
    oneMore = do
      r <- p
      rs <- many p
      return (r:rs)

-- Read the constructors of a data or newtype declaration.
parseConstrs :: Parser [Constr]
parseConstrs = do
    c <- parseConstr
    oneMore c `mplus` return [c]
  where
    oneMore c = do
      token DefMid
      cs <- parseConstrs
      return (c:cs)

-- Read one constructor.
parseConstr :: Parser Constr
parseConstr = Parser pC
  where
    pC (NewLine: ts) = \s -> pC ts (s { curPos=curPos s+1})
    pC (DefCon con: DefParCurlyOp:ts) = stripParser ts $ do
      fields <- parseTupel DefComma DefParCurlyCl $ do
        vars <- parseTupel DefComma DefDoubleColon parseVar
	ty <- parseType `mplus` fail "type of record field"
	return (map (\var -> (var,ty)) vars)
      return (CRec con (concat fields)) 
    pC (DefCon con:ts) = stripParser ts $ do
      tys <- (many parseAType)
      return (CNorm con tys)
    pC ts = Failed

-- Parse the deiving clause for user data types.
parseMDeriving :: Parser [TyCon]
parseMDeriving = Parser pMD
  where
    pMD (NewLine: ts) = \s -> pMD ts (s { curPos=curPos s+1})
    pMD (DefEnd:ts) = Running [] ts
    pMD (DefDeriving: DefCon con: DefEnd:ts) = Running [con] ts
    pMD (DefDeriving: DefParRoundOp: ts) = stripParser ts $ do
      cons <- parseTupel DefComma DefParRoundCl parseCon
      token DefEnd
      return cons
    pMD ts = stripParser ts $ fail $
	     "deriving clause: "++concatMap show (take 5 ts)


-- Parse a function definition or type signature.
parseFunction :: DaVar -> Parser HFunction
parseFunction var = pTypeMany `mplus` pTypeOne `mplus` 
		    liftM (FunDefn var) (many parseAPat)
  where
    pTypeMany = do
      token DefComma
      vars <- parseTupel DefComma DefDoubleColon parseVar `mplus`
	      fail "type declaration of several variables"
      ctxt <- parseMContext
      ty <- parseType `mplus`
	    fail "type of several variables"
      return (FunDecl (var:vars) ctxt ty)
    pTypeOne = do
      token DefDoubleColon
      ctxt <- parseMContext
      ty <- parseType `mplus`
	    fail ("type of variable "++unpackPS var)
      return (FunDecl [var] ctxt ty)

-- Parse a simple pattern.
parseAPat :: Parser HPat
parseAPat = Parser pAP
  where
    pAP (NewLine: ts) = \s -> pAP ts (s { curPos=curPos s+1})
    pAP (DefVar var: DefAt: ts) = stripParser ts $ do
      apat <- parseAPat
      return (PatAt var apat)
    pAP (DefVar var: ts) = Running (PatVar var) ts
    pAP (DefParCurlyOp: ts) = stripParser ts $ liftM PatRec $
      parseTupel DefComma DefParCurlyCl $ do
        var <- parseVar
	token DefEquals
	pat <- parsePat
	return (var,pat)
    pAP (DefCon con: ts) = Running (PatCon con []) ts
    pAP (DefUnderscore: ts) = Running PatAny ts
    pAP (DefParRoundOp: DefParRoundCl: ts) = Running (PatParen []) ts
    pAP (DefParRoundOp: ts) = stripParser ts $ liftM PatParen $
      parseTupel DefComma DefParRoundCl parsePat
    pAP (DefParSquareOp: DefParSquareCl: ts) = Running (PatList []) ts
    pAP (DefParSquareOp: ts) = stripParser ts $ liftM PatList $
      parseTupel DefComma DefParSquareCl parsePat
    pAP ts = Failed

-- Parse a pattern.
parsePat = parseConVars `mplus` parseAPat
  where
    parseConVars = do
      con <- parseCon
      pats <- many parseAPat
      return (PatCon con pats)

-- All error recovery is at toplevel where the global state is augmented
-- with error messages, where this function comes in. It tries to 
-- evaluate a Parser. If this succeeds, it restarts the Toplevel
-- parser and inserts the result into the symbol table. Otherwise it
-- throws away tokens until the stream looks sensible again. The parsing
-- then continues as if we were at a toplevel declaration.
parseCheck :: String -> Parser Toplevel
parseCheck msg = Parser $ check msg

check :: String -> BareParser Toplevel
check msg ts s = pE ts (addError
  ("parse error in "++msg++": "++concatMap show (take 10 ts)) s)
  where
    pE :: BareParser Toplevel
    pE [] = toplevel []
    pE (Error str:ts) = \s -> pE ts (addPackedError str s)
    pE (NewLine:ts) = \s -> pE ts (s { curPos=curPos s+1})
    pE (DefEnd:ts@(TLComment:_)) = toplevel ts
    pE (DefEnd:ts@(TLType _:_)) = toplevel ts
    pE (DefEnd:ts@(TLClass:_)) = toplevel ts
    pE (DefEnd:ts@(TLInstance:_)) = toplevel ts
    pE (t:ts) = pE ts


