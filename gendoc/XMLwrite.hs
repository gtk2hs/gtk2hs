module XMLwrite where

import State
import Abstract
import PrettyLib
import Maybe		(fromMaybe)
import List		(transpose)
import Data.PackedString
import Data.FiniteMap
import Data.List	(nub, intersperse)
import Debug.Trace

hcat :: [Doc] -> Doc
hcat [] = nil
hcat ls = foldr1 (<>) ls

fsep :: [Doc] -> Doc
fsep = group . hcat  .intersperse (delimiter " ")

type Name  = String
type Value = String
data Attribute  = Attribute Name Value deriving Show

data XML
  = Element { 
    name       :: Name,
    attributes :: [Attribute],
    content    :: [XML] } 
  | Plain {
    value      :: String }
  | WhiteSpace

-- Escape characters in plain text.
--
escape ('<':xs) = '&':'l':'t':';':escape xs
escape ('>':xs) = '&':'g':'t':';':escape xs
escape ('"':xs) = '&':'q':'u':'o':'t':';':escape xs
escape ('[':xs) = '&':'l':'s':'q':'b':';':escape xs
escape (']':xs) = '&':'r':'s':'q':'b':';':escape xs
escape (x:xs) = x:escape xs
escape [] = []

isElement :: XML -> Bool
isElement (Element _ _ _) = True
isElement _		   = False

allElement :: [XML] -> Bool
allElement = and . map isElement

xmlToDoc :: XML -> Doc
xmlToDoc (WhiteSpace) =  fdelimiter " "
xmlToDoc (Plain value) =  text (escape value)
xmlToDoc (Element name attr []) = 
  group (
    lStart <> text name <> 
    (if null attr then nil else
      groupNest 2 (delimiter " " <> fsep (map attrToDoc attr))) <>
    rStop)
xmlToDoc (Element name attr con) = group (
  group (
    lStart <> text name <>
    (if null attr then nil else 
      groupNest 2 (delimiter " " <> fsep (map attrToDoc attr))) <>
    rStart
  ) <> 
  groupNest 1 (hcat (
    (if allElement con then intersperse line else id)
    (map xmlToDoc con))) <> 
  group (
    lStop <> text name <> rStart
  ))


-- fsep (text (packString name):map attrToDoc attr)

attrToDoc :: Attribute -> Doc
attrToDoc (Attribute name val) = text name <> equals <> 
				 doubleQuotes (text val)

lStart, lStop, rStart, rStop, equals :: Doc
lStart = text "<"
lStop  = text "</"
rStart = text ">"
rStop  = text "/>"
equals = text "="

doubleQuotes :: Doc -> Doc
doubleQuotes d = q <> d <> q
  where
    q = text "\""

instance Show XML where
  show = pretty 80 . group . xmlToDoc
  showList = (++) . pretty 80 . group . hcat . map xmlToDoc 

renderXML :: XML -> String
renderXML = show

docuToXML :: (Module,ModInfo) -> [Docu] -> [XML]
docuToXML (mName,mI) [] = []
docuToXML (mName,mI) [Paragraph] = []
docuToXML (mName,mI) docu = 
  Element "para" [] (init (dTX para)):docuToXML (mName,mI) rem
  where
    isPara :: Docu -> Bool
    isPara Paragraph = True
    isPara _	     = False
    (para,rem) = break isPara (dropWhile isPara docu)
    -- Generate XML that always has a trailing WhiteSpace. The latter is
    -- removed by the call to init above.
    dTX :: [Docu] -> [XML]
    dTX (Words ws:ds) =
      (concatMap (\x -> [Plain (unpackPS x),WhiteSpace]) ws)++dTX ds
    dTX (RefArg var fol:ds) = Element "varname" [] [Plain (unpackPS var)]:
      (if nullPS fol then id else (Plain (unpackPS fol):)) (WhiteSpace:dTX ds)
    dTX (RefSym _ var fol:ds) = 
	Element "link"
	 [Attribute "linkend" (unpackPS var)]
	 [Element "varname" [] (Plain (unpackPS var):
	  (if nullPS fol then [] else [Plain (unpackPS fol)]))]:
	WhiteSpace:dTX ds
    dTX (RefTyp _ var fol:ds) =
	Element "link"
	 [Attribute "linkend" (unpackPS var)]
	 [Element "varname" [] (Plain (unpackPS var):
	  (if nullPS fol then [] else [Plain (unpackPS fol)]))]:
	WhiteSpace:dTX ds
    dTX (RefVariant var fol:ds) = 
	Element "link"
	 [Attribute "linkend" (unpackPS var)]
	 [Element "varname" [] (Plain (unpackPS var):
	  (if nullPS fol then [] else [Plain (unpackPS fol)]))]:
	WhiteSpace:dTX ds
    dTX (Verb multi str fol:ds) =
      (if multi then Element "literallayout" [Attribute "class" "monospaced"]
	 else Element "literal" [])
      [Plain (unpackPS str)]:
      (if nullPS fol then id else (Plain (unpackPS fol):)) (WhiteSpace:dTX ds)
    dTX	[] = []

moduleToXML :: Module -> ModInfo -> XML
moduleToXML mName mI@(ModInfo { modCat=cat, modSynop=syn, modIntro=int,
  modTodo=todo, modSymTab=st, modTypTab=tt, modCtxt=ctxt }) =
  Element "refentry" [
    Attribute "id" (unpackPS mName)
  ] $ [
    Element "refnamediv" [] [
      Element "refname" [] [Plain (unpackPS mName)],
      Element "refpurpose" [] (docuToXML (mName,mI) syn)
    ],
    Element "refsynopsisdiv" [] [
      Element "informaltable" [
        Attribute "frame" "none",
	Attribute "colsep" "0",
	Attribute "rowsep" "0"] [
	Element "tgroup" [
	  Attribute "align" "left",
	  Attribute "cols" "1"] [
	  Element "tbody" [] (
	    map (makeTypeSig mName) dataty++
	    map (makeSynopsis mName ctxt) cons++
	    map (makeSynopsis mName ctxt) const++
	    map (makeSynopsis mName ctxt) func++
	    map (makeSynopsis mName ctxt) meth++
	    map (makeSynopsis mName ctxt) sign
	  )
	]
      ]
    ]
  ]++makeSect "Introduction" (docuToXML (mName,mI) int)
  ++makeSect "Todo" (docuToXML (mName,mI) todo)
  ++makeSect "Datatypes" (map (makeTypeDescr mName mI) dataty)
  ++makeSect "Constructors" (map (makeSymDescr mName mI) cons)
  ++makeSect "Methods" (map (makeSymDescr mName mI) meth)
  ++makeSect "Functions" (map (makeSymDescr mName mI) func)
  ++makeSect "Constants" (map (makeSymDescr mName mI) const)
  ++makeSect "Signals" (map (makeSymDescr mName mI) sign)
  where
    dataty = fmToList tt
    (cons, meth, func, const, sign) = partitionSym (fmToList st)
    partitionSym [] = ([],[],[],[],[])
    partitionSym (all@(_,SymInfo { symKind=Constructor }):syms) = let
      (cons, meth, func, const, sign) = partitionSym syms
      in (all:cons, meth, func, const, sign)
    partitionSym (all@(_,SymInfo { symKind=Method }):syms) = let
      (cons, meth, func, const, sign) = partitionSym syms
      in (cons, all:meth, func, const, sign)
    partitionSym (all@(_,SymInfo { symKind=Function }):syms) = let
      (cons, meth, func, const, sign) = partitionSym syms
      in (cons, meth, all:func, const, sign)
    partitionSym (all@(_,SymInfo { symKind=Constant }):syms) = let
      (cons, meth, func, const, sign) = partitionSym syms
      in (cons, meth, func, all:const, sign)
    partitionSym (all@(_,SymInfo { symKind=Signal }):syms) = let
      (cons, meth, func, const, sign) = partitionSym syms
      in (cons, meth, func, const, all:sign)
    makeSect :: String -> [XML] -> [XML]
    makeSect name [] = []
    makeSect name docu = [
      Element "refsect1" [] (
        Element "title" [] [Plain name]:docu
      )]

makeSynopsis :: Module -> FiniteMap TyVar TyCon -> (DaVar,SymInfo) -> XML
makeSynopsis mName contexts (var, SymInfo { symType=Just ty }) =
  Element "row" [] [
    Element "entry" [] [
        Element "literallayout" [
	  Attribute "class" "monospaced"] [
	  Element "link" [
	    Attribute "linkend" (unpackPS var)
	  ] [
	    Plain (unpackPS var)],
	  Plain (":: "++showContext ctxt++show ty)
	]
    ]
  ]
  where
    ctxt = concatMap getContext (nub (getTyVars ty))
    getContext :: TyVar -> HContext
    getContext var = case contexts `lookupFM` var of
      (Just con) -> [(var, con)]
      Nothing -> []
makeSynopsis mName contexts (var, _) =
  Element "row" [] [
    Element "entry" [] [
        Element "literallayout" [] [
	  Plain (unpackPS var++" <no type information>")
	]
    ]
  ]


-- Create the keyword from the ConInfo datatype.
getKeyword :: ConInfo -> XML
getKeyword (ConInfo { conNewType=True }) = Plain "newtype"
getKeyword (ConInfo { conNewType=False}) = Plain "data"
getKeyword (SynInfo { })		 = Plain "type"

-- The short description of an algebraic data type consists of "data Blah"
-- without any constructors.
makeTypeSig :: Module -> (TyCon, ConInfo) -> XML
makeTypeSig mName (con, info) =
  Element "row" [] [
    Element "entry" [] [
        Element "literallayout" [
	  Attribute "class" "monospaced"] [
	  getKeyword info,
	  WhiteSpace,
	  Element "link" [
	    Attribute "linkend" (unpackPS con)
	  ] [Plain (show (conSig info))]
	]
    ]
  ]

makeSymDescr :: Module -> ModInfo -> (DaVar, SymInfo) -> XML
makeSymDescr mName mI@(ModInfo { modCtxt=classMap })
  (var, SymInfo { symKind=k, symDocu=doc, symType=Just ty, symArgs=args }) =
  let (syn, thorough) = span (not . isParagraph) doc in
  Element "refsect2" [Attribute "id" (unpackPS var)] (
    Element "title" [] [Plain (unpackPS var)]:
    docuToXML (mName,mI) syn++
    Element "informaltable" [
      Attribute "frame" "none",
      Attribute "colsep" "0",
      Attribute "rowsep" "0"
    ] (
      [
      Element "tgroup" [
	Attribute "align" "center",
	Attribute "cols" (show noOfCols)
      ] (
	[Element "colspec" [] []] ++
        (replicate noOfCols 
           (Element "colspec" [Attribute "colwidth" "100pt"] [])) ++
	[Element "tbody" [
	  Attribute "valign" "middle"
	] (
	  Element "row" [] (
	    Element "entry" [
	      Attribute "morerows" (show noOfRows)
	    ] [
	      Plain (unpackPS var)
	    ]:typeToXML (\_ -> Nothing) ty (Just formatter)
	  ):map argsToXML (transpose args)
	)
      ])
    ]):docuToXML (mName,mI) thorough
  )
  where
    noOfRows = case args of
      [] -> 0
      (equations:_) -> length equations
    noOfCols = length (take 50 args) -- don't loop forever if no defn
    formatter :: [XML] -> [XML] -> [XML]
    formatter x1 x2@(Element "entry" _ _:_) =
		 Element "entry" [] (Plain "::":WhiteSpace:x1):x2
    formatter x1 x2 = [Element "entry" [] (Plain "::":WhiteSpace:x1),
		      Element "entry" [
		        Attribute "morerows" (show noOfRows)]
			(Plain "::":WhiteSpace:x2)]
    argsToXML :: [HPat] -> XML
    argsToXML args = Element "row" [] (map argToXML args)
    argToXML :: HPat -> XML
    argToXML hPat = Element "entry" [] [Plain (show hPat)]

makeSymDescr mName _ (var, _) = Element "refsect2" [] 
   [Plain "no type info on symbol ", Plain (unpackPS var)]

makeTypeDescr :: Module -> ModInfo -> (TyCon, ConInfo) -> XML
makeTypeDescr mName mI 
  (con, info@ConInfo { conNewType=isNT, conDocu=doc, conSig=ty,
		       conDaCon=variants }) =
  let (syn, thorough) = span (not . isParagraph) doc in
  Element "refsect2" [Attribute "id" (unpackPS con)] (
    Element "title" [] [getKeyword info, WhiteSpace, Plain (unpackPS con)]:
    docuToXML (mName,mI) syn++
    Element "informaltable" [
      Attribute "frame" "none",
      Attribute "colsep" "0",
      Attribute "rowsep" "0"
    ] [Element "tgroup" [
	Attribute "align" "left",
	Attribute "cols" "2"
      ] [
	Element "colspec" [] [],
	Element "colspec" [] [],
	Element "tbody" [Attribute "valign" "middle"]
	  (doVariants first (fmToList variants))
      ]
    ]:docuToXML (mName,mI) thorough
  )
  where
    first :: XML
    first = Element "entry" [] ([getKeyword info, WhiteSpace]++
	    typeToXML (\_ -> Nothing) ty Nothing ++[WhiteSpace, Plain "="])
    next :: XML
    next = Element "entry" [Attribute "align" "right"] [Plain "|", WhiteSpace]

    -- Create the "data ..." entry this first time, after that procude "|"
    doVariants :: XML -> [(DaCon,DaConInfo)] -> [XML]
    doVariants left []       = []
    doVariants left (info:s) = doVariant left info:
			       doVariants next s

    doVariant :: XML -> (DaCon,DaConInfo) -> XML
    doVariant left (con,DaConSimple { daConDocu=doc, daConType=tys }) =
      Element "row" [] [left, Element "entry" []  
        (intersperse WhiteSpace (Plain (unpackPS con):flatten
	  (concatMap (\t -> typeToXML (\_ -> Nothing) t Nothing) tys)))]
    doVariant left (con, DaConRecord { daConDocu=doc, daConType=tys,
				       daConSel=sels }) =
      Element "row" [] [left, Element "entry" []
        (intersperse WhiteSpace (Plain (unpackPS con):flatten
	  (concatMap (\t -> typeToXML (\_ -> Nothing) t Nothing) tys)))]

makeTypeDescr mName mI (con, info@SynInfo { conSig=new, conRHS=old,
					    conDocu=doc }) = 
  Element "refsect2" [Attribute "id" (unpackPS con)] (
    Element "title" [] [Plain ("type "++unpackPS con)]:
    Element "informaltable" [
      Attribute "frame" "none",
      Attribute "colsep" "0",
      Attribute "rowsep" "0"
    ] [Element "tgroup" [
	Attribute "align" "left",
	Attribute "cols" "2"
      ] [
	Element "colspec" [] [],
	Element "colspec" [] [],
	Element "tbody" [Attribute "valign" "middle"] [
	  Element "row" [] [
	    Element "entry" [] (typeToXML (\_ -> Nothing) new Nothing),
	    Element "entry" [] (typeToXML (\_ -> Nothing) old Nothing)
	  ]
	]
      ]
    ]:docuToXML (mName,mI) doc
  )

-- Combine several consecutive Plain elements.
--
flatten :: [XML] -> [XML]
flatten (Plain t1: Plain t2: xs) = flatten (Plain (t1++t2):xs)
flatten (x:xs) = x:flatten xs
flatten [] = []

-- Convert a type into XML. Each element in the list corresponds to one
-- element in the type.
--
typeToXML :: (Var -> Maybe String) -> HType -> 
	     Maybe ([XML] -> [XML] -> [XML]) -> [XML]
typeToXML varInfo ty comb = flatten $ ttX 0 (fromMaybe plain comb) ty
  where
    plain :: [XML] -> [XML] -> [XML]
    plain xs1 xs2 = xs1++WhiteSpace:Plain "->":WhiteSpace:xs2
    ttX :: Int -> ([XML] -> [XML] -> [XML]) -> HType -> [XML]
    ttX 0 f (TyFun t1 t2) = f (ttX 1 plain t1) (ttX 0 f t2)
    ttX n _ (TyApp t1 t2) | n<2 = ttX 0 plain t1++WhiteSpace:ttX 2 plain t2
    ttX _ _ (TyCon con) = case varInfo con of
      Nothing -> [Plain (unpackPS con)]
      Just refName ->
        [Element "link" [Attribute "linkend" refName] [Plain (unpackPS con)]]
    ttX _ _ (TyVar var) = case varInfo var of
      Nothing -> [Plain (unpackPS var)]
      Just refName ->
        [Element "link" [Attribute "linkend" refName] [Plain (unpackPS var)]]
    ttX _ _ (TyPar tys) = Plain "(":concat 
			  (intersperse [Plain ","]
			    (map (ttX 0 plain) tys)
			  )++
			  [Plain ")"]
    ttX _ _ (TyLst t) = Plain "[":
			ttX 0 plain t++
			[Plain "]"]
    ttX _ _ t = Plain "(":ttX 0 plain t++[Plain ")"]
