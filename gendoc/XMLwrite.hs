module XMLwrite where

import State
import Abstract
import PrettyLib
import Data.PackedString
import Data.FiniteMap
import Data.List	(nub, intersperse)

hcat :: [Doc] -> Doc
hcat [] = nil
hcat ls = foldr1 (<>) ls

fsep :: [Doc] -> Doc
fsep = group . hcat  .intersperse (delimiter " ")

type Name  = String
type Value = PackedString
data Attribute  = Attribute Name Value deriving Show

data XML
  = Element { 
    name       :: Name,
    attributes :: [Attribute],
    content    :: [XML] } 
  | Plain {
    value      :: Value }
  | WhiteSpace

--  deriving Show
isElement :: XML -> Bool
isElement (Element _ _ _) = True
isElement _		   = False

allElement :: [XML] -> Bool
allElement = and . map isElement


xmlToDoc :: XML -> Doc
xmlToDoc (WhiteSpace) =  fdelimiter " "
xmlToDoc (Plain value) =  text value
xmlToDoc (Element name attr []) = 
  group (
    lStart <> text (packString name) <> 
    (if null attr then nil else
      groupNest 2 (delimiter " " <> fsep (map attrToDoc attr))) <>
    rStop)
xmlToDoc (Element name attr con) = group (
  group (
    lStart <> text (packString name) <>
    (if null attr then nil else 
      groupNest 2 (delimiter " " <> fsep (map attrToDoc attr))) <>
    rStart
  ) <> 
  groupNest 1 (hcat (
    (if allElement con then intersperse line else id)
    (map xmlToDoc con))) <> 
  group (
    lStop <> text (packString name) <> rStart
  ))


-- fsep (text (packString name):map attrToDoc attr)

attrToDoc :: Attribute -> Doc
attrToDoc (Attribute name val) = text (packString name) <> equals <> 
				 doubleQuotes (text val)

lStart, lStop, rStart, rStop, equals :: Doc
lStart = text (packString "<")
lStop  = text (packString "</")
rStart = text (packString ">")
rStop  = text (packString "/>")
equals = text (packString "=")

doubleQuotes :: Doc -> Doc
doubleQuotes d = q <> d <> q
  where
    q = text (packString "\"")

instance Show XML where
  show = pretty 80 . group . xmlToDoc
  showList = (++) . pretty 80 . group . hcat . map xmlToDoc 

renderXML :: XML -> String
renderXML = show

test = Element "hafunsynopsis" [] [
  Element "function" [] [Plain (packString "textViewNewWithAttributes")],
  Element "hatyfun" [] [
    Element "hatycon" [] [Plain (packString "Int")],
    Element "hatyvar" [] [Plain (packString "cr")]]]

hTypeToXML :: HType -> XML
hTypeToXML (TyFun t1 t2) = Element "hatyfun" [] [hTypeToXML t1,hTypeToXML t2]
hTypeToXML (TyApp t1 t2) = Element "hatyapp" [] [hTypeToXML t1,hTypeToXML t2]
hTypeToXML (TyCon con)   = Element "hatycon" [] [Plain con]
hTypeToXML (TyVar var)   = Element "hatyvar" [] [Plain var]
hTypeToXML (TyPar tys)   = Element "hatypar" [] (map hTypeToXML tys)
hTypeToXML (TyLst t)     = Element "hatylst" [] [hTypeToXML t]

docuToXML :: [Docu] -> [XML]
docuToXML [] = []
docuToXML [Paragraph] = []
docuToXML docu = Element "para" [] (init (dTX para)):
		 docuToXML rem
  where
    isPara :: Docu -> Bool
    isPara Paragraph = True
    isPara _	     = False
    (para,rem) = break isPara (dropWhile isPara docu)
    -- Generate XML that always has a trailing WhiteSpace. The latter is
    -- removed by the call to init above.
    dTX :: [Docu] -> [XML]
    dTX (Words ws:ds) = (concatMap (\x -> [Plain x,WhiteSpace]) ws)++dTX ds
    dTX (RefArg var fol:ds) = Element "emphasis" [] [Plain var]:
			       (if nullPS fol then id else (Plain fol:))
			       (WhiteSpace:dTX ds)
    dTX (RefSym _ var fol:ds) = Element "emphasis" [] [Plain var]:
			       (if nullPS fol then id else (Plain fol:))
			       (WhiteSpace:dTX ds)
    dTX (RefTyp _ var fol:ds) = Element "emphasis" [] [Plain var]:
			       (if nullPS fol then id else (Plain fol:))
			       (WhiteSpace:dTX ds)
    dTX (RefVariant var fol:ds) = Element "emphasis" [] [Plain var]:
			       (if nullPS fol then id else (Plain fol:))
			       (WhiteSpace:dTX ds)
    dTX (Verb str fol:ds) = Element "programlisting" [] [Plain str]:
			       (if nullPS fol then id else (Plain fol:))
			       (WhiteSpace:dTX ds)
    dTX	[] = []

moduleToXML :: Module -> ModInfo -> XML
moduleToXML mName (ModInfo { modCat=cat, modSynop=syn, modIntro=int,
  modTodo=todo, modSymTab=st, modCtxt=ctxt }) =
  Element "refentry" [] $ [
    Element "refnamediv" [] [
      Element "refname" [] [Plain mName],
      Element "refpurpose" [] (docuToXML syn)
    ],
    Element "refsynopsisdiv" [] [
      Element "informaltable" [
        Attribute "frame" (packString "none"),
	Attribute "colsep" (packString "0"),
	Attribute "rowsep" (packString "0")] [
	Element "tgroup" [
	  Attribute "align" (packString "left"),
	  Attribute "cols" (packString "1")] [
	  Element "tbody" [] (
	    map (makeSynopsis ctxt) cons++
	    map (makeSynopsis ctxt) const++
	    map (makeSynopsis ctxt) func++
	    map (makeSynopsis ctxt) meth++
	    map (makeSynopsis ctxt) sign
	  )
	]
      ]
    ]
  ]++makeSect "Introduction" (docuToXML int)
  ++makeSect "Todo" (docuToXML todo)
  ++makeSect "Constructors" (map makeSymDescr cons)
  ++makeSect "Methods" (map makeSymDescr meth)
  ++makeSect "Functions" (map makeSymDescr func)
  ++makeSect "Constants" (map makeSymDescr const)
  ++makeSect "Signals" (map makeSymDescr sign)
  where
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
        Element "title" [] [Plain (packString name)]:docu
      )]

makeSynopsis :: FiniteMap TyVar TyCon -> (DaVar,SymInfo) -> XML
makeSynopsis contexts (var, SymInfo { symType=Just ty }) =
  Element "row" [] [
    Element "entry" [] [
        Element "programlisting" [] [
	  Plain (var `appendPS` 
		packString  (":: "++showContext ctxt++show ty))
	]
    ]
  ]
  where
    ctxt = concatMap getContext (nub (getTyVars ty))
    getContext :: TyVar -> HContext
    getContext var = case contexts `lookupFM` var of
      (Just con) -> [(var, con)]
      Nothing -> []
makeSynopsis contexts (var, _) =
  Element "row" [] [
    Element "entry" [] [
        Element "programlisting" [] [
	  Plain (var `appendPS` packString " &lt;no type information&gt;")
	]
    ]
  ]

    

makeSymDescr :: (DaVar, SymInfo) -> XML
makeSymDescr (var, SymInfo { symKind=k, symDocu=doc, 
			     symType=Just ty, symArgs=args }) =
  Element "refsect2" [] (
    Element "title" [] [Plain var]:
    docuToXML doc
  )
makeSymDescr (var, _) = Element "refsect2" [] 
   [Plain (packString "no type info on symbol"), Plain var]