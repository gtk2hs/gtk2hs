module XMLwrite where

import State
import Abstract
import Text.PrettyPrint
import Data.PackedString
import Data.FiniteMap
import Data.List	(nub)

type Name  = String
type Value = String
data Attribute  = Attribute Name Value deriving Show

data XML
  = Element { 
    name       :: Name,
    attributes :: [Attribute],
    content    :: [XML] } 
  | Plain {
    value      :: Doc
  }
  | Verbatim {
    value      :: Doc
  }
--  deriving Show

isVerbatim :: [XML] -> Bool
isVerbatim (Verbatim _:_) = True
isVerbatim _ = False

xmlToDoc :: [XML] -> [Doc]
xmlToDoc (Plain value:rem) = value:xmlToDoc rem
xmlToDoc (Verbatim value:rem) = value:xmlToDoc rem
xmlToDoc (Element name attr []:Verbatim value:rem) = 
  (lStart <> text name <+> sep (map attrToDoc attr) <> rStop <> value):
  xmlToDoc rem
xmlToDoc (Element name attr []:rem) = 
  (lStart <> text name <+> sep (map attrToDoc attr) <> rStop):
  xmlToDoc rem
xmlToDoc (Element name attr (Verbatim val1:con):Verbatim value:rem) = 
  fcat [
    (lStart <> text name <+> sep (map attrToDoc attr) <> rStart <>
    val1),
    nest 2 (fsep (xmlToDoc con)),
    lStop <> text name <> rStart <> value
  ]:xmlToDoc rem
xmlToDoc (Element name attr (Verbatim val1:con):rem) = 
  fcat [
    (lStart <> text name <+> sep (map attrToDoc attr) <> rStart <> 
    val1),
    nest 2 (fsep (xmlToDoc con)),
    lStop <> text name <> rStart
  ]:xmlToDoc rem
xmlToDoc (Element name attr con:Verbatim value:rem) = 
  fcat [
    (lStart <> text name <+> sep (map attrToDoc attr) <> rStart),
    nest 2 (fsep (xmlToDoc con)),
    lStop <> text name <> rStart <> value
  ]:xmlToDoc rem
xmlToDoc (Element name attr con:rem) = 
  fcat [
    (lStart <> text name <+> sep (map attrToDoc attr) <> rStart),
    nest 2 (fsep (xmlToDoc con)),
    lStop <> text name <> rStart
  ]:xmlToDoc rem
xmlToDoc [] = []

attrToDoc :: Attribute -> Doc
attrToDoc (Attribute name val) = text name <> equals <> doubleQuotes (text val)

lStart, lStop, rStart, rStop :: Doc
lStart = text "<"
lStop  = text "</"
rStart = text ">"
rStop  = text "/>"

instance Show XML where
  show = render.fsep.xmlToDoc.(\x -> [x])
  showList = (++).render.fsep.xmlToDoc

renderXML :: Style -> [XML] -> String
renderXML s = renderStyle s.fsep.xmlToDoc

test = Element "hafunsynopsis" [] [
  Element "function" [] [Plain $ text "textViewNewWithAttributes"],
  Element "hatyfun" [] [
    Element "hatycon" [] [Plain $ text "Int"],
    Element "hatyvar" [] [Plain $ text "cr"]]]

hTypeToXML :: HType -> XML
hTypeToXML (TyFun t1 t2) = Element "hatyfun" [] [hTypeToXML t1,hTypeToXML t2]
hTypeToXML (TyApp t1 t2) = Element "hatyapp" [] [hTypeToXML t1,hTypeToXML t2]
hTypeToXML (TyCon con)   = Element "hatycon" [] [Plain $ text $ unpackPS con]
hTypeToXML (TyVar var)   = Element "hatyvar" [] [Plain $ text $ unpackPS var]
hTypeToXML (TyPar tys)   = Element "hatypar" [] (map hTypeToXML tys)
hTypeToXML (TyLst t)     = Element "hatylst" [] [hTypeToXML t]

docuToXML :: [Docu] -> [XML]
docuToXML [] = []
docuToXML [Paragraph] = []
docuToXML docu = Element "para" [] (dTX para):docuToXML rem
  where
    isPara :: Docu -> Bool
    isPara Paragraph = True
    isPara _	     = False
    (para,rem) = break isPara (dropWhile isPara docu)
	         
    dTX :: [Docu] -> [XML]
    dTX (Words ws:ds) = map (\t -> Plain ((text.unpackPS) t)) ws++dTX ds
    dTX (RefArg var fol:ds) = Element "emphasis" [] 
			       [Plain ((text.unpackPS) var)]:
			       if nullPS fol then dTX ds else 
			         Verbatim ((text.unpackPS) fol):dTX ds
    dTX (RefSym _ var fol:ds) = Element "emphasis" [] 
			       [Plain ((text.unpackPS) var)]:
			       if nullPS fol then dTX ds else 
			         Verbatim ((text.unpackPS) fol):dTX ds
    dTX (RefTyp _ var fol:ds) = Element "emphasis" [] 
			       [Plain ((text.unpackPS) var)]:
			       if nullPS fol then dTX ds else 
			         Verbatim ((text.unpackPS) fol):dTX ds
    dTX (Verb str:ds) = Element "programlisting" []
			  [Verbatim ((text.unpackPS) str)]:dTX ds
    dTX	[] = []

moduleToXML :: Module -> ModInfo -> XML
moduleToXML mName (ModInfo { modCat=cat, modSynop=syn, modIntro=int,
  modTodo=todo, modSymTab=st, modCtxt=ctxt }) =
  Element "refentry" [] $ [
    Element "refnamediv" [] [
      Element "refname" [] [Plain (text (unpackPS mName))],
      Element "refpurpose" [] (docuToXML syn)
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
        Element "title" [] [Plain (text name)]:docu
      )]

makeSynopsis :: FiniteMap TyVar TyCon -> (DaVar,SymInfo) -> XML
makeSynopsis contexts (var, SymInfo { symType=Just ty }) =
  Element "row" [] [
    Element "entry" [] [
        Element "programlisting" [] [
	  Verbatim $ text $ unpackPS var++" :: "++showContext ctxt++show ty
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
	  Verbatim $ text $ unpackPS var++" &lt;no type information&gt;"
	]
    ]
  ]

    

makeSymDescr :: (DaVar, SymInfo) -> XML
makeSymDescr (var, SymInfo { symKind=k, symDocu=doc, 
			     symType=Just ty, symArgs=args }) =
  Element "refsect2" [] (
    Element "title" [] [Plain $ text $ unpackPS var]:
    docuToXML doc
  )
makeSymDescr (var, _) = Element "refsect2" [] 
   [Plain $ text ("no type info on symbol "++unpackPS var)]