module Abstract where

import Data.PackedString
import Data.FiniteMap
import List

type Loc = Int

type Id	    = PackedString
type Var    = Id
type DaVar  = Var
type TyVar  = Var
type Con    = Id
type DaCon  = Con
type TyCon  = Con
type Module = Id
type DWord  = PackedString

type HContext = [(TyVar, TyCon)]

data Constr = CNorm DaCon [HType]
	    | CRec DaCon [(DaVar, HType)]

data HFunction = FunDecl [DaVar] HContext HType
	       | FunDefn DaVar [HPat]
	       deriving Show

data HPat = PatAt DaVar HPat
	  | PatVar DaVar
	  | PatCon DaCon [HPat]
	  | PatRec [(DaVar, HPat)]
	  | PatAny
	  | PatParen [HPat]
	  | PatList [HPat]
	  deriving Eq

data HType = TyFun HType HType
	   | TyApp HType HType
	   | TyCon PackedString
	   | TyVar PackedString
	   | TyPar [HType]
	   | TyLst HType

instance Show HPat where
  show (PatAt v p) = unpackPS v++'@':show p
  show (PatVar v) = unpackPS v
  show (PatCon c ps) = unpackPS c++concatMap (\p -> ' ':show p) ps
  show (PatRec f) = '{':concat (intersperse ", " 
		    (map (\(v,p) -> unpackPS v++'=':show p) f))++"}"
  show (PatAny) = "_"
  show (PatParen ps) = '(':concat (intersperse ", " (map show ps))++")"
  show (PatList ps) = '[':concat (intersperse ", " (map show ps))++"]"

instance Show HType where
  showsPrec 0 (TyFun t1 t2) =  showsPrec 1 t1 . showString " -> " . 
				     showsPrec 0 t2
  showsPrec n (TyApp t1 t2) | n<2 = showsPrec 0 t1 . showChar ' ' .
			      showsPrec 2 t2
  showsPrec _ (TyCon con)   = showString $ unpackPS con
  showsPrec _ (TyVar var)   = showString $ unpackPS var
  showsPrec _ (TyPar tys)   = showChar '(' . foldr (.) id  
			      (intersperse (showChar ',') (map shows tys)) .
			      showChar ')'
  showsPrec _ (TyLst t)     = showString "&lsqb;" . showsPrec 0 t .
			      showString "&rsqb;"
  showsPrec _ t = showChar '(' . showsPrec 0 t . showChar ')'

showContext :: HContext -> String
showContext [] = ""
showContext [(var, con)] = unpackPS con ++ ' ':unpackPS var ++ " => "
showContext cons = '(': (concat $ intersperse ", " $ 
		   map (\(var,con) -> unpackPS con ++ ' ':unpackPS var) cons)
		   ++") => "

getTyVars :: HType -> [TyVar]
getTyVars (TyFun t1 t2) = getTyVars t1++getTyVars t2
getTyVars (TyApp t1 t2) = getTyVars t1++getTyVars t2
getTyVars (TyCon t) = []
getTyVars (TyVar t) = [t]
getTyVars (TyPar tys) = concatMap getTyVars tys
getTyVars (TyLst t) = getTyVars t
