module Update where

import Data.PackedString
import Data.FiniteMap
import Data.Set
import List
import Abstract
import State
import Debug.Trace

-- Change the current module.
changeModule :: Module -> ModCategory -> State -> State
changeModule name cat state@(State { modTab=tab }) =
  case tab `lookupFM` name of
    (Just mInfo@(ModInfo { modCat=Independent })) ->
      state { curModule=name,
	      modTab=addToFM tab name (mInfo { modCat = cat }) }
    (Just mInfo@(ModInfo { modCat=cat' })) -> if cat==cat' then
        state { curModule=name }
      else
        addError ("module "++unpackPS name++" is supposed to have two \
		 \different kinds: "++show cat'++" and "++show cat') 
	state
    Nothing -> state { curModule=name, modTab=addToFM tab name
      (ModInfo {
        modCat=cat,
	modSynop=[],
	modIntro=[],
	modTodo=[],
	modSymTab=emptyFM,
	modTypTab=emptyFM,
	daConXRef=emptyFM,
	modCtxt=emptyFM 
      })}


-- Check if a symbol was already referenced before its definition. Lookup
-- both, the unqualified and the qualified name.
execForward :: Id -> State -> State
execForward name state@(State { curModule=mod, forward=for }) =
  ((lookupWithDefaultFM for id name).
  (lookupWithDefaultFM for id (mod `appendPS` consPS '.' name))) state

-- Postpone an update if the symbol hasn't been defined yet.
canPostpone :: (ModInfo -> FiniteMap Id a) -> 
	       Var -> (State -> State) -> State -> State
canPostpone sel name upd = withCurModule $ \mInfo state ->
  if name `elemFM` (sel mInfo) then upd state else
    state { forward=addToFM_C (.) (forward state) name upd }

canPostponeVar :: Var -> (State -> State) -> State -> State
canPostponeVar = canPostpone modSymTab

-- Postpone an update if the constructor hasn't been defined yet.
canPostponeDaCon :: DaCon -> (State -> State) -> State -> State
canPostponeDaCon = canPostpone daConXRef

canPostponeTyCon :: DaCon -> (State -> State) -> State -> State
canPostponeTyCon = canPostpone modTypTab

-- Global Module and file handling.

-- Add a name that should be dealt with later on.
addImport :: FilePath -> State -> State
addImport fname state@(State { filesTodo=todo, filesDone=done, 
			       filesExcl=ignore }) =
  if fname `elementOf` done || fname `elementOf` ignore then state else
    state { filesTodo=addToSet (filesTodo state) fname }

-- Add a type definition to the symbol table
addType :: ConKind -> HType -> [Constr] -> State -> State
addType kind ty cons = withCurModule $
  \mI@(ModInfo { modTypTab=tt, daConXRef=xref }) state -> let
    tn = getTypeName ty
    getTypeName :: HType -> TyCon
    getTypeName (TyCon tc) = tc
    getTypeName (TyApp t _) = getTypeName t
    getTypeName t = error ("Update.addType: illegal type : "++show t)

{-
    tvars = getTypeVars tt
    getTypeVars :: HType -> [TyVar]
    getTypeVars (TyFun t1 t2) = getTypeVars t1++getTypeVars t2
    getTypeVars (TyApp t1 t2) = getTypeVars t1++getTypeVars t2
    getTypeVars (TyCon _) = []
    getTypeVars (TyVar tv) = [tv]
    getTypeVars (TyPar ts) = contactMap getTypeVars ts
    getTypeVars (TyLst t) = getTypeVars t -- can't happen
-}
    getConName :: Constr -> DaCon
    getConName (CNorm dc _) = dc
    getConName (CRec dc _) = dc

    doUpdate :: ConInfo -> State
    doUpdate ti = 
      replaceModInfo (mI { modTypTab=addToFM tt tn ti,
			   daConXRef=addListToFM xref
				     (map (\con -> (getConName con, tn)) cons)
			 }) state

    createDaConDesc :: Constr -> (DaCon, DaConInfo)
    createDaConDesc (CNorm con tys) = (con,DaConSimple {
      daConDocu  = [],
      daConType	 = tys})
    createDaConDesc (CRec con varNtypes) = (con,
      let (vars,types) = unzip varNtypes in
        DaConRecord {
	  daConDocu  = [],
	  daConType  = types,
	  daConSel   = vars})

    getDaCon :: Constr -> DaCon
    getDaCon (CNorm con _) = con
    getDaCon (CRec con _) = con

  in case (tt `lookupFM` tn, kind) of
    (Nothing, Type) -> execForward tn $ doUpdate (SynInfo {
      conSig = ty,
      conRHS = (\[CNorm _ [ty]] -> ty) cons,
      conDocu = [] })
    (Nothing, _) -> foldr (.) id (map execForward (tn:map getDaCon cons)) $
		    doUpdate (ConInfo {
      conNewType= kind==Newtype,
      conDocu = [],
      conSig  = ty,
      conDaCon= listToFM (map createDaConDesc cons) })
    (Just _, _) -> addError ("type constructor "++
			     show tn++" was already present") state

-- Add documentation for a type constructor.
addTypeComm :: TyCon -> [Docu] -> State -> State
addTypeComm con docu = canPostponeTyCon con $ withCurModule $
  \mI@(ModInfo { modTypTab=tt }) state -> let
    doUpdate :: ConInfo -> State
    doUpdate ci = replaceModInfo (mI { modTypTab=addToFM tt con ci })
		  state
  in case tt `lookupFM` con of
    Nothing -> addError ("inserting documentation: "++
			"symbol "++show con++" not found ") state
    (Just ti) -> doUpdate (ti { conDocu=conDocu ti++docu })


-- Insert a variable declaration or definition.
insertSymbol :: HFunction -> State -> State
insertSymbol (FunDecl names ctxt ty) =
  foldr1 (.) (map insertDecl (names)) . 
  foldr (.) id (map insertContext ctxt)
  where
    insertDecl :: DaVar -> State -> State
    insertDecl var = withCurModule $ \mI@(ModInfo { modSymTab=st }) state ->
      case st `lookupFM` var of
        Nothing -> state
	(Just si@(SymInfo { symType=Just _ })) ->
	  addError ("type for symbol "++unpackPS var++
		    " is declared several times.") state
	(Just si) -> replaceModInfo (mI { 
	  modSymTab=addToFM st var (si { symType=Just ty }) }) state
    insertContext :: (TyVar, TyCon) -> State -> State
    insertContext (con,var) = withCurModule $ 
      \mI@(ModInfo { modCtxt=ct }) state -> case ct `lookupFM` var of
        (Just con') -> if con==con' then state else
	  addError ("type variabel "++unpackPS var++" was used with class "++
		    unpackPS con'++" earlier.") state
	Nothing -> replaceModInfo (mI { modCtxt=addToFM ct var con }) state
insertSymbol (FunDefn fun pats) = withCurModule $ 
  \mI@(ModInfo { modSymTab=st }) state -> case st `lookupFM` fun of
    Nothing -> state
    (Just si@(SymInfo { symArgs=args })) -> replaceModInfo (mI {
      modSymTab=addToFM st fun (si { symArgs=zipWith (:) pats
			(if args==[] then repeat [] else args) }) })
      state

{-
  this was to merge the different patterns for a function which is probably
  not a good idea (better a line for each case)  
  where
    merge :: [HPat] -> [[HPat]] -> [[HPat]]
    merge ps [] = map (\x -> [x]) ps
    merge [] as = as
    merge (PatAny:ps) (a:as) = a:(merge ps as)
    merge (p:ps) ([PatAny]:as) = [p]:(merge ps as)
    merge (p:ps) (a:as) | p `elem` a = a:(merge ps as)
			| otherwise  = (p:a):(merge ps as)
-}

-- Insert the commentary for a symbol.
addSymComm :: SymKind -> DaVar -> [Docu] -> State -> State
addSymComm kind var docu = withCurModule $
  \mI@(ModInfo { modSymTab=st }) state -> let
    doUpdate :: SymInfo -> State
    doUpdate si = replaceModInfo (mI { modSymTab=addToFM st var si })
		  state
  in case st `lookupFM` var of
    Nothing -> doUpdate (SymInfo {
      symKind = kind,
      symDocu = docu,
      symType = Nothing,
      symArgs = [] })
    (Just si@(SymInfo { symKind=k})) -> if k==kind then 
      doUpdate si { symDocu=docu++symDocu si }
      else addError ("symbol was already present as "++show k) state

-- Individual Module handling.
-- Do something with the current module.
withCurModule :: (ModInfo -> State -> State) -> State -> State
withCurModule fun state@(State { curModule=mod, modTab=tab }) =
  case tab `lookupFM` mod of
    (Just mInfo) -> fun mInfo state
    Nothing	 -> addError
		    ("withCurModule: there is no such module "++unpackPS mod)
		    state

-- Replace the current module description.
replaceModInfo :: ModInfo -> State -> State
replaceModInfo mI state =
  state { modTab = addToFM (modTab state) (curModule state) mI }

-- Change a the current module description.
updateModInfo :: (ModInfo -> ModInfo) -> State -> State
updateModInfo upd = withCurModule $ \mI -> replaceModInfo (upd mI)

setSynop :: [Docu] -> State -> State
setSynop docu = updateModInfo (\mI -> mI { modSynop=docu })

setIntro :: [Docu] -> State -> State
setIntro docu = updateModInfo (\mI -> mI { modIntro=docu })

setTodo :: [Docu] -> State -> State
setTodo docu = updateModInfo (\mI -> mI { modTodo=docu })

-- Add an error message. Pass -1 for no location.
addError :: String -> State -> State
addError txt s = s { errors=(curPos s, curFile s, packString txt):errors s }

addPackedError :: PackedString -> State -> State
addPackedError txt s = s { errors=(curPos s, curFile s, txt):errors s }


-- Show errors.
showErrors :: ErrSpec -> String
showErrors errs = concatMap showErr (reverse errs)
  where
    showErr (-1,_,e) = "\n"++"toplevel:\n"++unpackPS e
    showErr (l,f,e) = "\n"++f++'(':show l++"):\n"++unpackPS e

instance Show State where
  show (state@State { errors=[], modTab=mt }) =
    concatMap showMod (fmToList mt)
    where
      showMod :: (Module,ModInfo) -> String
      showMod (mod,mi) = "\nmodule: "++unpackPS mod++
	  concatMap (\(n,si) -> "\n"++unpackPS n++":: "++
            show si) (fmToList (modSymTab mi))

  show (State { errors=err }) = showErrors err

instance Show SymInfo where
  show (SymInfo { symKind=kind, symType=typ, symArgs=args }) =
    show kind++" "++show typ++"\n  "++show args

instance Show ConInfo where
  show (ConInfo { conNewType=nt }) | nt = "newtype"
				   | otherwise = "data"
  show (SynInfo { }) = "type"

