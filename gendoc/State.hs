module State where

import Data.PackedString
import Data.FiniteMap
import Data.Set
import Abstract

type ErrSpec = [(Loc,FilePath,PackedString)]

-- This is the base name of a file without .hs and the path.
type HsModule = String

data ModCategory
  = Widget
  | Auxilliary Module
  | Independent
  deriving (Show,Eq)

data State = State {
  curPos    :: Loc,
  curModule :: Module,
  modTab    :: FiniteMap Module ModInfo,
  forward   :: FiniteMap Id (State -> State),
  filesTodo :: Set HsModule,
  filesDone :: Set HsModule,
  filesExcl :: Set HsModule,
  curFile   :: FilePath,
  inclPath  :: Set FilePath,
  outFile   :: FilePath,
  errors    :: ErrSpec }

data ModInfo = ModInfo {
  modCat    :: ModCategory,
  modSynop  :: [Docu],
  modIntro  :: [Docu],
  modTodo   :: [Docu],
  modSymTab :: FiniteMap DaVar SymInfo,
  modTypTab :: FiniteMap TyCon ConInfo,
  daConXRef :: FiniteMap DaCon TyCon,
  modCtxt   :: FiniteMap TyVar TyCon }

data Docu
  = Words [DWord]
  | Paragraph
  | RefArg DaVar DWord
  | RefVariant DaCon DWord
  | RefSym SymKind DaVar DWord
  | RefTyp ConKind TyCon DWord
  | Verb PackedString DWord
  deriving Show

data SymKind
  = Constructor
  | Method
  | Function
  | Constant
  | Signal
  deriving (Eq,Show)

data SymInfo = SymInfo {
  symKind   :: SymKind,
  symDocu   :: [Docu],
  symType   :: Maybe HType,
  symArgs   :: [[HPat]]}

data ConKind
  = Data
  | Newtype
  | Type
  deriving (Show, Eq)

data ConInfo 
  = ConInfo {
    conModule :: Module,
    conKind   :: ConKind,
    conContxt :: Set TyVar,
    conDaCon  :: FiniteMap DaCon DaConInfo}

data DaConInfo 
  = DaConSimple {
    daConDocu :: [Docu],
    daConType :: [HType] }
  | DaConRecord {
    daConDocu :: [Docu],
    daConType :: [HType],
    daConSel  :: [DaVar] }
  deriving Show

initialState :: State
initialState = State {
  curPos    = -1,
  curModule = nilPS,
  modTab    = emptyFM,
  forward   = emptyFM,
  filesTodo = emptySet,
  filesDone = emptySet,
  filesExcl = emptySet,
  curFile   = "",
  inclPath  = emptySet,
  outFile   = "reference.xml",
  errors    = [] }

