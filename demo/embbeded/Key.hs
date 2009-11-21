module Key where

import Data.Map (Map)

import Text.Regex.TDFA
import Data.Maybe

import qualified Data.Map as M

-- | The key modifier for transform event.
data Modifier = MShift | MCtrl | MMeta | MSuper
                deriving (Show,Eq,Ord)

-- | The key type.
data Key = KEsc | KFun Int | KPrtScr | KPause | KASCII Char | KBS | KIns
         | KHome | KEnd | KPageUp | KPageDown | KDel | KNP5 | KUp | KMenu
         | KLeft | KDown | KRight | KEnter | KTab 
           deriving (Eq,Show,Ord)

type Keytable = Map String Key

-- | Map GTK long names to Keys
keyTable :: Keytable
keyTable = M.fromList
    [("Down",         KDown)
    ,("Up",           KUp)
    ,("Left",         KLeft)
    ,("Right",        KRight)
    ,("Home",         KHome)
    ,("End",          KEnd)
    ,("BackSpace",    KBS)
    ,("Delete",       KDel)
    ,("Page_Up",      KPageUp)
    ,("Page_Down",    KPageDown)
    ,("Insert",       KIns)
    ,("Escape",       KEsc)
    ,("Return",       KEnter)
    ,("Tab",          KTab)
    ,("ISO_Left_Tab", KTab)]

-- | Lookup key name from key table.
keyLookup :: String -> Maybe Key
keyLookup keyName = 
  case key of
    Just k  -> Just k             -- control key
    Nothing -> if keyName =~ "^F[0-9]+$" :: Bool 
              then Just $ KFun (read (tail keyName) :: Int) -- function key
              else Nothing                                 -- other key
  where
    key = M.lookup keyName keyTable
