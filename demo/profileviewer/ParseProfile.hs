-- Copyright (c) 2004 Duncan Coutts
-- This library is liscenced under the GNU General Public License version 2
-- or (at your option) any later version.

-- This is a not-terribly-clever parser for ghc's time profile log files.

module ParseProfile (
  Profile(..),
  ProfileNode(..),
  parseProfileFile,
  pruneOnThreshold
) where

import Char
import Maybe (catMaybes)

data Profile = Profile {
    title :: String,
    command :: String,
    totalTime :: Float,
    totalAlloc :: Integer,   --can be several GB
    breakdown :: ProfileNode
  }

data ProfileNode = ProfileNode {
    costCentre :: String,
    moduleName :: String,
    entries :: !Int,
    individualTime  :: !Int, --scaled by 10
    individualAlloc :: !Int, --scaled by 10
    inheritedTime   :: !Int, --scaled by 10
    inheritedAlloc  :: !Int, --scaled by 10
    children :: [ProfileNode]
  }

pruneOnThreshold :: Int -> ProfileNode -> Maybe ProfileNode
pruneOnThreshold threshold node
  | inheritedTime node >= threshold
 || inheritedAlloc node >= threshold =
    let children' = catMaybes $ map (pruneOnThreshold threshold) (children node)
     in Just $ node { children = children' }
  | otherwise = Nothing


parseProfileFile :: String -> IO Profile
parseProfileFile filename = do
  content <- readFile filename
  let (titleLine:_:commandLine:_:timeLine:allocLine:theRest) = lines content
      profileDetail = dropWhile (\line -> take 4 line /= "MAIN") theRest
  return $ Profile {
      title = dropWhile isSpace titleLine,
      command = dropWhile isSpace commandLine,
      totalTime = read $ words timeLine !! 3,
      totalAlloc = read $ filter (/=',') $ words allocLine !! 3,
      breakdown = parseProfile profileDetail
    }

-- intermediate form
data ProfileEntry = ProfileEntry {
    depth :: !Int,
    ecostCentre :: String,
    emoduleName :: String,
    eentries :: !Int,
    eindividualTime  :: !Int, --scaled by 10
    eindividualAlloc :: !Int, --scaled by 10
    einheritedTime   :: !Int, --scaled by 10
    einheritedAlloc  :: !Int  --scaled by 10
  }

parseProfile :: [String] -> ProfileNode
parseProfile file =
  case (profileEntriesToProfile [] 0 . map parseProfileEntry) file of
    ([profile],[]) -> profile
    _ -> error "multiple top level entries"

parseProfileEntry :: String -> ProfileEntry
parseProfileEntry line =
  let depth = length (takeWhile (==' ') line)
   in case words line of
        [costCentre, moduleName, _, entries,
         individualTime, individualAlloc,
         inheritedTime, inheritedAlloc] ->
          ProfileEntry {
            depth = depth,
            ecostCentre = costCentre,
            emoduleName = moduleName,
            eentries = read entries,
            eindividualTime  = floor $ (read individualTime) * 10,
            eindividualAlloc = floor $ (read individualAlloc) * 10,
            einheritedTime  = floor $ (read inheritedTime) * 10,
            einheritedAlloc = floor $ (read inheritedAlloc) * 10 
          }
        _ -> error $ "bad profile line:\n\t" ++ line

profileEntriesToProfile :: [ProfileNode] -> Int -> [ProfileEntry] -> ([ProfileNode], [ProfileEntry])
profileEntriesToProfile acum curDepth [] = (acum, [])
profileEntriesToProfile acum curDepth (entry:entries)
  | depth entry == curDepth =
     let (children, remaining) = profileEntriesToProfile
                                   [] (depth entry + 1) entries
         curNode = ProfileNode {
             costCentre = ecostCentre entry,
             moduleName = emoduleName entry,
             entries    = eentries entry,
             individualTime  = eindividualTime entry,
             individualAlloc = eindividualAlloc entry,
             inheritedTime  = einheritedTime entry,
             inheritedAlloc = einheritedAlloc entry,
             children = children
           }
      in profileEntriesToProfile (curNode:acum) (depth entry) remaining
  | depth entry < curDepth = (acum, entry:entries)  --we're done for this level
  | otherwise = error "bad indentation in file"
