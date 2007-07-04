-- Copyright (c) 2006, Wouter Swierstra
-- All rights reserved.
-- This code is released under the BSD license
--   included in this distribution

-- Imports

import IO
import Maybe
import List
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Data.IORef
import Control.Monad

-- Players, boards and some useful pure functions

data Player =  Nought | Blank | Cross deriving (Ord, Eq, Show)

next        :: Player -> Player
next Nought =  Cross
next Blank  =  Blank
next Cross  =  Nought

type Board =  [[Player]]

size :: Int
size =  3

empty :: Board
empty =  replicate size (replicate size Blank)

move       :: Int -> Player -> Board -> Maybe Board
move n p b = case y of
	       Blank     -> Just (chop size (xs ++ (p : ys)))
	       _         -> Nothing
	       where
                 (xs,y:ys) = splitAt n (concat b)

chop      :: Int -> [a] -> [[a]]
chop n [] =  []
chop n xs =  take n xs : chop n (drop n xs)

diag 	 :: [[a]] -> [a]
diag xss =  [xss !! n !! n | n <- [0 .. length xss - 1]]

full   :: Board -> Bool
full b =  all (all (/= Blank)) b

wins	 :: Player -> Board -> Bool
wins p b =  any (all (== p)) b
	    || any (all (== p)) (transpose b)
	    || all (== p) (diag b)
	    || all (== p) (diag (reverse b))

won   :: Board -> Bool
won b =  wins Nought b || wins Cross b

-- The state and GUI

data State = State {
  board :: Board,
  turn :: Player
}

data GUI = GUI {
  disableBoard :: IO (),
  resetBoard :: IO (),
  setSquare :: Int -> Player -> IO (),
  setStatus :: String -> IO ()
}

-- reset the game
reset gui (State board turn) = do
  setStatus gui "Player Cross: make your move."
  resetBoard gui
  return (State empty Cross)

-- when a square is clicked on, try to make a move.
--   if the square is already occupied, nothing happens
--   otherwise, update the board, let the next player make his move,
--     and check whether someone has won or the board is full.
occupy gui square st@(State board player) = do
  case move square player board of
    Nothing -> return st
    Just newBoard -> do
      setSquare gui square player
      handleMove gui newBoard player
      return (State newBoard (next player))

-- check whether a board is won or full
handleMove gui board player
  | wins player board = do
      setStatus gui ("Player "  ++ show player ++ " wins!")
      disableBoard gui
  | full board = do
      setStatus gui "It's a draw."
      disableBoard gui
  | otherwise = do
      setStatus gui ("Player " ++ show (next player) ++ ": make your move")

main = do
  initGUI

  -- Extract widgets from the glade xml file
  Just xml <- xmlNew "noughty.glade"

  window <- xmlGetWidget xml castToWindow "window"
  window `onDestroy` mainQuit

  newGame <- xmlGetWidget xml castToMenuItem "newGame"
  quit <- xmlGetWidget xml castToMenuItem "quit"

  squares <- flip mapM [1..9] $ \n -> do
    square <- xmlGetWidget xml castToButton ("button" ++ show n)
    -- we set this in the glde file but it doesn't seem to work there.
    set square [ widgetCanFocus := False ]
    return square

  images <- flip mapM [1..9] $ \n -> do
    xmlGetWidget xml castToImage ("image" ++ show n)

  statusbar <- xmlGetWidget xml castToStatusbar "statusbar"
  ctx <- statusbarGetContextId statusbar "state"
  statusbarPush statusbar ctx "Player Cross: make your move."

  -- Construct the GUI actions that abstracts from the actual widgets
  gui <- guiActions squares images statusbar ctx

  -- Initialize the state
  state <- newIORef State { board = empty, turn = Cross }
  let modifyState f = readIORef state >>= f >>= writeIORef state

  -- Add action handlers
  onActivateLeaf quit mainQuit
  onActivateLeaf newGame $ modifyState $ reset gui
  zipWithM_ (\square i ->
    onPressed square $ modifyState $ occupy gui i)
    squares [0..8]

  widgetShowAll window
  mainGUI

guiActions buttons images statusbar ctx = do
  noughtPic <- pixbufNewFromFile "Nought.png"
  crossPic  <- pixbufNewFromFile "Cross.png"
  return GUI {
    disableBoard = mapM_ (flip widgetSetSensitivity False) buttons,
    resetBoard = do
      mapM_ (\i -> imageClear i >> widgetQueueDraw i) images
      mapM_ (flip widgetSetSensitivity True) buttons,
    setSquare = \ i player ->
      case player of
        Cross -> set (images !! i) [ imagePixbuf := crossPic ]
        Nought-> set (images !! i) [ imagePixbuf := noughtPic ],
    setStatus = \msg -> do
            statusbarPop statusbar ctx
            statusbarPush statusbar ctx msg
            return ()
   }
