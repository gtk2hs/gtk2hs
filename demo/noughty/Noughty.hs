-- Copyright (c) 2006, Wouter Swierstra
-- All rights reserved.
-- This code is released under the BSD license
--   included in this distribution

-- Imports

import IO
import Maybe
import List
import Graphics.UI.Gtk
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
  window <- windowNew
  window `onDestroy` mainQuit
  set window [ windowTitle := "Noughty"
             , windowResizable := False ]
  label <- labelNew (Just "Player Cross: make your move.")
  vboxOuter <- vBoxNew False 0
  vboxInner <- vBoxNew False 5

-- Add an initial board to the inner vBox and make the menu bar
  (squares, images) <- addFieldsTo vboxInner
  (mb,newGame,quit) <- makeMenuBar

  -- Construct the GUI actions that abstracts from the actual widgets
  gui <- guiActions squares images label

  -- Initialize the state
  state <- newIORef State { board = empty, turn = Cross }
  let modifyState f = readIORef state >>= f >>= writeIORef state

  -- Add action handlers
  onActivateLeaf quit mainQuit
  onActivateLeaf newGame $ modifyState $ reset gui
  zipWithM_ (\square i ->
    onPressed square $ modifyState $ occupy gui i)
    squares [0..8]

  -- Assemble the bits
  set vboxOuter [ containerChild := mb
                , containerChild := vboxInner ]
  set vboxInner [ containerChild := label
                , containerBorderWidth := 10 ]
  set window [ containerChild := vboxOuter ]

  widgetShowAll window
  mainGUI

guiActions buttons images label = do
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
    setStatus = labelSetText label}

makeMenuBar = do
  mb <- menuBarNew
  fileMenu <- menuNew
  newGame <- menuItemNewWithMnemonic "_New Game"
  quit <- menuItemNewWithMnemonic "_Quit"
  file <- menuItemNewWithMnemonic "_Game"
  menuShellAppend fileMenu newGame
  menuShellAppend fileMenu quit
  menuItemSetSubmenu file fileMenu
  containerAdd mb file
  return (mb,newGame,quit)

addFieldsTo container = do
  table <- tableNew 5 5 False
  buttons@[b0,b1,b2,b3,b4,b5,b6,b7,b8] <- replicateM 9 squareNew
  images <- replicateM 9 imageNew
  zipWithM_ containerAdd buttons images
  tableAttachDefaults table b0 0 1 0 1
  tableAttachDefaults table b1 2 3 0 1
  tableAttachDefaults table b2 4 5 0 1
  tableAttachDefaults table b3 0 1 2 3
  tableAttachDefaults table b4 2 3 2 3
  tableAttachDefaults table b5 4 5 2 3
  tableAttachDefaults table b6 0 1 4 5
  tableAttachDefaults table b7 2 3 4 5
  tableAttachDefaults table b8 4 5 4 5
  vline1 <- vSeparatorNew
  vline2 <- vSeparatorNew
  hline1 <- hSeparatorNew
  hline2 <- hSeparatorNew
  tableAttachDefaults table vline1 1 2 0 5
  tableAttachDefaults table vline2 3 4 0 5
  tableAttachDefaults table hline1 0 5 1 2
  tableAttachDefaults table hline2 0 5 3 4
  tableSetRowSpacings table 0
  tableSetColSpacings table 0
  containerAdd container table
  return (buttons, images)

squareNew = do
  square <- buttonNew
  widgetSetSizeRequest square 100 100
  set square [ widgetCanFocus := False, buttonRelief := ReliefNone]
  return square
