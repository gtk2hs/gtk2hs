-- a demo that shows how to create a normal tree view and a tree view in
-- which only a chosen subset of rows are shown (namely those with upper case letters)
module Main ( main ) where

import Graphics.UI.Gtk
import Data.List
import Data.Char
import Debug.Trace

-- | Define a virtual column of the model that determines the visibility of a row in
--   the model.
visCol :: ColumnId String Bool
visCol = makeColumnIdBool 0

main = do
  initGUI

  win <- windowNew
  onDestroy win mainQuit

  content <- readFile "FilterDemo.hs"

  -- create a view that shows all lines
  model <- listStoreNew (lines content)
  viewAll <- treeViewNewWithModel model
  col <- treeViewColumnNew
  ren <- cellRendererTextNew
  cellLayoutPackStart col ren True
  cellLayoutSetAttributes col ren model $ \row -> [ cellText := row ]
  treeViewAppendColumn viewAll col

  -- create a view that only shows lines with upper case characters
  fModel <- treeModelFilterNew model []

  -- create a virtual column 'visCol' that contains @True@ if a certain row has
  -- upper case letters. Then set this column to determine the visibility of a row.
  customStoreSetColumn model visCol (any isUpper)
  treeModelFilterSetVisibleColumn fModel visCol

{-
  -- this is an alternative way to determine the visibility of a row. In this case,
  -- it is not necessary to create the column 'visCol'.
  treeModelFilterSetVisibleFunc fModel $ Just $ \iter -> do
     row <- treeModelGetRow model iter
     return (any isUpper row)
-}
  -- note: it is important to insert the model into the view after the visibility
  -- row or the visibility function have been set. Otherwise, the view is filled
  -- first and setting a new visibility column/function will not update the view.
  viewFew <- treeViewNewWithModel fModel
  col <- treeViewColumnNew
  ren <- cellRendererTextNew
  cellLayoutPackStart col ren True
  cellLayoutSetAttributes col ren model $ \row -> [ cellText := row ]

  treeViewAppendColumn viewFew col



  box <- vBoxNew False 0
  swAll <- scrolledWindowNew Nothing Nothing
  containerAdd swAll viewAll
  boxPackStart box swAll PackGrow 4

  swFew <- scrolledWindowNew Nothing Nothing
  containerAdd swFew viewFew
  boxPackEnd box swFew PackGrow 4

  containerAdd win box
  widgetShowAll win
  mainGUI
