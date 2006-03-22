module Main where

import Graphics.UI.Gtk hiding (
  listStorePrepend,
  listStoreAppend,
  listStoreInsert,
  listStoreSetValue,
  listStoreRemove,
  listStoreClear,
  listStoreNew
  )

import Graphics.UI.Gtk.TreeList.ListStoreNew
import Graphics.UI.Gtk.TreeList.CellLayout


data Phone = Phone { name :: String, number :: Int, marked :: Bool }

main = do
  initGUI

  win <- windowNew
  onDestroy win mainQuit

  -- create a new list model
  model <- listStoreNew
    [Phone { name = "Foo", number = 12345, marked = False }
    ,Phone { name = "Bar", number = 67890, marked = True  }
    ,Phone { name = "Baz", number = 39496, marked = False }]
  view <- treeViewNewWithModel model

  treeViewSetHeadersVisible view True

  -- add a couple columns
  col1 <- treeViewColumnNew
  col2 <- treeViewColumnNew
  col3 <- treeViewColumnNew

  treeViewColumnSetTitle col1 "String column"
  treeViewColumnSetTitle col2 "Int column"
  treeViewColumnSetTitle col3 "Bool column"

  renderer1 <- cellRendererTextNew
  renderer2 <- cellRendererTextNew
  renderer3 <- cellRendererToggleNew

  cellLayoutPackStart col1 renderer1 True
  cellLayoutPackStart col2 renderer2 True
  cellLayoutPackStart col3 renderer3 True

  cellLayoutSetAttributes col1 renderer1 model $ \row -> [ cellText := name row ]
  cellLayoutSetAttributes col2 renderer2 model $ \row -> [ cellText := show (number row) ]
  cellLayoutSetAttributes col3 renderer3 model $ \row -> [ cellActive := marked row ]

  treeViewAppendColumn view col1
  treeViewAppendColumn view col2
  treeViewAppendColumn view col3

  containerAdd win view
  widgetShowAll win
  mainGUI 
