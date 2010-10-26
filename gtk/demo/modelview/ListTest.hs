module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.ModelView as New

data Phone = Phone { name :: String, number :: Int, marked :: Bool }

main = do
  initGUI
  Just xml <- xmlNew "ListTest.glade"

  win <- xmlGetWidget xml castToWindow "window"
  onDestroy win mainQuit

  view <- xmlGetWidget xml castToTreeView "view"

  stringValue <- xmlGetWidget xml castToEntry "stringValue"
  intValue    <- xmlGetWidget xml castToSpinButton "intValue"
  boolValue   <- xmlGetWidget xml castToCheckButton "boolValue"

  insertButton  <- xmlGetWidget xml castToButton "insert"
  prependButton <- xmlGetWidget xml castToButton "prepend"
  appendButton  <- xmlGetWidget xml castToButton "append"
  updateButton  <- xmlGetWidget xml castToButton "update"
  newIndex      <- xmlGetWidget xml castToSpinButton "newIndex"
  updateIndex   <- xmlGetWidget xml castToSpinButton "updateIndex"

  removeButton  <- xmlGetWidget xml castToButton "remove"
  clearButton   <- xmlGetWidget xml castToButton "clear"
  removeIndex   <- xmlGetWidget xml castToSpinButton "removeIndex"

  -- create a new list store
  store <- storeImpl
  New.treeViewSetModel view store
  setupView view store

  let getValues = do
        name   <- entryGetText stringValue
        number <- spinButtonGetValue intValue
        marked <- toggleButtonGetActive boolValue
        return Phone {
          name = name,
          number = floor number,
          marked = marked
        }

  onClicked prependButton $ getValues >>= New.listStorePrepend store
  onClicked appendButton $ getValues >>= New.listStoreAppend store >> return ()

  onClicked insertButton $ do
    value <- getValues
    index <- fmap floor $ spinButtonGetValue newIndex
    New.listStoreInsert store index value

  onClicked updateButton $ do
    value <- getValues
    index <- fmap floor $ spinButtonGetValue updateIndex
    New.listStoreSetValue store index value

  onClicked removeButton $ do
    index <- fmap floor $ spinButtonGetValue removeIndex
    New.listStoreRemove store index

  onClicked clearButton $ New.listStoreClear store

  New.treeViewSetReorderable view True

--  containerAdd win view
  widgetShowAll win
  mainGUI

setupView view model = do
  New.treeViewSetHeadersVisible view True

  -- add a couple columns
  renderer1 <- New.cellRendererTextNew
  col1 <- New.treeViewColumnNew
  New.treeViewColumnPackStart col1 renderer1 True
  New.cellLayoutSetAttributes col1 renderer1 model $ \row -> [ New.cellText := name row ]
  New.treeViewColumnSetTitle col1 "String column"
  New.treeViewAppendColumn view col1

  renderer2 <- New.cellRendererTextNew
  col2 <- New.treeViewColumnNew
  New.treeViewColumnPackStart col2 renderer2 True
  New.cellLayoutSetAttributes col2 renderer2 model $ \row -> [ New.cellText := show (number row) ]
  New.treeViewColumnSetTitle col2 "Int column"
  New.treeViewAppendColumn view col2

  renderer3 <- New.cellRendererToggleNew
  col3 <- New.treeViewColumnNew
  New.treeViewColumnPackStart col3 renderer3 True
  New.cellLayoutSetAttributes col3 renderer3 model $ \row -> [ New.cellToggleActive := marked row ]
  New.treeViewColumnSetTitle col3 "Check box column"
  New.treeViewAppendColumn view col3

storeImpl =
  New.listStoreNew
    [Phone { name = "Foo", number = 12345, marked = False }
    ,Phone { name = "Bar", number = 67890, marked = True  }
    ,Phone { name = "Baz", number = 39496, marked = False }]
