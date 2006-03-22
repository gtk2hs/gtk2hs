module Main where

import qualified Data.Tree as Tree

import Graphics.UI.Gtk hiding (
  listStorePrepend,
  listStoreAppend,
  listStoreInsert,
  listStoreSetValue,
  listStoreRemove,
  listStoreClear,
  listStoreNew
  )
import Graphics.UI.Gtk.Glade

import Graphics.UI.Gtk.TreeList.ListStoreNew

main = do
  initGUI
  Just xml <- xmlNew "ListTest.glade"
  
  win <- xmlGetWidget xml castToWindow "window"
  win `onDestroy` mainQuit

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
  treeViewSetModel view store
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

  onClicked prependButton $ getValues >>= listStorePrepend store
  onClicked appendButton $ getValues >>= listStoreAppend store

  onClicked insertButton $ do
    value <- getValues
    index <- fmap floor $ spinButtonGetValue newIndex
    listStoreInsert store index value

  onClicked updateButton $ do
    value <- getValues
    index <- fmap floor $ spinButtonGetValue updateIndex
    listStoreSetValue store index value
  
  onClicked removeButton $ do
    index <- fmap floor $ spinButtonGetValue removeIndex
    listStoreRemove store index

  onClicked clearButton $ listStoreClear store
  
--  containerAdd win view
  widgetShowAll win
  mainGUI 

setupView view model = do
  treeViewSetHeadersVisible view True

  -- add a couple columns
  renderer1 <- cellRendererTextNew
  col1 <- treeViewColumnNew
  treeViewColumnPackStart col1 renderer1 True
  cellLayoutSetAttributes col1 renderer1 model $ \row -> [ cellText := name row ]
  treeViewColumnSetTitle col1 "String column"
  treeViewAppendColumn view col1

  renderer2 <- cellRendererTextNew
  col2 <- treeViewColumnNew
  treeViewColumnPackStart col2 renderer2 True
  cellLayoutSetAttributes col2 renderer2 model $ \row -> [ cellText := show (number row) ]
  treeViewColumnSetTitle col2 "Int column"
  treeViewAppendColumn view col2

  renderer3 <- cellRendererToggleNew
  col3 <- treeViewColumnNew
  treeViewColumnPackStart col3 renderer3 True
  cellLayoutSetAttributes col3 renderer3 model $ \row -> [ cellActive := marked row ]
  treeViewColumnSetTitle col3 "Check box column"
  treeViewAppendColumn view col3

data Phone = Phone { name :: String, number :: Int, marked :: Bool }

storeImpl =
  listStoreNew
    [Phone { name = "Foo", number = 12345, marked = False }
    ,Phone { name = "Bar", number = 67890, marked = True  }
    ,Phone { name = "Baz", number = 39496, marked = False }]
