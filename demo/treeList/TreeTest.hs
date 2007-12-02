module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.ModelView as New

import qualified Data.Tree as Tree

data Phone = Phone { name :: String, number :: Int, marked :: Bool }

main = do
  initGUI
  Just xml <- xmlNew "TreeTest.glade"
  
  win <- xmlGetWidget xml castToWindow "window"
  onDestroy win mainQuit

  view <- xmlGetWidget xml castToTreeView "view"

  stringValue <- xmlGetWidget xml castToEntry "stringValue"
  intValue    <- xmlGetWidget xml castToSpinButton "intValue"
  boolValue   <- xmlGetWidget xml castToCheckButton "boolValue"

  insertButton  <- xmlGetWidget xml castToButton "insert"
  updateButton  <- xmlGetWidget xml castToButton "update"
  newPath       <- xmlGetWidget xml castToEntry "newPath"
  updatePath    <- xmlGetWidget xml castToEntry "updatePath"

  removeButton  <- xmlGetWidget xml castToButton "remove"
  clearButton   <- xmlGetWidget xml castToButton "clear"
  removePath    <- xmlGetWidget xml castToEntry "removePath"

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

  onClicked insertButton $ do
    value <- getValues
    path <- fmap read $ get newPath entryText
    New.treeStoreInsert store (init path) (last path) value

  onClicked updateButton $ do
    value <- getValues
    path <- fmap read $ get updatePath entryText
    New.treeStoreSetValue store path value
  
  onClicked removeButton $ do
    path <- fmap read $ get removePath entryText
    New.treeStoreRemove store path
    return ()

  onClicked clearButton $ New.treeStoreClear store

  New.treeViewSetReorderable view True

  widgetShowAll win
  mainGUI 

setupView view model = do
  New.treeViewSetHeadersVisible view True

  -- add three columns
  col1 <- New.treeViewColumnNew
  col2 <- New.treeViewColumnNew
  col3 <- New.treeViewColumnNew

  New.treeViewColumnSetTitle col1 "String column"
  New.treeViewColumnSetTitle col2 "Int column"
  New.treeViewColumnSetTitle col3 "Bool column"

  renderer1 <- New.cellRendererTextNew
  renderer2 <- New.cellRendererTextNew
  renderer3 <- New.cellRendererToggleNew

  New.cellLayoutPackStart col1 renderer1 True 
  New.cellLayoutPackStart col2 renderer2 True
  New.cellLayoutPackStart col3 renderer3 True

  New.cellLayoutSetAttributes col1 renderer1 model $ \row -> [ New.cellText := name row ]
  New.cellLayoutSetAttributes col2 renderer2 model $ \row -> [ New.cellText := show (number row) ]
  New.cellLayoutSetAttributes col3 renderer3 model $ \row -> [ New.cellToggleActive := marked row ]

  New.treeViewAppendColumn view col1
  New.treeViewAppendColumn view col2 
  New.treeViewAppendColumn view col3

storeImpl =
  New.treeStoreNew
    [Tree.Node {
       Tree.rootLabel = Phone { name = "Foo", number = 1, marked = False },
       Tree.subForest = [leafNode Phone { name = "Bar", number = 11, marked = True  }
                        ,leafNode Phone { name = "Baz", number = 12, marked = False }]
     },
     Tree.Node {
       Tree.rootLabel = Phone { name = "Foo", number = 2, marked = False },
       Tree.subForest = [leafNode Phone { name = "Bar", number = 21, marked = True  }
                        ,leafNode Phone { name = "Baz", number = 22, marked = False }]
     },
     Tree.Node {
       Tree.rootLabel = Phone { name = "Foo", number = 3, marked = False },
       Tree.subForest = [leafNode Phone { name = "Bar", number = 31, marked = True  }
                        ,leafNode Phone { name = "Baz", number = 32, marked = False }]
     }]
  where leafNode a = Tree.Node { Tree.rootLabel = a, Tree.subForest = [] }
