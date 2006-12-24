module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as New

import qualified Data.Tree as Tree


main = do
  initGUI
  win <- windowNew
  win `onDestroy` mainQuit

  -- create a new TreeView Widget
  model <- storeImpl
  view <- New.treeViewNewWithModel model
  New.treeViewSetHeadersVisible view True
  containerAdd win view

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
  New.cellLayoutSetAttributes col3 renderer3 model $ \row -> [ New.cellActive := marked row ]
  New.treeViewColumnSetTitle col3 "Check box column"
  New.treeViewAppendColumn view col3
  
  widgetShowAll win
  mainGUI 

data Phone = Phone { name :: String, number :: Int, marked :: Bool }

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
