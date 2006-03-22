module Main where

import Graphics.UI.Gtk hiding (treeStoreNew)

import qualified Data.Tree as Tree

import Graphics.UI.Gtk.TreeList.TreeStoreAxel (treeStoreNew)

main = do
  initGUI
  win <- windowNew
  win `onDestroy` mainQuit

  -- create a new TreeView Widget
  model <- storeImpl
  view <- treeViewNewWithModel model
  treeViewSetHeadersVisible view True
  containerAdd win view

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
  
  widgetShowAll win
  mainGUI 

data Phone = Phone { name :: String, number :: Int, marked :: Bool }

storeImpl =
  treeStoreNew
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
