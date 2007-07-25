module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as New

import qualified Data.Tree as Tree

data Phone = Phone { name :: String, number :: Int, marked :: Bool }

main = do
  initGUI

  win <- windowNew
  onDestroy win mainQuit

  -- create a new tree model
  model <- storeImpl
  view <- New.treeViewNewWithModel model

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
  
  containerAdd win view
  widgetShowAll win
  mainGUI 

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
