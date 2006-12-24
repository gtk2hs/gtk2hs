module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as New

data Phone = Phone { name :: String, number :: Int, marked :: Bool }

main = do
  initGUI

  win <- windowNew
  onDestroy win mainQuit

  -- create a new list model
  model <- New.listStoreNew
    [Phone { name = "Foo", number = 12345, marked = False }
    ,Phone { name = "Bar", number = 67890, marked = True  }
    ,Phone { name = "Baz", number = 39496, marked = False }]
  view <- New.treeViewNewWithModel model

  New.treeViewSetHeadersVisible view True

  -- add a couple columns
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
  New.cellLayoutSetAttributes col3 renderer3 model $ \row -> [ New.cellActive := marked row ]

  New.treeViewAppendColumn view col1
  New.treeViewAppendColumn view col2
  New.treeViewAppendColumn view col3

  containerAdd win view
  widgetShowAll win
  mainGUI 
