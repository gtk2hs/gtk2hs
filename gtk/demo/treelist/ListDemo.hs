module Main where

import Control.Monad.IO.Class
import Graphics.UI.Gtk
import System.Glib.Signals (on)
import Data.List ( isPrefixOf )
import Data.Char ( toLower )

data Phone = Phone { name :: String, number :: Int, marked :: Bool }

main = do
  initGUI

  win <- windowNew
  on win objectDestroy mainQuit

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
  cellLayoutSetAttributes col3 renderer3 model $ \row -> [ cellToggleActive := marked row ]

  treeViewAppendColumn view col1
  treeViewAppendColumn view col2
  treeViewAppendColumn view col3

  -- update the model when the toggle buttons are activated
  on renderer3 cellToggled $ \pathStr -> do
    let (i:_) = stringToTreePath pathStr
    val <- listStoreGetValue model i
    listStoreSetValue model i val { marked = not (marked val) }


  -- enable interactive search
  treeViewSetEnableSearch view True
  treeViewSetSearchEqualFunc view $ Just $ \str iter -> do
    (i:_) <- treeModelGetPath model iter
    row <- listStoreGetValue model i
    return (map toLower str `isPrefixOf` map toLower (name row))

  containerAdd win view
  widgetShowAll win
  mainGUI
