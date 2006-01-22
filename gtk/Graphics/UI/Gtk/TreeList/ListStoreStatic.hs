module Graphics.UI.Gtk.TreeList.ListStoreStatic (
  listStoreNewStatic
  ) where

import Data.Word  (Word)
import Data.Array

import Graphics.UI.Gtk.TreeList.TreeModel
import Graphics.UI.Gtk.TreeList.CustomStore
import Graphics.UI.Gtk.TreeList.TreeIter
import Graphics.UI.Gtk.TreeList.Column

listStoreNewStatic :: [Column a] -> [a] -> IO TreeModel
listStoreNewStatic rs xs = do
  let size :: Word
      size = fromIntegral (length xs)
      rows = listArray (0, size - 1) xs
      cols = listArray (0, length rs - 1) rs

  customStoreNew $ 
    CustomStore {
      customStoreGetFlags      = return [TreeModelListOnly],
      customStoreGetNColumns   = return (length rs),
      customStoreGetColumnType = \n -> return (columnGType (cols ! n)),
      customStoreGetIter       = \[n] -> return (Just (TreeIter 0 (fromIntegral n) 0 0)),
      customStoreGetPath       = \(TreeIter _ n _ _) -> return [fromIntegral n],
      customStoreGetValue      = \(TreeIter _ n _ _) i gvalue -> columnSetGValue (cols ! i) (rows ! n) gvalue,
      customStoreIterNext      = \(TreeIter _ n _ _) ->
                                    if n >= size - 1
                                      then return Nothing
                                      else return (Just (TreeIter 0 (n+1) 0 0)),
      customStoreIterChildren  = \_ -> return Nothing,
      customStoreIterHasChild  = \_ -> return False,
      customStoreIterNChildren = \index -> case index of
                                             Nothing -> return (fromIntegral size)
                                             _       -> return 0,
      customStoreIterNthChild  = \index n -> case index of
                                               Nothing -> return (Just (TreeIter 0 (fromIntegral n) 0 0))
                                               _       -> return Nothing,
      customStoreIterParent    = \_ -> return Nothing,
      customStoreRefNode       = \_ -> return (),
      customStoreUnrefNode     = \_ -> return ()
    }
