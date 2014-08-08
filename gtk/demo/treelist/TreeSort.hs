import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as New
import Data.Tree

main = do
  initGUI
  win <- windowNew
  -- Create a tree model with some unsorted data.
  rawmodel <- New.treeStoreNew
   [Node ("zoo",8) [], Node ("foo",5) [],
    Node ("bar",20) [], Node ("baz",2) []]

  -- Create a sorting proxy model, that is, a model that permutates the
  -- rows of a different model such that they appear to be sorted.
  model <- New.treeModelSortNewWithModel rawmodel

  -- Define two sorting functions, one being the default sorting function and
  -- the other one being the sorting function for the 'SortColumnId' 2.
  -- 'SortColumnId's are arbitrary positive numbers, i.e., we could have chosen
  -- any other unique number.
  New.treeSortableSetDefaultSortFunc model $ Just $ \iter1 iter2 -> do
    (t1,_) <- New.treeModelGetRow rawmodel iter1
    (t2,_) <- New.treeModelGetRow rawmodel iter2
    return (compare t1 t2)
  New.treeSortableSetSortFunc model 2 $ \iter1 iter2 -> do
    (_,n1) <- New.treeModelGetRow rawmodel iter1
    (_,n2) <- New.treeModelGetRow rawmodel iter2
    return (compare n1 n2)

  -- Create the view.
  view <- New.treeViewNewWithModel model

  -- Create and insert two columns, one with the heading Name, one with the
  -- heading Number. Associate the 'SortColumnId' 2 with the latter column such
  -- that clicking on the Number header will sort the rows by the numbers.
  col <- New.treeViewColumnNew
  New.treeViewColumnSetTitle col "Name"
  rend <- New.cellRendererTextNew
  New.cellLayoutPackStart col rend True
  New.cellLayoutSetAttributeFunc col rend model $ \iter -> do
   cIter <- New.treeModelSortConvertIterToChildIter model iter
   (n,_) <- New.treeModelGetRow rawmodel cIter
   set rend [New.cellText := n]
  New.treeViewAppendColumn view col

  col' <- New.treeViewColumnNew
  New.treeViewColumnSetTitle col' "Number"
  rend <- New.cellRendererTextNew
  New.cellLayoutPackStart col' rend True
  New.cellLayoutSetAttributeFunc col' rend model $ \iter -> do
   cIter <- New.treeModelSortConvertIterToChildIter model iter
   (_,c) <- New.treeModelGetRow rawmodel cIter
   set rend [New.cellText := show c]
  New.treeViewAppendColumn view col'
  New.treeViewColumnSetSortColumnId col' 2

  -- Create a button that shows information on the current state of the sorting
  -- settings.
  button <- buttonNewWithLabel "Dump Info"
  on button buttonActivated $ do
    sId <- New.treeViewColumnGetSortColumnId col
    putStrLn ("tvc1 sort id is "++show sId)
    sId <- New.treeViewColumnGetSortColumnId col'
    putStrLn ("tvc2 sort id is "++show sId)
    sId <- New.treeSortableGetSortColumnId model
    putStrLn ("sort id is "++show sId)
    -- Show all entries of the proxy model
    let recurse Nothing = return ()
        recurse (Just iter) = do
          cIter <- New.treeModelSortConvertIterToChildIter model iter
          row <- New.treeModelGetRow rawmodel cIter
          putStrLn ("iter "++show cIter++": "++show row)
          mIter <- New.treeModelIterNext model iter
          recurse mIter
    mIter <- New.treeModelGetIterFirst model
    recurse mIter

  -- Put it all together.
  vBox <- vBoxNew False 3
  -- boxPackStartDefaults vBox view
  boxPackStart vBox view PackRepel 0
  boxPackEnd vBox button PackNatural 0
  containerAdd win vBox
  widgetShowAll win
  on win objectDestroy mainQuit
  mainGUI
