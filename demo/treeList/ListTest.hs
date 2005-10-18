-- Test file for the ListView widget.
module Main(main) where

import Graphics.UI.Gtk.Mogul
import Graphics.UI.Gtk.Gdk.Events

main = do
  initGUI
  win <- newWindow
  win `onDestroy` mainQuit

  -- create a new TextView Widget
  (store, r, w, attrs) <- createStore
  tv <- newTreeViewWithModel store
  tv `treeViewSetHeadersVisible` True
  sel <- treeViewGetSelection tv
  sel `treeSelectionSetMode` SelectionMultiple
  win `containerAdd` tv

  -- add a single column
  tvc <- newTreeViewColumn
  treeViewAppendColumn tv tvc

  -- create a single text renderer within this column
  tRen <- treeViewColumnNewText tvc True True
  tvc `treeViewColumnSetTitle` "My Title"
  treeViewColumnAssociate tRen attrs

  -- set an attribute on the renderer that applies to all the rows, ie it's
  -- global rather than being determined on a per-row bssis by the data model
  cellRendererSetAttribute tRen cellEditable (Just True)

  -- fill the list with some entries
  mapM_ (\txt -> do
    iter <- listStoreAppend store
    w iter txt (Just "red") Nothing) 
    ["Hello", "how", "are", "you"]

  tv `onButtonPress` showMenu tv store sel

  -- show the widget and run the main loop
  widgetShow tv
  widgetShow win
  mainGUI 


createStore = do
  -- Start by creating a description of how the database (the store) looks
  -- like. We call this descripton the skeleton of the store.
  skel <- emptyListSkel
  (tAttr, tRead, tWrite) <- listSkelAddAttribute skel cellText
  (fAttr, fRead, fWrite) <- listSkelAddAttribute skel cellForeground
  (bAttr, bRead, bWrite) <- listSkelAddAttribute skel cellBackground
  
  -- Now create the real store from this skeleton. The skeleton is of no use
  -- after this function call.
  store <- newListStore skel

  -- Return this store and read and write functions that read and write a
  -- whole line in the store.

  let writeStore :: TreeIter -> String -> Maybe String -> Maybe String -> IO ()
      writeStore iter txt fore back = do
        tWrite iter txt
	fWrite iter fore
	bWrite iter back
  let readStore :: TreeIter -> IO (String, Maybe String, Maybe String)
      readStore iter = do
	txt  <- tRead iter
	fore <- fRead iter
	back <- bRead iter
	return (txt, fore, back)
  return (store, readStore, writeStore, [tAttr, fAttr, bAttr])

  
showMenu :: TreeView -> ListStore -> TreeSelection -> Event -> IO Bool
showMenu tv tm sel (Button { eventX=xPos, 
			     eventY=yPos, 
			     eventClick=SingleClick, 
			     eventButton=RightButton }) = do
  res <- treeViewGetPathAtPos tv (round xPos, round yPos)
  case res of
    Nothing -> return ()
    (Just (tp, ti, _)) -> do
      putStrLn ("right click in cell "++(show tp))

  -- first way to get the selected rows
  treeSelectionSelectedForeach sel $ \ti -> do
    path <- treeModelGetPath tm ti
    putStrLn ("selected: "++show path)

  -- second way to get the selected rows
  rows <- treeSelectionGetSelectedRows sel
  putStrLn ("selected rows: "++show rows)

  return True

-- let Gtk handle normal button clicks
showMenu tv _ _ _ = return False
