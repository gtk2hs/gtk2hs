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
  win `containerAdd` tv

  -- add a single column
  tvc <- newTreeViewColumn
  treeViewAppendColumn tv tvc

  -- create a single text renderer within this column
  tRen <- treeViewColumnNewText tvc True True
  tvc `treeViewColumnSetTitle` "My Title"
  treeViewColumnAssociate tRen attrs

  -- fill the list with some entries
  mapM_ (\txt -> do
    iter <- listStoreAppend store
    w iter txt (Just "red") Nothing) 
    ["Hello", "how", "are", "you"]

  tv `onButtonPress` showMenu tv

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

  
showMenu :: TreeView -> Event -> IO Bool
showMenu tv (Button { x=xPos, 
		      y=yPos, 
		      click=SingleClick, 
		      button=RightButton }) = do
  res <- treeViewGetPathAtPos tv (round xPos, round yPos)
  case res of
    Nothing -> return ()
    (Just (tp, ti, _)) -> do
      putStrLn ("right click in cell "++(show tp))
  return True
-- let Gtk handle normal button clicks
showMenu tv _ = return False
