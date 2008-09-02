module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as New
import System.Glib.GObject ( toGObject )
import System.Glib.Signals ( on )
import System.FilePath
import Control.Concurrent.MVar
import Control.Monad ( liftM )
import Control.Monad.Trans ( liftIO )
import Data.Maybe ( fromMaybe )
import Data.List ( findIndex )

roomStrCol :: ColumnId String String
roomStrCol = makeColumnIdString 1

-- Define a string column and an image column on the store holding the
-- computer types.
compPicCol :: ColumnId CompType Pixbuf
compPicCol = makeColumnIdPixbuf 1

compStrCol :: ColumnId CompType String
compStrCol = makeColumnIdString 2

data Computer = Computer {
	name :: String,
	addr :: (Int, Int, Int, Int),
	roomStore  :: New.ListStore String,
	roomSel :: Int,
        cType :: CompType }

data CompType
  = MacBookPro
  | MacBook
  | Printer
  | MacPro
  | Xserve
  | IMac
  deriving (Enum, Bounded, Show)

showCT :: CompType -> String
showCT ct = case show ct of
  'I':xs -> 'i':xs
  xs -> xs

main = do
  initGUI

  win <- windowNew
  onDestroy win mainQuit

  -- create a tag that we use as selection, target and selection type
  compTypeTag <- tagNew "_CompType"

  let pNames = map ("resListDND" </>)
               ["laptop.png","laptopSmall.png","printer.png",
		"tower.png","server.png","desktop.png"]
  pics <- mapM pixbufNewFromFile pNames

  smallPics <- mapM (\n -> pixbufNewFromFileAtScale n 48 48 True) pNames
	
  [noRoom, publicRoom, restrictedRoom] <- mapM New.listStoreNew
    [["Paul (Home)","John (Home)","Fred (Home)"],
     ["N12","S112", "S113", "S114"],
     ["Server Room Upstairs", "Server Room Downstairs"]]

  -- define extractor function for the string column
  New.treeModelSetColumn noRoom roomStrCol id
  New.treeModelSetColumn publicRoom roomStrCol id
  New.treeModelSetColumn restrictedRoom roomStrCol id
  
  let genRoomStore MacBookPro = noRoom
      genRoomStore MacBook = noRoom
      genRoomStore Printer = publicRoom
      genRoomStore MacPro = publicRoom
      genRoomStore Xserve = restrictedRoom
      genRoomStore IMac = publicRoom

  -- the initial computer list - it's a coincidence that there's
  -- one computer of each type
  content <- New.listStoreNewDND 
    (map (\t -> Computer { name = showCT t, addr = (192,168,0,fromEnum t+1),
			  roomStore = genRoomStore t, roomSel = 0, cType = t})
	      [minBound :: CompType .. maxBound])
    (Just listStoreDefaultDragSourceIface)
    (Just DragDestIface {
      treeDragDestRowDropPossible = \store path@(i:_) -> do
        mCT <- selectionDataGet compTypeTag
        case mCT :: Maybe [Int] of
          Just [ct] -> return True
          Nothing ->
            (treeDragDestRowDropPossible listStoreDefaultDragDestIface)
            store path
          _ -> return False,
      treeDragDestDragDataReceived = \store path@(i:_) -> do
        mCT <- selectionDataGet compTypeTag
        case mCT of
          Just [ct] -> do
            let t = toEnum ct
            liftIO $ New.listStoreInsert store i
              Computer { name = showCT t, addr = (192,168,0,254),
                         roomStore = genRoomStore t, roomSel = 0,
                         cType = t }
            return True
          Nothing ->
            (treeDragDestDragDataReceived listStoreDefaultDragDestIface)
              store path
      })
  -- the area with the possible computer types
  compTypes <- New.listStoreNewDND [minBound :: CompType .. maxBound]
    (Just DragSourceIface {
      treeDragSourceRowDraggable = \store (i:_) -> return True,
      treeDragSourceDragDataGet = \store (i:_) -> do
        ty <- selectionDataGetTarget
        ct <- liftIO $ listStoreGetValue store i
        selectionDataSet compTypeTag [fromEnum ct]
        t <- selectionDataGetTarget
        l <- selectionDataGetLength
        return True,
      treeDragSourceDragDataDelete = \store path -> return True
    })
    Nothing

  -- define extractor functions for the two column
  New.treeModelSetColumn compTypes compPicCol $
    \t -> pics !! fromEnum t
  New.treeModelSetColumn compTypes compStrCol showCT
  
  -- create an icon view of all the computer types
  typesView <- New.iconViewNew
  set typesView [New.iconViewModel := Just compTypes,
                 New.iconViewPixbufColumn := compPicCol,
                 New.iconViewTextColumn := compStrCol,
                 New.iconViewColumns := 6] 

  -- create an editable list of computers
  inventory <- New.treeViewNewWithModel content

  tyCol <- New.treeViewColumnNew
  New.treeViewColumnSetTitle tyCol "Type"
  picRen <- New.cellRendererPixbufNew
  New.treeViewColumnPackStart tyCol picRen False
  New.cellLayoutSetAttributes tyCol picRen content
    (\Computer { cType = t} -> [New.cellPixbuf := smallPics !! fromEnum t])
  tyRen <- New.cellRendererTextNew
  New.treeViewColumnPackStart tyCol tyRen False
  New.cellLayoutSetAttributes tyCol tyRen content
    (\Computer { cType = t} -> [New.cellText := showCT t])
  New.treeViewAppendColumn inventory tyCol

  nameCol <- New.treeViewColumnNew
  New.treeViewColumnSetTitle nameCol "Name"
  New.treeViewColumnSetResizable nameCol True
  New.treeViewColumnSetMinWidth nameCol 100
  nameRen <- New.cellRendererTextNew
  set nameRen [ New.cellTextEditable := True,
                New.cellTextEditableSet := True,
                New.cellTextEllipsize := EllipsizeEnd,
                New.cellTextEllipsizeSet := True]
  New.treeViewColumnPackStart nameCol nameRen True
  New.cellLayoutSetAttributes nameCol nameRen content
    (\Computer { name = n } -> [New.cellText := n])
  New.treeViewAppendColumn inventory nameCol
  on nameRen edited $ \[i] str -> do
    val <- New.listStoreGetValue content i
    New.listStoreSetValue content i val { name = str }

  addrCol <- New.treeViewColumnNew
  New.treeViewColumnSetTitle addrCol "Address"
  oct1 <- New.cellRendererTextNew
  dot1 <- New.cellRendererTextNew
  oct2 <- New.cellRendererTextNew
  dot2 <- New.cellRendererTextNew
  oct3 <- New.cellRendererTextNew
  dot3 <- New.cellRendererTextNew
  oct4 <- New.cellRendererTextNew
  mapM_ (uncurry (New.cellLayoutPackStart addrCol))
    [(oct1, True), (dot1, False), (oct2, True),
     (dot2, False), (oct3, True), (dot3, False), (oct4, True)]
  mapM_ (\d -> set d [New.cellText := ".",
		      New.cellTextWidthChars := 0]) [dot1, dot2, dot3]
  mapM_ (\o -> set o [New.cellXAlign := 1.0,
		      New.cellTextWidthChars := 3]) [oct1, oct2, oct3, oct4]
  New.cellLayoutSetAttributes addrCol oct1 content
    (\Computer { addr = (o1,_,_,_)} -> [New.cellText := show o1])
  New.cellLayoutSetAttributes addrCol oct2 content
    (\Computer { addr = (_,o2,_,_)} -> [New.cellText := show o2])
  New.cellLayoutSetAttributes addrCol oct3 content
    (\Computer { addr = (_,_,o3,_)} -> [New.cellText := show o3])
  New.cellLayoutSetAttributes addrCol oct4 content
    (\Computer { addr = (_,_,_,o4)} -> [New.cellText := show o4])  
  New.treeViewAppendColumn inventory addrCol

  roomCol <- New.treeViewColumnNew
  New.treeViewColumnSetTitle roomCol "Room"
  New.treeViewColumnSetResizable roomCol True
  New.treeViewColumnSetSizing roomCol New.TreeViewColumnAutosize
  roomRen <- New.cellRendererComboNew
  set roomRen [ New.cellTextEditable := True,
                New.cellTextEditableSet := True,
                New.cellComboHasEntry := True ]
  New.treeViewColumnPackStart roomCol roomRen True
  New.cellLayoutSetAttributes roomCol roomRen content
    (\Computer { roomStore = t, roomSel = idx } ->
    [New.cellText :=> New.listStoreGetValue t idx,
    New.cellComboTextModel := (t, roomStrCol)])
  on roomRen edited $ \[i] str -> do
    row@Computer { roomStore = t } <- New.listStoreGetValue content i
    elems <- New.listStoreToList t
    idx <- case (findIndex ((==) str) elems) of
      Just idx -> return idx
      Nothing -> New.listStoreAppend t str
    New.listStoreSetValue content i row { roomSel = idx }
  New.treeViewAppendColumn inventory roomCol

  -- make typesView a drag source for compTypeTag values
  tl <- targetListNew
  targetListAdd tl compTypeTag [TargetSameApp] 0
  iconViewEnableModelDragSource typesView [Button1] tl [ActionCopy]
  
  -- Due to a bug in Gtk+, the treeDragSourceDragDataGet handler in
  -- the DND source handler is not called unless the IconView is also
  -- set to be a DND destination. Bugzilla 550528
  tl <- targetListNew
  iconViewEnableModelDragDest typesView tl []
  
  -- make the inventory widget a drag destination for compTypeTag values
  tl <- targetListNew
  targetListAdd tl compTypeTag [TargetSameApp] 0
  targetListAdd tl targetTreeModelRow [TargetSameWidget] 0
  New.treeViewEnableModelDragDest inventory tl [ActionMove]
  tl <- targetListNew
  targetListAdd tl targetTreeModelRow [TargetSameWidget] 0
  New.treeViewEnableModelDragSource inventory [Button1] tl [ActionMove]
      
  -- Install drag and drop for permuting rows. This is now done above using
  -- the explicit target 'targetTreeModelRow'. Calling the function below
  -- will set a completely new 'TargetList' thereby removing our own
  -- 'compTypeTag' from the inventory widget's target list.
  
  --New.treeViewSetReorderable inventory True

  -- arrange the widgets
  v <- vPanedNew
  panedAdd1 v typesView
  panedAdd2 v inventory
  containerAdd win v

  widgetShowAll win
  mainGUI 
