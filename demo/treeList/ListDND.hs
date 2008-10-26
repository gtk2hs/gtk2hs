module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.EventM

import System.Glib.GObject ( toGObject )
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
	roomStore  :: ListStore String,
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
  compTypeTag <- atomNew "_CompType"

  let pNames = map ("resListDND" </>)
               ["laptop.png","laptopSmall.png","printer.png",
		"tower.png","server.png","desktop.png"]
  pics <- mapM pixbufNewFromFile pNames

  smallPics <- mapM (\n -> pixbufNewFromFileAtScale n 48 48 True) pNames
	
  [noRoom, publicRoom, restrictedRoom] <- mapM listStoreNew
    [["Paul (Home)","John (Home)","Fred (Home)"],
     ["N12","S112", "S113", "S114"],
     ["Server Room Upstairs", "Server Room Downstairs"]]

  -- define extractor function for the string column
  treeModelSetColumn noRoom roomStrCol id
  treeModelSetColumn publicRoom roomStrCol id
  treeModelSetColumn restrictedRoom roomStrCol id
  
  let genRoomStore MacBookPro = noRoom
      genRoomStore MacBook = noRoom
      genRoomStore Printer = publicRoom
      genRoomStore MacPro = publicRoom
      genRoomStore Xserve = restrictedRoom
      genRoomStore IMac = publicRoom

  -- the initial computer list - it's a coincidence that there's
  -- one computer of each type
  content <- listStoreNewDND 
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
            liftIO $ listStoreInsert store i
              Computer { name = showCT t, addr = (192,168,0,254),
                         roomStore = genRoomStore t, roomSel = 0,
                         cType = t }
            return True
          Nothing ->
            (treeDragDestDragDataReceived listStoreDefaultDragDestIface)
              store path
      })
  -- the area with the possible computer types
  compTypes <- listStoreNewDND [minBound :: CompType .. maxBound]
    (Just DragSourceIface {
      treeDragSourceRowDraggable = \store (i:_) -> return True,
      treeDragSourceDragDataGet = \store (i:_) -> do
        ty <- selectionDataGetTarget
        ct <- liftIO $ listStoreGetValue store i
        selectionDataSet compTypeTag [fromEnum ct]
        return True,
      treeDragSourceDragDataDelete = \store path -> return True
    })
    Nothing

  -- define extractor functions for the two column
  treeModelSetColumn compTypes compPicCol $
    \t -> pics !! fromEnum t
  treeModelSetColumn compTypes compStrCol showCT
  
  -- create an icon view of all the computer types
  typesView <- iconViewNew
  set typesView [iconViewModel := Just compTypes,
                 iconViewPixbufColumn := compPicCol,
                 iconViewTextColumn := compStrCol,
                 iconViewColumns := 6] 

  -- create an editable list of computers
  inventory <- treeViewNewWithModel content

  tyCol <- treeViewColumnNew
  treeViewColumnSetTitle tyCol "Type"
  picRen <- cellRendererPixbufNew
  treeViewColumnPackStart tyCol picRen False
  cellLayoutSetAttributes tyCol picRen content
    (\Computer { cType = t} -> [cellPixbuf := smallPics !! fromEnum t])
  tyRen <- cellRendererTextNew
  treeViewColumnPackStart tyCol tyRen False
  cellLayoutSetAttributes tyCol tyRen content
    (\Computer { cType = t} -> [cellText := showCT t])
  treeViewAppendColumn inventory tyCol

  nameCol <- treeViewColumnNew
  treeViewColumnSetTitle nameCol "Name"
  treeViewColumnSetResizable nameCol True
  treeViewColumnSetMinWidth nameCol 100
  nameRen <- cellRendererTextNew
  set nameRen [ cellTextEditable := True,
                cellTextEditableSet := True,
                cellTextEllipsize := EllipsizeEnd,
                cellTextEllipsizeSet := True]
  treeViewColumnPackStart nameCol nameRen True
  cellLayoutSetAttributes nameCol nameRen content
    (\Computer { name = n } -> [cellText := n])
  treeViewAppendColumn inventory nameCol
  on nameRen edited $ \[i] str -> do
    val <- listStoreGetValue content i
    listStoreSetValue content i val { name = str }

  addrCol <- treeViewColumnNew
  treeViewColumnSetTitle addrCol "Address"
  oct1 <- cellRendererTextNew
  dot1 <- cellRendererTextNew
  oct2 <- cellRendererTextNew
  dot2 <- cellRendererTextNew
  oct3 <- cellRendererTextNew
  dot3 <- cellRendererTextNew
  oct4 <- cellRendererTextNew
  mapM_ (uncurry (cellLayoutPackStart addrCol))
    [(oct1, True), (dot1, False), (oct2, True),
     (dot2, False), (oct3, True), (dot3, False), (oct4, True)]
  mapM_ (\d -> set d [cellText := ".",
		      cellTextWidthChars := 0]) [dot1, dot2, dot3]
  mapM_ (\o -> set o [cellXAlign := 1.0,
		      cellTextWidthChars := 3]) [oct1, oct2, oct3, oct4]
  cellLayoutSetAttributes addrCol oct1 content
    (\Computer { addr = (o1,_,_,_)} -> [cellText := show o1])
  cellLayoutSetAttributes addrCol oct2 content
    (\Computer { addr = (_,o2,_,_)} -> [cellText := show o2])
  cellLayoutSetAttributes addrCol oct3 content
    (\Computer { addr = (_,_,o3,_)} -> [cellText := show o3])
  cellLayoutSetAttributes addrCol oct4 content
    (\Computer { addr = (_,_,_,o4)} -> [cellText := show o4])  
  treeViewAppendColumn inventory addrCol

  roomCol <- treeViewColumnNew
  treeViewColumnSetTitle roomCol "Room"
  treeViewColumnSetResizable roomCol True
  treeViewColumnSetSizing roomCol TreeViewColumnAutosize
  roomRen <- cellRendererComboNew
  set roomRen [ cellTextEditable := True,
                cellTextEditableSet := True,
                cellComboHasEntry := True ]
  treeViewColumnPackStart roomCol roomRen True
  cellLayoutSetAttributes roomCol roomRen content
    (\Computer { roomStore = t, roomSel = idx } ->
    [cellText :=> listStoreGetValue t idx,
    cellComboTextModel := (t, roomStrCol)])
  on roomRen edited $ \[i] str -> do
    row@Computer { roomStore = t } <- listStoreGetValue content i
    elems <- listStoreToList t
    idx <- case (findIndex ((==) str) elems) of
      Just idx -> return idx
      Nothing -> listStoreAppend t str
    listStoreSetValue content i row { roomSel = idx }
  treeViewAppendColumn inventory roomCol

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
  treeViewEnableModelDragDest inventory tl [ActionMove]
  tl <- targetListNew
  targetListAdd tl targetTreeModelRow [TargetSameWidget] 0
  treeViewEnableModelDragSource inventory [Button1] tl [ActionMove]
      
  -- Install drag and drop for permuting rows. This is now done above using
  -- the explicit target 'targetTreeModelRow'. Calling the function below
  -- will set a completely new 'TargetList' thereby removing our own
  -- 'compTypeTag' from the inventory widget's target list.
  
  --treeViewSetReorderable inventory True

  -- arrange the widgets
  v <- vPanedNew
  panedAdd1 v typesView
  panedAdd2 v inventory
  containerAdd win v

  widgetShowAll win
  mainGUI 
