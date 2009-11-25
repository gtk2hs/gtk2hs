import Graphics.UI.Gtk
import Data.Char
import Data.List
import Data.Maybe

data RowInfo = RowInfo { rowString :: String, rowCase :: Maybe Bool }

mkCase Nothing str = str
mkCase (Just False) str = map toLower str
mkCase (Just True) str = map toUpper str

advCase Nothing = Just False
advCase (Just False) = Just True
advCase (Just True) = Nothing

main :: IO ()
main = do
  unsafeInitGUIForThreadedRTS
  win <- windowNew
  win `on` objectDestroy $ mainQuit

  content <- readFile "ListText.hs"

  model <- listStoreNew (map (\r -> RowInfo r Nothing) (lines content))
  view <- treeViewNewWithModel model

  -- add a column showing the index
  col <- treeViewColumnNew
  treeViewAppendColumn view col

  cell <- cellRendererTextNew
  cellLayoutPackStart col cell True
  cellLayoutSetAttributeFunc col cell model $ \(TreeIter _ n _ _) ->
    set cell [cellText := show n]
  set col [treeViewColumnTitle := "line",
	   treeViewColumnReorderable := True ]

  -- add a column showing the line in the file
  col <- treeViewColumnNew
  treeViewAppendColumn view col
  set col [treeViewColumnTitle := "line in file",
	   treeViewColumnReorderable := True ]

  cell <- cellRendererTextNew
  cellLayoutPackStart col cell True
  cellLayoutSetAttributes col cell model $
    \row -> [cellText := mkCase (rowCase row) (rowString row)]
  
  -- add a column showing if it is forced to a specific case
  col <- treeViewColumnNew
  treeViewAppendColumn view col
  set col [treeViewColumnTitle := "case",
	   treeViewColumnReorderable := True ]

  cell <- cellRendererToggleNew
  cellLayoutPackStart col cell True
  cellLayoutSetAttributes col cell model $
    \row -> [cellToggleActive := fromMaybe False (rowCase row),
	     cellToggleInconsistent := rowCase row==Nothing]
  cell `on` cellToggled $ \tpStr -> do
    let [i] = stringToTreePath tpStr
    row@RowInfo { rowCase = c } <- listStoreGetValue model i
    listStoreSetValue model i row { rowCase = advCase c }

  -- to annoy the user: don't allow any columns to be dropped at the far right
  treeViewSetColumnDragFunction view $ Just $ \_ rCol _ -> do
    return (rCol /= Nothing)

  treeViewSetSearchEqualFunc view $ Just $ \str (TreeIter _ n _ _) -> do
    row <- listStoreGetValue model (fromIntegral n)
    return (map toLower str `isPrefixOf` map toLower (filter isAlphaNum (rowString row)))

  swin <- scrolledWindowNew Nothing Nothing
  set swin [ containerChild := view ]
  set win [ containerChild := swin ]
  widgetShowAll win
  mainGUI

