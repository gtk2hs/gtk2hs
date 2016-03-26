{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Text (Text)
import qualified GI.Gtk.Functions as GI (main, init)
import GI.Gtk.Objects.Action (onActionActivate, actionNew)
import GI.Gtk.Functions (mainQuit)
import GI.Gtk.Objects.ActionGroup
       (actionGroupAddActionWithAccel, actionGroupAddAction,
        actionGroupNew)
import GI.Gtk.Objects.UIManager
       (uIManagerGetWidget, uIManagerInsertActionGroup,
        uIManagerAddUiFromString, uIManagerNew)
import GI.Gtk.Objects.Window (windowNew)
import GI.Gtk.Objects.Widget
       (widgetShowAll, setWidgetHeightRequest, setWidgetWidthRequest,
        onWidgetDestroy)
import GI.Gtk.Objects.TextView (textViewNew)
import GI.Gtk.Objects.VBox (vBoxNew)
import GI.Gtk.Objects.Box (boxPackStart, setBoxHomogeneous)
import GI.Gtk.Objects.Container (containerAdd)
import GI.Gtk.Constants
       (pattern STOCK_PASTE, pattern STOCK_COPY, pattern STOCK_CUT, pattern STOCK_QUIT,
        pattern STOCK_SAVE_AS, pattern STOCK_SAVE, pattern STOCK_OPEN, pattern STOCK_NEW)
import GI.Gtk.Enums (WindowType(..))
import qualified Data.Text as T (length)

-- A function like this can be used to tag string literals for i18n.
-- It also avoids a lot of type annotations.
__ :: Text -> Text
__ = id -- Replace with getText from the hgettext package in localised versions

uiDef =
  "<ui>\
  \  <menubar>\
  \    <menu name=\"File\" action=\"FileAction\">\
  \      <menuitem name=\"New\" action=\"NewAction\" />\
  \      <menuitem name=\"Open\" action=\"OpenAction\" />\
  \      <menuitem name=\"Save\" action=\"SaveAction\" />\
  \      <menuitem name=\"SaveAs\" action=\"SaveAsAction\" />\
  \      <separator/>\
  \      <menuitem name=\"Exit\" action=\"ExitAction\"/>\
  \      <placeholder name=\"FileMenuAdditions\" />\
  \    </menu>\
  \    <menu name=\"Edit\" action=\"EditAction\">\
  \      <menuitem name=\"Cut\" action=\"CutAction\"/>\
  \      <menuitem name=\"Copy\" action=\"CopyAction\"/>\
  \      <menuitem name=\"Paste\" action=\"PasteAction\"/>\
  \    </menu>\
  \  </menubar>\
  \  <toolbar>\
  \    <placeholder name=\"FileToolItems\">\
  \      <separator/>\
  \      <toolitem name=\"New\" action=\"NewAction\"/>\
  \      <toolitem name=\"Open\" action=\"OpenAction\"/>\
  \      <toolitem name=\"Save\" action=\"SaveAction\"/>\
  \      <separator/>\
  \    </placeholder>\
  \    <placeholder name=\"EditToolItems\">\
  \      <separator/>\
  \      <toolitem name=\"Cut\" action=\"CutAction\"/>\
  \      <toolitem name=\"Copy\" action=\"CopyAction\"/>\
  \      <toolitem name=\"Paste\" action=\"PasteAction\"/>\
  \      <separator/>\
  \    </placeholder>\
  \  </toolbar>\
  \</ui>" :: Text

main = do
  GI.init Nothing

  -- Create the menus
  fileAct <- actionNew "FileAction" (Just (__"File")) Nothing Nothing
  editAct <- actionNew "EditAction" (Just (__"Edit")) Nothing Nothing

  -- Create menu items
  newAct <- actionNew "NewAction" (Just (__"New"))
            (Just (__"Clear the spreadsheet area."))
            (Just STOCK_NEW)
  onActionActivate newAct $ putStrLn "New activated."
  openAct <- actionNew "OpenAction" (Just (__"Open"))
            (Just (__"Open an existing spreadsheet."))
            (Just STOCK_OPEN)
  onActionActivate openAct $ putStrLn "Open activated."
  saveAct <- actionNew "SaveAction" (Just (__"Save"))
            (Just (__"Save the current spreadsheet."))
            (Just STOCK_SAVE)
  onActionActivate saveAct $ putStrLn "Save activated."
  saveAsAct <- actionNew "SaveAsAction" (Just (__"SaveAs"))
            (Just (__"Save spreadsheet under new name."))
            (Just STOCK_SAVE_AS)
  onActionActivate saveAsAct $ putStrLn "SaveAs activated."
  exitAct <- actionNew "ExitAction" (Just (__"Exit"))
            (Just (__"Exit this application."))
            (Just STOCK_QUIT)
  onActionActivate exitAct mainQuit
  cutAct <- actionNew "CutAction" (Just (__"Cut"))
            (Just (__"Cut out the current selection."))
            (Just STOCK_CUT)
  onActionActivate cutAct $ putStrLn "Cut activated."
  copyAct <- actionNew "CopyAction" (Just (__"Copy"))
            (Just (__"Copy the current selection."))
            (Just STOCK_COPY)
  onActionActivate copyAct $ putStrLn "Copy activated."
  pasteAct <- actionNew "PasteAction" (Just (__"Paste"))
            (Just (__"Paste the current selection."))
            (Just STOCK_PASTE)
  onActionActivate pasteAct $ putStrLn "Paste activated."

  standardGroup <- actionGroupNew ("standard"::Text)
  mapM_ (actionGroupAddAction standardGroup) [fileAct, editAct]
  mapM_ (\act -> actionGroupAddActionWithAccel standardGroup act (Nothing::Maybe Text))
    [newAct, openAct, saveAct, saveAsAct, exitAct, cutAct, copyAct, pasteAct]

  ui <- uIManagerNew
  mid <- uIManagerAddUiFromString ui uiDef (fromIntegral $ T.length uiDef)
  uIManagerInsertActionGroup ui standardGroup 0

  win <- windowNew WindowTypeToplevel
  onWidgetDestroy win mainQuit
  setWidgetWidthRequest win 200
  setWidgetHeightRequest win 100
  menuBar <- uIManagerGetWidget ui "/ui/menubar"
  toolBar <- uIManagerGetWidget ui "/ui/toolbar"

  edit <- textViewNew
  vBox <- vBoxNew False 0
  setBoxHomogeneous vBox False
  boxPackStart vBox menuBar False False 0
  boxPackStart vBox toolBar False False 0
  boxPackStart vBox edit True True 0

  containerAdd win vBox
  widgetShowAll win

  GI.main
