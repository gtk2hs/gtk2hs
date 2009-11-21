-- Use GtkSocket and GtkPlug for cross-process embedded.
-- Just startup program, press 'Alt-m' to new editor, press `Alt-n` to new terminal.
-- And those plug widget (editor, terminal) running in child-process, 
-- so program won't crash when child-process throw un-catch exception.

module Main where

import System.Posix.Process
import System.Environment
import System.Directory
import System.FilePath ((</>))
import Control.Monad

import Event
import Key

import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.Structs
import Graphics.UI.Gtk.Vte.Vte

import qualified Graphics.UI.Gtk.Gdk.Events as E

data PlugType = PlugEditor
              | PlugTerminal
              deriving (Eq, Ord, Show, Read)

-- | Main.
main :: IO ()
main = do
  -- Init main.
  initGUI

  -- Get program arguments.
  args <- getArgs

  case length args of
    -- Entry socket main when no arguments.
    0 -> socketMain 

    -- Entry plug main when have two arguments.
    2 -> do
      let typeArg = read (head args) :: PlugType -- get Plug type
          idArg   = toNativeWindowId $ read (last args) :: NativeWindowId -- get GtkSocket id

      case typeArg of
        PlugEditor   -> editorPlugMain idArg   -- entry eidtor plug main
        PlugTerminal -> terminalPlugMain idArg -- entry terminal plug main

    -- Otherwise just output error and exit.
    _ -> putStrLn "Wrong program arguments."
  
-- | Handle key press.
handleKeyPress :: E.Event -> Notebook -> IO Bool
handleKeyPress ev notebook = 
    case eventTransform ev of
      Nothing -> return False
      Just e  -> 
          case eventGetName e of
            "M-m" -> forkPlugProcess notebook PlugEditor "Editor" >> return True
            "M-n" -> forkPlugProcess notebook PlugTerminal "Terminal" >> return True
            _     -> return False

-- | Fork plug process.
forkPlugProcess :: Notebook -> PlugType -> String -> IO ()
forkPlugProcess notebook plugType tabName = do
  -- Create new GtkSocket.
  socket <- socketNew
  widgetShow socket                          -- must show before add GtkSocekt to container
  notebookAppendPage notebook socket tabName -- add to GtkSocekt notebook
  id <- socketGetId socket                    -- get GtkSocket id

  -- Fork process to add GtkPlug into GtkSocekt. 
  forkProcess (do
                path <- liftM2 (</>) getCurrentDirectory getProgName -- get program full path
                executeFile path False [show plugType, show $ fromNativeWindowId id] Nothing)
  return ()

-- | GtkSocekt main.
socketMain :: IO ()  
socketMain = do
  -- Output message.
  pid <- getProcessID
  putStrLn $ "Running in socket process : " ++ show pid

  -- Create top-level window.
  window <- windowNew
  windowFullscreen window
  window `onDestroy` mainQuit

  -- Create notebook to contain GtkSocekt.
  notebook <- notebookNew
  window `containerAdd` notebook

  -- Handle key press.
  window `onKeyPress` (\event -> handleKeyPress event notebook)

  widgetShowAll window

  mainGUI

-- | Editor plug main.
editorPlugMain :: NativeWindowId -> IO ()
editorPlugMain id = do
  -- Create editor.
  textView <- textViewNew
  textBuffer <- textViewGetBuffer textView
  textBufferSetText textBuffer $ show id

  plugWrap id textView

-- | Terminal plug main.
terminalPlugMain :: NativeWindowId -> IO ()
terminalPlugMain id = do
  -- Create terminal.
  terminal <- terminalNew
  terminalForkCommand terminal Nothing Nothing Nothing Nothing False False False

  plugWrap id terminal

-- | Plug wrap function.
plugWrap :: WidgetClass widget => NativeWindowId -> widget -> IO ()
plugWrap id widget = do
  -- Output message.
  pid <- getProcessID
  putStrLn $ "Running in plug process : " ++ show pid

  -- Create GtkPlug with GtkSocekt id.
  plug <- plugNew $ Just id
  plug `onDestroy` mainQuit
  
  -- Add widget to GtkPlug.
  scrolledWindow <- scrolledWindowNew Nothing Nothing
  scrolledWindow `containerAdd` widget
  plug `containerAdd` scrolledWindow

  widgetShowAll plug  

  mainGUI

