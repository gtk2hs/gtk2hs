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
import Control.Monad.Trans

import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.Structs
import Graphics.UI.Gtk.Vte.Vte
import Graphics.UI.Gtk.Gdk.EventM

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
      let pType = read (head args) :: PlugType -- get Plug type
          id    = toNativeWindowId $ read (last args) :: NativeWindowId -- get GtkSocket id

      plugMain id pType
    -- Otherwise just output error and exit.
    _ -> putStrLn "Wrong program arguments."
  
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
  window `on` keyPressEvent $ tryEvent $ do
    keyModifier <- eventModifier
    keyName     <- eventKeyName
    liftIO $ when (keyModifier == [Alt]) $ 
      case keyName of
        "m" -> forkPlugProcess notebook PlugEditor "Editor"     -- create editor GtkPlug
        "n" -> forkPlugProcess notebook PlugTerminal "Terminal" -- create terminal GtkPlug

  widgetShowAll window

  mainGUI

-- | GtkPlug main.
plugMain :: NativeWindowId -> PlugType -> IO ()
plugMain id PlugEditor   = plugWrap id =<< createEditor
plugMain id PlugTerminal = plugWrap id =<< createTerminal

-- | Fork plug process.
forkPlugProcess :: Notebook -> PlugType -> String -> IO ()
forkPlugProcess notebook plugType tabName = do
  -- Create new GtkSocket.
  socket <- socketNew
  widgetShow socket                          -- must show before add GtkSocekt to container
  notebookAppendPage notebook socket tabName -- add to GtkSocekt notebook
  id <- socketGetId socket                    -- get GtkSocket id

  -- Fork process to add GtkPlug into GtkSocekt. 
  path <- liftM2 (</>) getCurrentDirectory getProgName -- get program full path
  forkProcess (executeFile path False [show plugType, show $ fromNativeWindowId id] Nothing)
  return ()

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

-- Create editor widget.
createEditor :: IO TextView
createEditor = textViewNew
  
-- Create terminal widget.
createTerminal :: IO Terminal  
createTerminal = do
  terminal <- terminalNew
  terminalForkCommand terminal Nothing Nothing Nothing Nothing False False False
  return terminal