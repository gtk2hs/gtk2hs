-- Test file for the SourceView widget.
module Main where

import Gtk
import Mogul
import SourceView
import SourceBuffer
import SourceLanguage
import SourceLanguagesManager

main = do
  initGUI
  win <- newWindow
  win `onDestroy` Mogul.mainQuit

  -- create the appropriate language
  lm <- sourceLanguagesManagerNew
  langM <- sourceLanguagesManagerGetLanguageFromMimeType lm "text/x-haskell"
  lang <- case langM of
    (Just lang) -> return lang
    Nothing -> do
      langDirs <- sourceLanguagesManagerGetLangFilesDirs lm
      error ("please copy haskell.lang to one of the following directories:\n"
	     ++unlines langDirs)

  -- create a new SourceBuffer object
  buffer <- sourceBufferNewWithLanguage lang

  -- load up and display a file
  fileContents <- readFile "SourceViewTest.hs"
  textBufferSetText buffer fileContents
  textBufferSetModified buffer False

  sourceBufferSetHighlight buffer True

  -- create a new SourceView Widget
  sv <- sourceViewNewWithBuffer buffer

  -- put it in a scrolled window
  sw <- scrolledWindowNew Nothing Nothing
  sw `containerAdd` sv
  scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
  sw `scrolledWindowSetShadowType` ShadowIn
  win `containerAdd` sw
  
  -- show the widget and run the main loop
  windowSetDefaultSize win 400 500
  widgetShowAll win
  mainGUI
