{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Notebook demo (include Spinner animation).
--  Author      :  Andy Stewart
--  Copyright   :  (c) 2010 Andy Stewart <lazycat.manatee@gmail.com>

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Text (Text)
import Data.Monoid ((<>))
import GI.Gtk
       (containerRemove, ContainerK, boxReorderChild, widgetGetParent,
        WidgetK, BoxK, imageNewFromPixbuf, iconThemeLoadIcon,
        iconThemeGetDefault, Image, spinnerStop, widgetShow, spinnerStart,
        labelSetText, setWindowTitle, boxPackStart, toolButtonNew,
        spinnerNew, hBoxNew, mainQuit, onWidgetDestroy, containerAdd,
        notebookRemovePage, notebookPageNum, onToolButtonClicked,
        notebookAppendPageMenu, labelNew, widgetShowAll, textViewNew,
        onWidgetKeyPressEvent, windowSetPosition, windowSetDefaultSize,
        notebookNew, windowNew, ToolButton, Label, Spinner, HBox)
import qualified Data.Text as T (unpack)
import qualified GI.Gtk as Gtk (main, init)
import GI.Gtk.Enums (WindowType(..), WindowPosition(..))
import Data.GI.Base.Attributes (AttrOp(..), set)
import GI.Gdk (keyvalName, eventKeyReadKeyval, eventKeyReadState)
import GI.Gdk.Flags (ModifierType(..))
import GI.GLib (timeoutAdd, pattern PRIORITY_DEFAULT)
import GI.Gtk.Flags (IconLookupFlags(..))
import Control.Exception (catch)
import Data.GI.Base.BasicTypes (UnexpectedNullPointerReturn(..))

data NotebookTab =
    NotebookTab {ntBox          :: HBox
                ,ntSpinner      :: Spinner
                ,ntLabel        :: Label
                ,ntCloseButton  :: ToolButton
                ,ntSize         :: Int}

-- | Main
main :: IO ()
main = do
  -- Init.
  Gtk.init Nothing

  -- Create window and notebook.
  window <- windowNew WindowTypeToplevel
  notebook <- notebookNew

  -- Set window.
  windowSetDefaultSize window 800 600
  windowSetPosition window WindowPositionCenter
  setWindowTitle window "Press Ctrl + n to create new tab."

  -- Handle key press action.
  onWidgetKeyPressEvent window $ \e ->
    -- Create new tab when user press Ctrl+n
    eventKeyReadState e >>= \case
      [ModifierTypeControlMask] ->
        eventKeyReadKeyval e >>= keyvalName >>= \case
          "n" -> do
            -- Create text view.
            textView <- textViewNew
            widgetShowAll textView -- must show before add notebook,
                                -- otherwise notebook won't display child widget
                                -- even have add in notebook.

            -- Create notebook tab.
            tab <- notebookTabNew (Just "Cool tab") Nothing
            menuLabel <- labelNew (Nothing :: Maybe Text)

            -- Add widgets in notebook.
            notebookAppendPageMenu notebook textView (Just $ ntBox tab) (Just menuLabel)

            -- Start spinner animation when create tab.
            notebookTabStart tab

            -- Stop spinner animation after finish load.
            timeoutAdd PRIORITY_DEFAULT 5000 $ notebookTabStop tab >> return False

            -- Close tab when click button.
            onToolButtonClicked (ntCloseButton tab) $ do
              index <- notebookPageNum notebook textView
              notebookRemovePage notebook index
            return True
          _ -> return False
      _ -> return False

  -- Show window.
  containerAdd window notebook
  widgetShowAll window
  onWidgetDestroy window mainQuit
  Gtk.main

-- | Create notebook tab.
notebookTabNew :: Maybe Text -> Maybe Int -> IO NotebookTab
notebookTabNew name size = do
  -- Init.
  let iconSize = fromMaybe 12 size
  box <- hBoxNew False 0
  spinner <- spinnerNew
  label <- labelNew name
  image <- imageNewFromIcon "window-close" iconSize
  closeButton <- toolButtonNew (Just image) (Nothing::Maybe Text)

  -- Show.
  boxPackStart box label False False 0
  boxPackStart box closeButton False False 0
  widgetShowAll box

  return $ NotebookTab box spinner label closeButton iconSize

-- | Set tab name.
notebookTabSetName :: NotebookTab -> Text -> IO ()
notebookTabSetName tab =
  labelSetText (ntLabel tab)

-- | Start spinner animation.
notebookTabStart :: NotebookTab -> IO ()
notebookTabStart NotebookTab {ntBox     = box
                             ,ntSpinner = spinner
                             ,ntSize    = size} = do
  boxTryPack box spinner False False (Just 0) (size `div` 2)
  spinnerStart spinner
  widgetShow spinner

-- | Stop spinner animation.
notebookTabStop :: NotebookTab -> IO ()
notebookTabStop NotebookTab {ntBox     = box
                            ,ntSpinner = spinner} = do
  containerTryRemove box spinner
  spinnerStop spinner

-- | Create image widget with given icon name and size.
imageNewFromIcon :: Text -> Int -> IO Image
imageNewFromIcon iconName size = do
  iconTheme <- iconThemeGetDefault
  -- Function 'iconThemeLoadIcon' can scale icon with specified size.
  pixbuf <- iconThemeLoadIcon iconTheme iconName (fromIntegral size) [IconLookupFlagsUseBuiltin]
  imageNewFromPixbuf (Just pixbuf)

-- | Try to packing widget in box.
-- If @child@ have exist parent, do nothing,
-- otherwise, add @child@ to @parent@.
boxTryPack :: (BoxK parent, WidgetK child) => parent -> child -> Bool -> Bool -> Maybe Int -> Int -> IO ()
boxTryPack box widget expand fill order space =
    void (widgetGetParent widget)
  `catch` (\(_ :: UnexpectedNullPointerReturn) -> do
    boxPackStart box widget expand fill (fromIntegral space)
    order ?>= (boxReorderChild box widget . fromIntegral))

-- | Try to remove child from parent.
containerTryRemove :: (ContainerK parent, WidgetK child) => parent -> child -> IO ()
containerTryRemove parent widget = do
  hasParent <- (widgetGetParent widget >> return True) `catch` (\(_ :: UnexpectedNullPointerReturn) -> return False)
  when hasParent $ containerRemove parent widget

-- | Maybe.
(?>=) :: Monad m => Maybe a -> (a -> m ()) -> m ()
m ?>= f = maybe (return ()) f m
