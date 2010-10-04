-- | Notebook demo (include Spinner animation).
--  Author      :  Andy Stewart
--  Copyright   :  (c) 2010 Andy Stewart <lazycat.manatee@gmail.com>

module Main where

import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import Graphics.UI.Gtk

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
  initGUI

  -- Create window and notebook.
  window <- windowNew
  notebook <- notebookNew

  -- Set window.
  windowSetDefaultSize window 800 600
  windowSetPosition window WinPosCenter
  set window [windowTitle := "Press Ctrl + n to create new tab."]

  -- Handle key press action.
  window `on` keyPressEvent $ tryEvent $ do
    -- Create new tab when user press Ctrl+n
    [Control] <- eventModifier
    "n" <- eventKeyName
    liftIO $ do
         -- Create text view.
         textView <- textViewNew
         widgetShowAll textView -- must show before add notebook,
                                -- otherwise notebook won't display child widget 
                                -- even have add in notebook.

         -- Create notebook tab.
         tab <- notebookTabNew (Just "Cool tab") Nothing
         menuLabel <- labelNew Nothing

         -- Add widgets in notebook.
         notebookAppendPageMenu notebook textView (ntBox tab) menuLabel

         -- Start spinner animation when create tab.
         notebookTabStart tab

         -- Stop spinner animation after finish load.
         timeoutAdd (notebookTabStop tab >> return False) 5000

         -- Close tab when click button.
         ntCloseButton tab `onToolButtonClicked` do
           index <- notebookPageNum notebook textView
           index ?>= \i -> notebookRemovePage notebook i

         return ()
        
  -- Show window.
  window `containerAdd` notebook
  widgetShowAll window
  onDestroy window mainQuit
  mainGUI

-- | Create notebook tab.
notebookTabNew :: Maybe String -> Maybe Int -> IO NotebookTab
notebookTabNew name size = do
  -- Init.
  let iconSize = fromMaybe 12 size
  box <- hBoxNew False 0
  spinner <- spinnerNew
  label <- labelNew name
  image <- imageNewFromIcon "gtk-close" iconSize
  closeButton <- toolButtonNew (Just image) Nothing

  -- Show.
  boxPackStart box label PackNatural 0
  boxPackStart box closeButton PackNatural 0
  widgetShowAll box

  return $ NotebookTab box spinner label closeButton iconSize

-- | Set tab name.
notebookTabSetName :: NotebookTab -> String -> IO ()
notebookTabSetName tab = 
  labelSetText (ntLabel tab)

-- | Start spinner animation.
notebookTabStart :: NotebookTab -> IO ()
notebookTabStart NotebookTab {ntBox     = box
                             ,ntSpinner = spinner
                             ,ntSize    = size} = do
  boxTryPack box spinner PackNatural (Just 0) (size `div` 2)
  spinnerStart spinner
  widgetShow spinner

-- | Stop spinner animation.
notebookTabStop :: NotebookTab -> IO ()
notebookTabStop NotebookTab {ntBox     = box
                            ,ntSpinner = spinner} = do
  containerTryRemove box spinner
  spinnerStop spinner

-- | Create image widget with given icon name and size. 
imageNewFromIcon :: String -> Int -> IO Image
imageNewFromIcon iconName size = do
  iconTheme <- iconThemeGetDefault
  pixbuf <- do 
    -- Function 'iconThemeLoadIcon' can scale icon with specified size.
    pixbuf <- iconThemeLoadIcon iconTheme iconName size IconLookupUseBuiltin
    case pixbuf of
      Just p  -> return p
      Nothing -> error $ "imageNewFromIcon : search icon " ++ iconName ++ " failed."
  imageNewFromPixbuf pixbuf

-- | Try to packing widget in box.
-- If @child@ have exist parent, do nothing,
-- otherwise, add @child@ to @parent@.
boxTryPack :: (BoxClass parent, WidgetClass child) => parent -> child -> Packing -> Maybe Int -> Int -> IO ()
boxTryPack box widget packing order space = do
  parent <- widgetGetParent widget
  when (isNothing parent) $ do
    boxPackStart box widget packing space
    order ?>= boxReorderChild box widget

-- | Try to remove child from parent.    
containerTryRemove :: (ContainerClass parent, WidgetClass child) => parent -> child -> IO ()     
containerTryRemove parent widget = do
  hasParent <- widgetGetParent widget
  unless (isNothing hasParent) $ containerRemove parent widget

-- | Maybe.
(?>=) :: Monad m => Maybe a -> (a -> m ()) -> m () 
m ?>= f = maybe (return ()) f m
