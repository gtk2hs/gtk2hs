{-# LANGUAGE OverloadedStrings #-}
-- Example of using a PangoLayout

import Data.IORef
import Data.Monoid ((<>))
import qualified Data.Text as T

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

loremIpsum = "Lorem ipsum dolor sit amet, consectetur adipisicing elit,\
        \ sed do eiusmod tempor incididunt ut labore et dolore magna\
        \ aliqua. Ut enim ad minim veniam, quis nostrud exercitation\
        \ ullamco laboris nisi ut aliquip ex ea commodo consequat.\
        \ Duis aute irure dolor in reprehenderit in voluptate\
        \ velit esse cillum dolore eu fugiat nulla pariatur.\
        \ Excepteur sint occaecat cupidatat non proident, sunt in culpa\
        \ qui officia deserunt mollit anim id est laborum."

data Buffer = Buffer T.Text Int

defaultBuffer = Buffer loremIpsum (T.length loremIpsum)

displayBuffer (Buffer str pos) =
  before <> "<CURSOR>" <> after
  where (before,after) = T.splitAt pos str

displayBufferPreedit (Buffer str pos) preeditStr preeditPos =
  before <> "[" <> prebefore <> "<CURSOR>" <> preafter <> "]" <> after
  where (before,after) = T.splitAt pos str
        (prebefore, preafter) = T.splitAt preeditPos preeditStr

insertStr new (Buffer str pos) = Buffer (before<>new<>after) (pos+T.length new)
  where (before,after) = T.splitAt pos str

deleteChar b@(Buffer str 0) = b
deleteChar (Buffer str pos) = Buffer (T.init before <> after) (pos-1)
  where (before,after) = T.splitAt pos str

moveLeft b@(Buffer str pos) | pos==0 = b
                            | otherwise = Buffer str (pos-1)

moveRight b@(Buffer str pos) | pos==T.length str = b
                             | otherwise = Buffer str (pos+1)

main = do
  initGUI

  -- Create the main window.
  win <- windowNew
  on win objectDestroy mainQuit
  -- Create a drawing area in which we can render text.
  area <- drawingAreaNew
  containerAdd win area
  widgetSetSizeRequest area 100 100

  -- Our widget's data
  buffer <- newIORef defaultBuffer

  preeditRef <- newIORef Nothing

  -- Create a Cairo Context that contains information about the current font,
  -- etc.
  ctxt <- cairoCreateContext Nothing
  lay <- layoutEmpty ctxt
  layoutSetWrap lay WrapWholeWords

  let relayout = do
      buffer@(Buffer _ cursor) <- readIORef buffer
      preedit <- readIORef preeditRef
      case preedit of
          Nothing -> do
              layoutSetText lay (displayBuffer buffer)
              layoutSetAttributes lay []
          Just (str,attrs,pos) -> do
              layoutSetText lay (displayBufferPreedit buffer str pos)
              layoutSetAttributes lay (map (shiftAttribute (cursor + 1))
                                           (concat attrs))
      widgetQueueDraw area

  relayout

  -- Wrap the layout to a different width each time the window is resized.
  on area sizeAllocate $ \(Rectangle _ _ w _) ->
    layoutSetWidth lay (Just (fromIntegral w))

  -- Setup the handler to draw the layout.
  on area draw $ updateArea area lay

  -- Set up input method
  im <- imMulticontextNew

  on im imContextPreeditStart $ do
      writeIORef preeditRef (Just ("",[],0))
      relayout
  on im imContextPreeditEnd $ do
      writeIORef preeditRef Nothing
      relayout
  on im imContextPreeditChanged $ do
      writeIORef preeditRef . Just =<< imContextGetPreeditString im
      relayout
  on im imContextCommit $ \str -> do
      modifyIORef buffer (insertStr str)
      relayout
  on im imContextRetrieveSurrounding $ do
      Buffer text pos <- readIORef buffer
      imContextSetSurrounding im text pos
      return True
  on im imContextDeleteSurrounding' $ \off nchars -> do
      putStrLn $ "delete-surrounding("++show off++","++show nchars++")"
      return False

  on win realize $ do
      imContextSetClientWindow im =<< widgetGetWindow win
  on win focusInEvent  $ liftIO (imContextFocusIn  im) >> return False
  on win focusOutEvent $ liftIO (imContextFocusOut im) >> return False
  on win keyReleaseEvent $ imContextFilterKeypress im
  on win keyPressEvent $ do
    imHandled <- imContextFilterKeypress im
    if imHandled then return True else do
       mod <- interpretKeyPress
       case mod of
           Just f -> liftIO $ modifyIORef buffer f >> relayout >> return True
           Nothing -> return False

  widgetShowAll win
  mainGUI

updateArea :: DrawingArea -> PangoLayout -> Render ()
updateArea area lay = do
    moveTo 0 0
    showLayout lay

interpretKeyPress :: EventM EKey (Maybe (Buffer -> Buffer))
interpretKeyPress = do
    modifiers <- eventModifier
    if modifiers /= [] then return Nothing else do
        keyName <- eventKeyName
        keyChar <- fmap keyToChar eventKeyVal
        case keyChar of
            Just ch -> do
                -- This does not appear to get called; the IM handles
                -- unmodified keypresses.
                liftIO $ putStrLn "Literal character not handled by IM"
                returnJust (insertStr $ T.singleton ch)
            Nothing -> do
                case keyName of
                    "Left" -> returnJust moveLeft
                    "Right" -> returnJust moveRight
                    "BackSpace" -> returnJust deleteChar
                    _ -> return Nothing
    where returnJust = return . Just

shiftAttribute :: Int -> PangoAttribute -> PangoAttribute
shiftAttribute x attr = attr { paStart = x + paStart attr,
                               paEnd   = x + paEnd attr }
