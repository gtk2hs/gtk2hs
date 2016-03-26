{-# LANGUAGE OverloadedStrings #-}
-- Example of using a PangoLayout

import Data.IORef
import Data.Monoid ((<>))
import qualified Data.Text as T

import Graphics.Rendering.Cairo (moveTo, Render)
import qualified GI.Gtk as Gtk (main, init)
import GI.Gtk
       (DrawingArea, widgetShowAll, onWidgetKeyPressEvent,
        iMContextFilterKeypress, onWidgetKeyReleaseEvent,
        iMContextFocusOut, onWidgetFocusOutEvent, iMContextFocusIn,
        onWidgetFocusInEvent, widgetGetWindow, iMContextSetClientWindow,
        onWidgetRealize, onIMContextDeleteSurrounding,
        iMContextSetSurrounding, onIMContextRetrieveSurrounding,
        onIMContextCommit, iMContextGetPreeditString,
        onIMContextPreeditChanged, onIMContextPreeditEnd,
        onIMContextPreeditStart, iMMulticontextNew, onWidgetDraw,
        onWidgetSizeAllocate, widgetQueueDraw, widgetSetSizeRequest,
        containerAdd, drawingAreaNew, mainQuit, onWidgetDestroy, windowNew)
import GI.Gtk.Enums (WrapMode(..), WindowType(..))
import GI.Pango
       (AttrList, Attribute, attrListInsert, attrListNew, Layout,
        layoutSetWidth, layoutNew, layoutSetAttributes, layoutSetText,
        layoutSetWrap)
import GI.PangoCairo.Functions (showLayout, fontMapGetDefault)
import GI.Gdk
       (keyvalToUnicode, keyvalName, eventKeyReadKeyval,
        eventKeyReadState, EventKey, rectangleReadWidth)
import GI.Cairo.Structs.Context (Context(..))
import Foreign.ForeignPtr (withForeignPtr)
import Control.Monad.Trans.Reader (ReaderT(..))
import Graphics.Rendering.Cairo.Types (Cairo(..))
import Foreign.Ptr (castPtr)
import Graphics.Rendering.Cairo.Internal (Render(..))
import Control.Monad.IO.Class (MonadIO(..))

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

attrListNewFromList :: MonadIO m => [Attribute] -> m AttrList
attrListNewFromList list = do
    al <- attrListNew
    mapM_ (attrListInsert al) list
    return al

main = do
  Gtk.init

  -- Create the main window.
  win <- windowNew WindowTypeToplevel
  onWidgetDestroy win mainQuit
  -- Create a drawing area in which we can render text.
  area <- drawingAreaNew
  containerAdd win area
  widgetSetSizeRequest area 100 100

  -- Our widget's data
  buffer <- newIORef defaultBuffer

  preeditRef <- newIORef Nothing

  -- Create a Cairo Context that contains information about the current font,
  -- etc.
  ctxt <- fontMapGetDefault
  lay <- layoutNew ctxt
  layoutSetWrap lay WrapModeWord

  let relayout = do
          buffer@(Buffer _ cursor) <- readIORef buffer
          preedit <- readIORef preeditRef
          case preedit of
              Nothing -> do
                  layoutSetText lay (displayBuffer buffer)
                  layoutSetAttributes lay []
              Just (str,attrs,pos) -> do
                  layoutSetText lay (displayBufferPreedit buffer str pos)
                  attrListNewFromList (map (shiftAttribute (cursor + 1))
                                               (concat attrs)) >>= layoutSetAttributes lay
          widgetQueueDraw area

  relayout

  -- Wrap the layout to a different width each time the window is resized.
  onWidgetSizeAllocate area $ \r -> do
    w <- rectangleReadWidth r
    layoutSetWidth lay (Just (fromIntegral w))

  -- Setup the handler to draw the layout.
  onWidgetDraw area $ \(Context fp) -> withForeignPtr fp $ \p -> (`runReaderT` Cairo (castPtr p)) $ runRender $ do
    updateArea area lay
    return True

  -- Set up input method
  im <- iMMulticontextNew

  onIMContextPreeditStart im $ do
      writeIORef preeditRef (Just ("",[],0))
      relayout
  onIMContextPreeditEnd im $ do
      writeIORef preeditRef Nothing
      relayout
  onIMContextPreeditChanged im $ do
      writeIORef preeditRef . Just =<< iMContextGetPreeditString im
      relayout
  onIMContextCommit im $ \str -> do
      modifyIORef buffer (insertStr str)
      relayout
  onIMContextRetrieveSurrounding im $ do
      Buffer text pos <- readIORef buffer
      iMContextSetSurrounding im text pos
      return True
  onIMContextDeleteSurrounding im $ \off nchars -> do
      putStrLn $ "delete-surrounding("++show off++","++show nchars++")"
      return False

  onWidgetRealize win $
      iMContextSetClientWindow im =<< widgetGetWindow win
  onWidgetFocusInEvent win $ \e -> liftIO (iMContextFocusIn  im) >> return False
  onWidgetFocusOutEvent win $ \e -> liftIO (iMContextFocusOut im) >> return False
  onWidgetKeyReleaseEvent win $ \e -> iMContextFilterKeypress im
  onWidgetKeyPressEvent win $ \e ->  do
    imHandled <- iMContextFilterKeypress im
    if imHandled then return True else do
       mod <- interpretKeyPress e
       case mod of
           Just f -> liftIO $ modifyIORef buffer f >> relayout >> return True
           Nothing -> return False

  widgetShowAll win
  Gtk.main

updateArea :: DrawingArea -> Layout -> Render ()
updateArea area lay = do
    moveTo 0 0
    showLayout lay

interpretKeyPress :: EventKey -> IO (Maybe (Buffer -> Buffer))
interpretKeyPress e = do
    modifiers <- eventKeyReadState e
    if modifiers /= [] then return Nothing else do
        keyVal <- eventKeyReadKeyval
        keyName <- keyvalName keyVal
        keyChar <- toEnum <$> keyvalToUnicode keyVal
        case keyChar of
            '\0' ->
                case keyName of
                    "Left" -> returnJust moveLeft
                    "Right" -> returnJust moveRight
                    "BackSpace" -> returnJust deleteChar
                    _ -> return Nothing
            ch -> do
                -- This does not appear to get called; the IM handles
                -- unmodified keypresses.
                liftIO $ putStrLn "Literal character not handled by IM"
                returnJust (insertStr $ T.singleton ch)
    where returnJust = return . Just

shiftAttribute :: Int -> Attribute -> IO Attribute
shiftAttribute x attr = attr { paStart = x + paStart attr,
                               paEnd   = x + paEnd attr }























