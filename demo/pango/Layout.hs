-- Example of using a PangoLayout
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

main = do
  initGUI
  -- Create the main window.
  win <- windowNew
  win `onDestroy` mainQuit
  -- Create a drawing area in which we can render text.
  area <- drawingAreaNew
  containerAdd win area
  area `onSizeRequest` return (Requisition 100 100)
  
  -- Create a Cairo Context that contains information about the current font,
  -- etc.
  ctxt <- cairoCreateContext Nothing
  lay <- layoutText ctxt loremIpsum
  layoutSetWrap lay WrapWholeWords
  
  -- Wrap the layout to a different width each time the window is resized.
  area `onSizeAllocate` \(Rectangle _ _ w _) -> do
    layoutSetWidth lay (Just (fromIntegral w))

  -- Setup the handler to draw the layout.
  area `onExpose` updateArea area lay
  
  -- Run the whole thing.
  widgetShowAll win
  mainGUI

updateArea :: DrawingArea -> PangoLayout -> Event -> IO Bool
updateArea area lay Expose {} = do
  win <- widgetGetDrawWindow area
  renderWithDrawable win $ do
    moveTo 0 0
    showLayout lay

  return True
 
