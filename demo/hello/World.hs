module Main (Main.main) where

import Gtk

-- If you return False in the "delete_event" signal handler, GTK will
-- emit the "destroy" signal.  



deleteEvent :: Event -> IO Bool
deleteEvent _ = do 
  putStrLn "delete event" 
  -- Returning True means you don't want the window to be destroyed.
  -- This is useful for popping up 'are you sure you want to quit?' type dialogs.
  return False

-- Another callback
destroyEvent :: IO ()
destroyEvent = do
  putStrLn "destroy event"
  mainQuit

main :: IO ()
main = do
  -- This is called in all Gtk2hs applications. Arguments are parsed
  -- from the command line and are returned to the application if
  -- Nothing is specified.
  Gtk.init Nothing	
  -- Create a new window
  window <- windowNew
  -- When the window is given the "delete_event" signal (this is given 
  -- by the window manager, usually by the "close" option, or on the
  -- titlebar), we ask it to call the deleteEvent function defined above.
  onDelete window deleteEvent
  -- Here we connect the "destroy" event to a signal handler.
  -- This event occurs when we call widgetDestroy on the window,
  -- or if we return FALSE in the "delete_event" callback.
  onDestroy window destroyEvent
  -- Sets the border width of the window.
  containerSetBorderWidth window 10
  -- Creates a new button with the label "Hello World".
  buttonHW <- buttonNewWithLabel "Hello World"
  -- When the button receives the "clicked" signal, it will call the
  -- function given as the second argument
  onClicked buttonHW (putStr "Hello World\n")
  -- This will cause the window to be destroyed by calling
  -- widgetDestroy when "clicked".  Again, the destroy
  -- signal could come from here, or the window manager.
  onClicked buttonHW $ do
    putStrLn "A \"clicked\"-handler to say \"destroy\""
    widgetDestroy window
  containerAdd window buttonHW
  -- This packs the button into the window (a gtk container).
  widgetShow buttonHW
  -- The final step is to display this newly created widget
  widgetShow window
  -- and the window
  -- All GTK applications must have a Gtk.main. Control ends here
  -- and waits for an event to occur (like a key press or
  -- mouse event).
  Gtk.main
  putStrLn "Gtk.main Returned"

