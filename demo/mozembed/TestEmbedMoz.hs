-- A Test Program for the Gtk2 Mozilla Widget
--

import Graphics.UI.Gtk
import Graphics.UI.Gtk.MozEmbed
import System (getArgs, getEnv, getProgName)


main :: IO ()
main = do
  initGUI
  w <- windowNew
  onDelete w (const $ return False)
  onDestroy w mainQuit
  mozEmbedSetCompPath "/usr/lib/mozilla"

  moz <- mozEmbedNew
  widgetShow moz

  containerAdd w moz

  windowSetTitle w "TestEmbedMoz"
  containerSetBorderWidth w 2
  widgetSetSizeRequest w 640 480
  widgetShowAll w

  onOpenConnectID <- onOpenURI moz
              (\ s -> do putStrLn ("onOpenURI: " ++ s)
                         return False)

  args <- getArgs
  case args of
    file@(c:_) : _ -> let
      (fr,dr) = span ('/' /=) $ reverse file
      dir = reverse dr
     in do
       dir <- if c == '/' then return dir
              else do pwd <- getEnv "PWD"
                      return (pwd ++ '/' : dir)
       let dirSlash = case last dir of
                        '/' -> dir
                        _ -> dir ++ "/"
           baseURI = "file://" ++ dirSlash
       mozdata <- readFile file
       mozEmbedRenderData moz mozdata baseURI mimeType
       mainGUI
    _ -> do p <- getProgName
            putStrLn ("Usage: " ++ p ++ " <htmlfile>")

mimeType = "text/html"

