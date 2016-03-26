{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- Example of an international dialog box.
import GI.Gtk
       (Box(..), dialogRun, widgetShowAll, boxPackStart, labelSetMarkup,
        labelNew, dialogGetContentArea, pattern STOCK_NO, pattern STOCK_YES,
        dialogAddButton, dialogNew, pattern STOCK_OK, widgetShow)

import Control.Applicative
import Prelude
import Data.Char
import qualified Data.Text as T
import Data.Text (Text)
import qualified GI.Gtk as Gtk (init)
import GI.Gtk.Enums (ResponseType(..))
import Data.GI.Base.ManagedPtr (unsafeCastTo)

main :: IO ()
main = do
  Gtk.init Nothing
  dia <- dialogNew
  dialogAddButton dia STOCK_YES (fromIntegral $ fromEnum ResponseTypeYes)
  dialogAddButton dia STOCK_NO (fromIntegral $ fromEnum ResponseTypeNo)
  contain <- dialogGetContentArea dia >>= unsafeCastTo Box
  theText <- labelNew (Nothing :: Maybe Text)
  labelSetMarkup theText (T.pack arabic)
  boxPackStart contain theText False False 0
  widgetShowAll dia
  res <- dialogRun dia
  case toEnum (fromIntegral res) of
    ResponseTypeNo -> yell
    _ -> return ()

arabic :: String
arabic = markSpan ["size=\"36000\""]  $
 --"Is Haskell a "++markSpan [FontForeground "red"] "fantastic"++" language?"++
 -- Do you find Haskell a fantastic language? (language has a grammatical
 -- mistake in it)
  map chr [0x647,0x644,32,0x62A,0x62C,0x62F,0x646,32]++
  markSpan ["foreground=\"red\""]
    (map chr [0x647,0x622,0x633,0x643,0x622,0x644])++
  map chr [32,0x644,0x63A,0x62A,32,0x645,0x62F,0x647,0x634,0x62A,0x61F]

yell :: IO ()
yell = do
  dia <- dialogNew
  dialogAddButton dia STOCK_OK (fromIntegral $ fromEnum ResponseTypeOk)
  contain <- dialogGetContentArea dia >>= unsafeCastTo Box
  msg <- labelNew (Just "This is not an option.")
  boxPackStart contain msg False False 0
  widgetShow msg
  dialogRun dia
  return ()

markSpan :: [String] -> String -> String
markSpan attrs text = showString "<span".
                      foldr ((.) . showString . (" " ++)) (showChar '>') attrs.
                      showString text.
                      showString "</span>" $ ""
