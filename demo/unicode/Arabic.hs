-- Example of an international dialog box.
import Gtk
import Char
import Exception

main :: IO ()
main = do
  initGUI
  dia <- dialogNew
  dialogAddButton dia stockYes ResponseYes
  dialogAddButton dia stockNo ResponseNo
  contain <- dialogGetUpper dia
  theText <- labelNew Nothing 
  labelSetMarkup theText arabic
  boxPackStartDefaults contain theText
  widgetShowAll dia
  res <- dialogRun dia
  case res of
    ResponseNo -> yell
    _ -> return ()

arabic :: Markup
arabic = markSpan [FontSize $ FSPoint 48]  $
 --"Is Haskell a "++markSpan [FontForeground "red"] "fantastic"++" language?"++
 -- Do you find Haskell a fantastic language? (language has a grammatical
 -- mistake in it)
  map chr [0x647,0x644,32,0x62A,0x62C,0x62F,0x646,32,0x647,0x622,
           0x633,0x643,0x622,0x644,32,0x644,0x63A,0x62A,32]++
  markSpan [FontForeground "red"]  
    (map chr [0x645,0x62F,0x647,0x634,0x62A])++
  [chr 0x61F]

yell :: IO ()
yell = do
  dia <- dialogNew
  dialogAddButton dia stockOk ResponseOk
  contain <- dialogGetUpper dia
  msg <- labelNew (Just "This is not an option.")
  contain `boxPackStartDefaults` msg
  widgetShow msg
  dialogRun dia
  return ()
