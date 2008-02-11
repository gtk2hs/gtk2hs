import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.ModelView as New

-- Define a column number to hold a string (the first type argument of
-- ColumnId is the type stored in the model which just happens to be a string
-- too). Each model has a map from column numbers to extraction functions.
-- This function is set below using 'treeModelSetColumn'. Column numbers can
-- be any number greater or equal to 0, but they should be low as the stores
-- create a list of extraction functions starting with column zero.
strCol :: New.ColumnId String String
strCol = New.makeColumnIdString 1

main = 
    do
      initGUI
      window <- windowNew
      store <- New.listStoreNew ["red","green","magenta"]
      entry <- entryNew
      completion <- New.entryCompletionNew
      -- Define the extraction function for this column to be the identity.
      New.treeModelSetColumn store strCol id
      set completion [entryCompletionModel := Just store,
                      entryCompletionTextColumn := strCol]
      entrySetCompletion entry completion
      set window [containerChild := entry]
      widgetShowAll window
      onDestroy window mainQuit
      mainGUI