module Graphics.UI.Gtk.TreeList.Column where

import System.Glib.GType
import qualified System.Glib.GTypeConstants as GType
import System.Glib.GValue
import System.Glib.GValueTypes

data Column a = ColumnString (a -> String)
              | ColumnInt    (a -> Int)
              | ColumnBool   (a -> Bool)
              | ColumnFloat  (a -> Float)
              | ColumnDouble (a -> Double)

columnGType :: Column a -> GType
columnGType (ColumnString _) = GType.string
columnGType (ColumnInt    _) = GType.int
columnGType (ColumnBool   _) = GType.bool
columnGType (ColumnFloat  _) = GType.float
columnGType (ColumnDouble _) = GType.double

columnSetGValue :: Column a -> a -> GValue -> IO ()
columnSetGValue (ColumnString f) x gvalue = do valueInit gvalue GType.string
                                               valueSetString gvalue (f x)
columnSetGValue (ColumnInt    f) x gvalue = do valueInit gvalue GType.int
                                               valueSetInt gvalue (f x)
columnSetGValue (ColumnBool   f) x gvalue = do valueInit gvalue GType.bool
                                               valueSetBool gvalue (f x)
columnSetGValue (ColumnFloat  f) x gvalue = do valueInit gvalue GType.float
                                               valueSetFloat gvalue (f x)
columnSetGValue (ColumnDouble f) x gvalue = do valueInit gvalue GType.double
                                               valueSetDouble gvalue (f x)
