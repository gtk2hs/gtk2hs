module Main where

import Gtk (initGUI, mainGUI)
import System.Gnome.GConf

import Monad (when)
import System.Exit (exitFailure)
import List (intersperse)

main = do
  -- connect to gconf
  conf <- gconfGetDefault

  -- for the purposes of this demo check for key and display usage message
  exists <- conf `gconfDirExists` "/apps/gtk2hs-gconf-demo"
  when (not exists) (do putStrLn usageMessage
                        exitFailure)

  -- get and print initial values
  (intValue :: Int) <- conf `gconfGet` "/apps/gtk2hs-gconf-demo/intValue"
  (boolValue :: Maybe Bool) <- conf `gconfGet` "/apps/gtk2hs-gconf-demo/boolValue"
  (floatValue :: Double) <- conf `gconfGet` "/apps/gtk2hs-gconf-demo/floatValue"
  (stringValue :: String) <- conf `gconfGet` "/apps/gtk2hs-gconf-demo/stringValue"
  (pairValue :: (Int,Bool)) <- conf `gconfGet` "/apps/gtk2hs-gconf-demo/pairValue"
  (listValue :: [Int]) <- conf `gconfGet` "/apps/gtk2hs-gconf-demo/listValue"
 
  print intValue
  print boolValue
  print floatValue
  print stringValue
  print pairValue
  print listValue
  
  -- register for notification of changes
  conf `gconfAddDir` "/apps/gtk2hs-gconf-demo"
  
  -- using the prefered API which allows you to specify the key/dir of interest.
  -- This is usuall what you want because you'll do different things in response
  -- to changes in different keys. Also, it allows you to use native types rather
  -- than converting from a dynamic type.
  gconfNotifyAdd conf "/apps/gtk2hs-gconf-demo/intValue"
    doSomethingWhenIntValueChanges
  gconfNotifyAdd conf "/apps/gtk2hs-gconf-demo/boolValue"
    doSomethingWhenBoolValueChanges
  gconfNotifyAdd conf "/apps/gtk2hs-gconf-demo/floatValue"
    doSomethingWhenFloatValueChanges
  gconfNotifyAdd conf "/apps/gtk2hs-gconf-demo/stringValue"
    doSomethingWhenStringValueChanges
  gconfNotifyAdd conf "/apps/gtk2hs-gconf-demo/pairValue"
    doSomethingWhenPairValueChanges
  gconfNotifyAdd conf "/apps/gtk2hs-gconf-demo/listValue"
    doSomethingWhenListValueChanges

  -- and the other API (which gives you notifications on everything)
  conf `afterValueChanged` doSomethingWhenAnyKeyChanges
                  
  -- run the glib main loop otherwise we wouldn't wait for changes
  putStrLn $ "waiting for any changes in the gconf dir"
          ++ "\"/apps/gtk2hs-gconf-demo\""
  initGUI
  mainGUI


-- Our various doSomething* functions
--
doSomethingWhenIntValueChanges :: String -> Int -> IO ()
doSomethingWhenIntValueChanges key value =
    putStrLn $ "[method1] intValue changed to " ++ show value

-- This one is designed to cope with the key being unset
doSomethingWhenBoolValueChanges :: String -> Maybe Bool -> IO ()
doSomethingWhenBoolValueChanges key (Just value) =
    putStrLn $ "[method1] boolValue changed to " ++ show value
doSomethingWhenBoolValueChanges key Nothing =
    putStrLn $ "[method1] boolValue was unset"

doSomethingWhenFloatValueChanges :: String -> Double -> IO ()
doSomethingWhenFloatValueChanges key value =
    putStrLn $ "[method1] floatValue changed to " ++ show value

doSomethingWhenStringValueChanges :: String -> String -> IO ()
doSomethingWhenStringValueChanges key value =
    putStrLn $ "[method1] stringValue changed to " ++ show value

doSomethingWhenPairValueChanges :: String -> (Int, Bool) -> IO ()
doSomethingWhenPairValueChanges key value =
    putStrLn $ "[method1] pairValue changed to " ++ show value

doSomethingWhenListValueChanges :: String -> [Int] -> IO ()
doSomethingWhenListValueChanges key value =
    putStrLn $ "[method1] listValue changed to " ++ show value


doSomethingWhenAnyKeyChanges :: String -> Maybe GConfValueDyn -> IO ()
doSomethingWhenAnyKeyChanges key (Just value) =
  putStrLn $ "[method2] the key " ++ key ++ " changed to " ++ showGConfValue value
doSomethingWhenAnyKeyChanges key Nothing =
  putStrLn $ "[method2] the key " ++ key ++ " was unset"


-- Helper function to display a value and its type
-- This is not an important part of the demo
--
showGConfValue :: GConfValueDyn -> String
showGConfValue value =
  showGConfValue_ValueOnly value ++ " :: " ++ showGConfValue_Type value

showGConfValue_ValueOnly :: GConfValueDyn -> String
showGConfValue_ValueOnly (GConfValueString s) = show s
showGConfValue_ValueOnly (GConfValueInt n) = show n
showGConfValue_ValueOnly (GConfValueBool b) = show b
showGConfValue_ValueOnly (GConfValueFloat f) = show f
showGConfValue_ValueOnly (GConfValueList as) =
  "[" ++ (concat $ intersperse "," $ map showGConfValue_ValueOnly as) ++ "]"
showGConfValue_ValueOnly (GConfValuePair (a,b)) =
      "(" ++ showGConfValue_ValueOnly a
  ++ ", " ++ showGConfValue_ValueOnly b ++ ")"


showGConfValue_Type :: GConfValueDyn -> String
showGConfValue_Type (GConfValueString s) = "String"
showGConfValue_Type (GConfValueInt n) = "Int"
showGConfValue_Type (GConfValueBool b) = "Bool"
showGConfValue_Type (GConfValueFloat f) = "Double"
-- gconf does type empty lists too but our GConfValueDyn cannot  represent
-- them using the GConfValueClass is preferable in this sense as it can type
-- all the GConfValue stuff exactly (so long as that type is known statically)
showGConfValue_Type (GConfValueList []) = "[unknown]"
showGConfValue_Type (GConfValueList (a:_)) = "[" ++ showGConfValue_Type a ++ "]"
showGConfValue_Type (GConfValuePair (a,b)) = "(" ++ showGConfValue_Type a ++ ", "
                                                 ++ showGConfValue_Type b ++ ")"

usageMessage =
     "To use this gconf demo program, first create the required gconf entrys.\n"
  ++ "Use the following commands:\n"
  ++ "  gconftool-2 --set /apps/gtk2hs-gconf-demo/intValue --type int 3\n"
  ++ "  gconftool-2 --set /apps/gtk2hs-gconf-demo/boolValue --type bool false\n"
  ++ "  gconftool-2 --set /apps/gtk2hs-gconf-demo/floatValue --type float 3.141592\n"
  ++ "  gconftool-2 --set /apps/gtk2hs-gconf-demo/stringValue --type string foo\n"
  ++ "  gconftool-2 --set /apps/gtk2hs-gconf-demo/pairValue --type pair \\\n"
  ++ "              --car-type int --cdr-type bool \"(3,false)\"\n"
  ++ "  gconftool-2 --set /apps/gtk2hs-gconf-demo/listValue --type list \\\n"
  ++ "              --list-type int \"[0,1,2,3,4]\"\n"
  ++ "This demo will display the values of these keys and then watch them for\n"
  ++ "changes. Use the gconf-editor program to change the values of these keys.\n"
  ++ "Hit ^C when you get bored.\n"
  ++ "To delete the keys when you're finnished with this demo use:\n"
  ++ "  gconftool-2 --recursive-unset /apps/gtk2hs-gconf-demo"
