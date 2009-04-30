module Names where

import MarshalFixup (cTypeNameToHSType, fixCFunctionName)
import Utils (splitBy, lowerCaseFirstWord, upperCaseFirstChar)
import Data.Char as Char (toLower, isUpper, isLower)

cFuncNameToHsName :: String -> String
cFuncNameToHsName =
    lowerCaseFirstWord
  . MarshalFixup.cTypeNameToHSType
  . toStudlyCapsWithFixups
  . takeWhile ('('/=)

cParamNameToHsName :: String -> String
cParamNameToHsName  =          --change "gtk_foo_bar" to "gtkFooBar"
    lowerCaseFirstWord
  . toStudlyCaps

cConstNameToHsName :: String -> String
cConstNameToHsName  =          --change "GTK_UPDATE_DISCONTINUOUS" to "UpdateDiscontinuous"
    MarshalFixup.cTypeNameToHSType
  . toStudlyCaps
  . map Char.toLower

cFuncNameToHsPropName :: String -> String
cFuncNameToHsPropName =
    concatMap upperCaseFirstChar
  . map fixCFunctionName
  . tail
  . dropWhile (/="get")
  . filter (not.null)
  . splitBy '_'

cAttrNametoHsName :: String -> String
cAttrNametoHsName  =          --change "label-xalign" to "LabelXAlign"
    toStudlyCapsWithFixups
  . map dashToUnderscore
  where dashToUnderscore '-' = '_'
        dashToUnderscore  c  =  c

toStudlyCaps :: String -> String
toStudlyCaps =                 --change "gtk_foo_bar" to "GtkFooBar"
    concatMap upperCaseFirstChar
  . filter (not.null) --to ignore tailing underscores
  . splitBy '_'

toStudlyCapsWithFixups :: String -> String
toStudlyCapsWithFixups =                 --change "gtk_foo_bar" to "GtkFooBar"
    concatMap upperCaseFirstChar
  . map MarshalFixup.fixCFunctionName
  . filter (not.null) --to ignore tailing underscores
  . splitBy '_'

hsTypeNameToCGetType :: String -> String
hsTypeNameToCGetType hsTypeName =
  convert False hsTypeName ++ "_get_type"
  where convert _         []                 = []
        convert True      (c:cs) | isUpper c = '_' : convert False (c:cs)
        convert lastLower (c:cs)             = toLower c : convert (isLower c) cs
