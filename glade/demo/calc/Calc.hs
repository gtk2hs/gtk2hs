module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

import Data.IORef

import qualified CalcModel as Calc

main = do
  initGUI
	 
  -- load up the glade file
  calcXmlM <- xmlNew "calc.glade"
  let calcXml = case calcXmlM of
                  (Just calcXml) -> calcXml
                  Nothing -> error "can't find the glade file \"calc.glade\" \
                                   \in the current directory"
	 
   -- get a handle on a some widgets from the glade file
  window <- xmlGetWidget calcXml castToWindow "calcwindow"
  display <- xmlGetWidget calcXml castToLabel "display"
  
  -- a list of the names of the buttons and the actions associated with them
  let buttonNamesAndOperations = numbericButtons ++ otherButtons
      numbericButtons = [ ("num-" ++ show n, Calc.enterDigit n)
                        | n <- [0..9] ]
      otherButtons =
        [("decimal", Calc.enterDecimalPoint)
        ,("op-plus", Calc.enterBinOp Calc.plus)
        ,("op-minus", Calc.enterBinOp Calc.minus)
        ,("op-times", Calc.enterBinOp Calc.times)
        ,("op-divide", Calc.enterBinOp Calc.divide)
        ,("equals", Calc.evaluate)
        ,("clear", \_ -> Just ("0", Calc.clearCalc))]
  
  -- action to do when a button corresponding to a calculator operation gets
  -- pressed: we update the calculator state and display the new result.
  -- These calculator operations can return Nothing for when the operation
  -- makes no sense, we do nothing in this case.
  calcRef <- newIORef Calc.clearCalc
  let calcOperation operation = do
        calc <- readIORef calcRef
        case operation calc of
          Nothing -> return ()
          Just (result, calc') -> do
            display `labelSetLabel` ("<big>" ++ result ++ "</big>")
            writeIORef calcRef calc'

      -- get a reference to a button from the glade file and attach the
      -- handler for when the button is pressed
      connectButtonToOperation name operation = do
        button <- xmlGetWidget calcXml castToButton name
        button `onClicked` calcOperation operation
  
  -- connect up all the buttons with their actions.
  mapM_ (uncurry connectButtonToOperation) buttonNamesAndOperations
  
  -- make the program exit when the main window is closed
  window `onDestroy` mainQuit
 
  -- show everything and run the main loop
  widgetShowAll window
  mainGUI
