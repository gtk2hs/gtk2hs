module Main where

import Gtk
import Glade

main = do
         initGUI
	 
	 -- load up the glade file
	 dialogXmlM <- xmlNew "simple.glade"
	 let dialogXml = case dialogXmlM of
	       (Just dialogXml) -> dialogXml
	       Nothing -> error "can't find the glade file \"simple.glade\" in the current directory"
	 
	 -- get a handle on a couple widgets from the glade file
	 window <- xmlGetWidget dialogXml castToWindow "window1"
	 button <- xmlGetWidget dialogXml castToButton "button1"
	 
	 -- do something with the widgets, just to prove it works
	 button `onClicked` putStrLn "button pressed!"
	 window `onDestroy` mainQuit
	 
	 -- show everything
	 widgetShowAll window
	 mainGUI
