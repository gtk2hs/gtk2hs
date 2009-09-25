 -- A simple program to demonstrate Vte Binding by Cjacker Huang
module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Vte.Vte
import Graphics.UI.Gtk.Pango.Font

main = do
    initGUI
    window <- windowNew
    onDestroy window mainQuit
    widgetSetSizeRequest window 640 480

    scrolled <- scrolledWindowNew Nothing Nothing
    scrolledWindowSetPolicy scrolled PolicyAutomatic PolicyAutomatic
    vte <- terminalNew
    terminalForkCommand vte Nothing Nothing Nothing Nothing False False False
    font <- fontDescriptionFromString "DejaVu Sans Mono 10"
    terminalSetFont vte font
    containerAdd scrolled vte
    containerAdd window scrolled
    on vte childExited $ mainQuit

    widgetShowAll window

    mainGUI
