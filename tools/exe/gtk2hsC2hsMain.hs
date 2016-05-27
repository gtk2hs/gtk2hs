module Main (main) where
import System.Environment (getArgs)
import Gtk2HsC2Hs (c2hsMain)
main = getArgs >>= c2hsMain
