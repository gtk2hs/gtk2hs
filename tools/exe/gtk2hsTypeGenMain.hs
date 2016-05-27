module Main (main) where
import System.Environment (getArgs)
import TypeGen (typeGen)
main = getArgs >>= typeGen >>= putStr
