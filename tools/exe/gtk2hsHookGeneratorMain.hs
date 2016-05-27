module Main (main) where
import System.Environment (getArgs)
import HookGenerator (hookGen)
main = getArgs >>= hookGen >>= putStr
