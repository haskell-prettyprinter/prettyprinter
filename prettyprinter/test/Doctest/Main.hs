module Main (main) where

import Test.DocTest (mainFromCabal)
import System.Environment (getArgs)

main :: IO ()
main = mainFromCabal "prettyprinter" =<< getArgs
