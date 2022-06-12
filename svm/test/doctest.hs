module Main (main) where

import Test.DocTest
import System.IO
import Control.Exception

main :: IO ()
main = doctest ["src/SVM"]
