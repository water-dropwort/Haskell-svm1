{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Exit

import           S1s.Compiler
import           SVM.Machine
import           SVM.Stack
import qualified Data.ByteString.Char8 as BS
import           Data.Int (Int8)

main :: IO ()
main = do
  runTest "main{1}" [1]
  runTest "main{1+2}" [3]
  runTest "main{1-3}" [-2]
  runTest "main{3*5}" [15]
  runTest "main{10/2}" [5]
  runTest "main{(1+2)+(3+4)}" [10]
  runTest "main{(1+2)*(9-6)/3}" [3]
  exitSuccess

runTest :: BS.ByteString -> [Int8] -> IO ()
runTest src stack = do
  case compile' src of
    Nothing  -> exitFailure
    Just bin -> do svm' <- _runSvm1 False $ newSvm1 $ map fromIntegral  bin
                   case operandStack svm' of
                     Stack st -> if st == stack then return () else exitFailure
