------------------------------------------------------------------------------
-- コンパイル処理
------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module S1s.Compiler where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.Word             (Word8)
import           Lib.Parser            (parse)
import           S1s.CompilerHelper
import           S1s.Parser            (progParser)
import           System.IO             (FilePath)

-- S1s言語で書かれたファイルを読み込み、バイナリファイルを生成する。
compile :: FilePath -> FilePath -> IO ()
compile srcfile outfile = do
  srccode <- BS8.readFile srcfile
  case parse progParser srccode of
    Nothing       -> BS8.putStrLn "Failed to parse source code."
    Just (prog,_) -> do let bins = BS.pack $ map toWord8 $ compileProgram prog -- [Int8] -> [Word8] -> ByteString
                        BS.writeFile outfile bins
                        BS8.putStrLn "Compile completed."
  where
    toWord8 x = (fromIntegral x) :: Word8
