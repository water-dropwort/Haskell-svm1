------------------------------------------------------------------------------
-- パーサーの補助関数群
------------------------------------------------------------------------------
module Lib.ParserHelper where

import           Lib.Parser
import           Data.Char
import qualified Data.ByteString.Char8 as BS
import           Data.Int
import           Control.Applicative

-- 1文字取得
item :: Parser Char
item = P BS.uncons

-- 先頭文字が条件に会えばそれを返す。
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

-- スペース(など)取得
space :: Parser Char
space = sat isSpace

-- 数字取得
digit :: Parser Char
digit = sat isDigit

-- 引数の文字(Char)に一致すれば取得
char :: Char -> Parser Char
char x = sat (== x)

-- 引数の文字列(ByteString)に一致すれば取得
bstring :: BS.ByteString -> Parser BS.ByteString
bstring bs
  | BS.null bs = return BS.empty
  | otherwise  = do case BS.uncons bs of
                      Just (x, xs) -> (char x) >> (bstring xs) >> (return bs)
                      Nothing      -> return BS.empty

-- 非負の整数取得
number :: Parser Int8
number = P (\input -> case parse (many digit) input of
                        Just (ds,out) -> if null ds then Nothing else Just (read ds, out)
                        Nothing       -> Nothing)
-- スペースを消費する。
skipSpaces :: Parser ()
skipSpaces = do many space
                return ()
