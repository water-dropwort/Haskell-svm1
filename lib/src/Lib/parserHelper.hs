------------------------------------------------------------------------------
-- パーサーの補助関数群
------------------------------------------------------------------------------
module Lib.ParserHelper where

import           Lib.Parser
import           Data.Char
import qualified Data.ByteString.Char8 as BS
import           Data.Int
import           Control.Applicative

-- | 1文字取得
--
-- >>> parse item "hoge"
-- Just ('h',"oge")
-- >>> parse item ""
-- Nothing
--
item :: Parser Char
item = P BS.uncons

-- | 先頭の文字を取得し、かつ、それが条件に会えば、1文字取得
--
-- >>> parse (sat (\_->True)) "ABC"
-- Just ('A',"BC")
-- >>> parse (sat (\_->True)) ""
-- Nothing
-- >>> parse (sat (\_->False)) "ABC"
-- Nothing
-- >>> parse (sat (\_->False)) ""
-- Nothing
--
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

-- | スペース取得
--
-- >>> parse space "  hoge"
-- Just (' '," hoge")
-- >>> parse space "hoge"
-- Nothing
--
space :: Parser Char
space = sat isSpace

-- | 数字取得
--
-- >>> parse digit "123"
-- Just ('1',"23")
-- >>> parse digit "a123"
-- Nothing
--
digit :: Parser Char
digit = sat isDigit

-- | 引数の文字(Char)に一致すれば取得
--
-- >>> parse (char 'h') "hoge"
-- Just ('h',"oge")
-- >>> parse (char 'h') "fuga"
-- Nothing
--
char :: Char -> Parser Char
char x = sat (== x)

-- | 引数の文字列(ByteString)に一致すれば取得
--
-- >>> parse (bstring "hoge") "hoge"
-- Just ("hoge","")
-- >>> parse (bstring "hoge") "hog"
-- Nothing
-- >>> parse (bstring "hoge") "fuga"
-- Nothing
-- >>> parse (bstring "") "hogefuga"
-- Just ("","hogefuga")
-- >>> parse (bstring "") ""
-- Just ("","")
--
bstring :: BS.ByteString -> Parser BS.ByteString
bstring bs
  | BS.null bs = return BS.empty
  | otherwise  = case BS.uncons bs of
                   Just (x, xs) -> (char x) >> (bstring xs) >> (return bs)
                   Nothing      -> return BS.empty
-- | 非負の整数取得
--
-- >>> parse number "123"
-- Just (123,"")
-- >>> parse number "-123"
-- Nothing
-- >>> parse number ""
-- Nothing
--
number :: Parser Int8
number = P (\input -> case parse (many digit) input of
                        Just (ds,out) -> if null ds then Nothing else Just (read ds, out)
                        Nothing       -> Nothing)
-- | スペースを消費する。
--
-- >>> parse skipSpaces " hoge "
-- Just ((),"hoge ")
-- >>> parse skipSpaces "hoge"
-- Just ((),"hoge")
-- >>> parse skipSpaces ""
-- Just ((),"")
--
skipSpaces :: Parser ()
skipSpaces = do many space
                return ()
