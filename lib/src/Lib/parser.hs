------------------------------------------------------------------------------
-- 『プログラミングHaskell 第2版』の第13章のパーサーをもとに作成したパーサー
------------------------------------------------------------------------------
module Lib.Parser where

import qualified Data.ByteString.Char8 as BS
import           Control.Applicative

newtype Parser a = P (BS.ByteString -> Maybe (a, BS.ByteString))

parse :: Parser a -> BS.ByteString -> Maybe (a, BS.ByteString)
parse (P p) input = p input

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P (\input -> fmap (\(v,out) -> (g v, out)) $ parse p input)

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\input -> Just (v, input))
  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P (\input -> (parse pg input) >>= (\(g,out) -> parse (fmap g px) out))

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\input -> (parse p input) >>= (\(v, out) -> parse (f v) out))

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\_ -> Nothing)
  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\input -> case parse p input of
                           Just (v, out) -> Just (v,out)
                           Nothing       -> parse q input)
