------------------------------------------------------------------------------
-- 『プログラミングHaskell 第2版』の第13章のパーサーをもとに作成したパーサー
------------------------------------------------------------------------------
module Lib.Parser where

import qualified Data.ByteString.Char8 as BS
import           Control.Applicative

newtype Parser a = P (BS.ByteString -> Maybe (a, BS.ByteString))

parse :: Parser a -> BS.ByteString -> Maybe (a, BS.ByteString)
parse (P p) input = p input

-- | Functor
--
-- >>> import Data.Char (digitToInt)
--
-- >>> let item = P BS.uncons
-- >>> parse (fmap digitToInt item) "abc"
-- Just (10,"bc")
-- >>> parse (fmap digitToInt item) ""
-- Nothing
--
instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P (\input -> fmap (\(v,out) -> (g v, out)) $ parse p input)

-- | Applicative
--
-- >>> let item = P BS.uncons
-- >>> import Data.Char (digitToInt)
-- >>> parse (pure digitToInt <*> item) "abc"
-- Just (10,"bc")
-- >>> parse (pure (\x y z -> (x,z)) <*> item <*> item <*> item) "abc"
-- Just (('a','c'),"")
--
instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\input -> Just (v, input))
  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P (\input -> (parse pg input) >>= (\(g,out) -> parse (fmap g px) out))

-- | Monad
--
-- >>> let item = P BS.uncons
-- >>> let three = item >>= (\x1 -> item >> item >>= (\x3 -> return (x1,x3)))
-- >>> parse three "abcd"
-- Just (('a','c'),"d")
--
instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\input -> (parse p input) >>= (\(v, out) -> parse (f v) out))

-- | Alternative
--
-- >>> import Data.Char (isDigit, isAlpha)
-- >>> let digit = P (\input -> if isDigit $ BS.head input then BS.uncons input else Nothing)
-- >>> let alpha = P (\input -> if isAlpha $ BS.head input then BS.uncons input else Nothing)
-- >>> parse (digit <|> alpha <|> empty) "1"
-- Just ('1',"")
-- >>> parse (digit <|> alpha <|> empty) "a"
-- Just ('a',"")
-- >>> parse (digit <|> alpha <|> empty) "("
-- Nothing
--
instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\_ -> Nothing)
  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\input -> case parse p input of
                           Just (v, out) -> Just (v,out)
                           Nothing       -> parse q input)
