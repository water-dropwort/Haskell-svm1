------------------------------------------------------------------------------
-- S1s言語パーサー
------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
module S1s.Parser where

import Lib.Parser
import Lib.ParserHelper
import S1s.SyntaxTree
import Control.Applicative

-- | Extended Parser
--
-- >>> parse (bstring' "main") "   main"
-- Just ("main","")
-- >>> parse (char' '{') "   {"
-- Just ('{',"")
-- >>> parse number' "   123"
-- Just (123,"")
bstring' s = skipSpaces >> (bstring s)
char' c  = skipSpaces >> (char c)
number' = skipSpaces >> number

-- | Program Parser
progParser :: Parser Prog
progParser = do
  bstring' "main"
  char' '{'
  expr <- exprParser
  char' '}'
  return $ Prog expr

-- | Expression Parser
exprParser :: Parser Expr
exprParser = do
  term <- termParser
  terms <- many termParser'
  return $ Expr term terms
  where
    opeasParser = do
      c <- (char' '+') <|> (char' '-')
      return if c == '+' then Add else Sub
    termParser' = do
      opeas <- opeasParser
      term <- termParser
      return (opeas, term)

-- | Term Parser
termParser :: Parser Term
termParser = do
  factor <- factorParser
  factors <- many factorParser'
  return $ Term factor factors
  where
    opemdParser = do
      c <- (char' '*') <|> (char' '/')
      return if c == '*' then Mul else Div
    factorParser' = do
      opemd <- opemdParser
      factor <- factorParser
      return (opemd, factor)

-- | Factor Parser
factorParser :: Parser Factor
factorParser = do
  factor <- nFactorParser <|> eFactorParser
  return factor
  where
    nFactorParser = do
      n <- number'
      return $ NFactor n
    eFactorParser = do
      char' '('
      expr <- exprParser
      char' ')'
      return $ EFactor expr
