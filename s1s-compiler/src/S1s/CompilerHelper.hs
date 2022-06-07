------------------------------------------------------------------------------
-- コンパイル処理の補助関数。
-- SyntaxTreeの各型をInt8に変換する
------------------------------------------------------------------------------
module S1s.CompilerHelper where

import S1s.SyntaxTree
import Data.Int

cmdAdd   =  96 :: Int8
cmdSub   = 100 :: Int8
cmdMul   = 104 :: Int8
cmdDiv   = 108 :: Int8
cmdPush  =  16 :: Int8
cmdPrint = -48 :: Int8

compileProgram :: Prog -> [Int8]
compileProgram (Prog expr) = (compileExpr expr) ++ [cmdPrint]

compileExpr :: Expr -> [Int8]
compileExpr (Expr lTerm rTerms) =
  let l  = compileTerm lTerm
      rs = foldl compileRTerm [] rTerms
  in  l ++ rs
  where
    compileOpeAS ope = case ope of
                         Add -> [cmdAdd]
                         Sub -> [cmdSub]
    compileRTerm rs (opeAs, term) = rs ++ (compileTerm term) ++ (compileOpeAS opeAs)

compileTerm :: Term -> [Int8]
compileTerm (Term lFactor rFactors) =
  let l  = compileFactor lFactor
      rs = foldl compileRFactor [] rFactors
  in  l ++ rs
  where
    compileOpeMD ope = case ope of
                         Mul -> [cmdMul]
                         Div -> [cmdDiv]
    compileRFactor rs (opeMd, factor) = rs ++ (compileFactor factor) ++ (compileOpeMD opeMd)

compileFactor :: Factor -> [Int8]
compileFactor (NFactor v) = [cmdPush, v]
compileFactor (EFactor expr) = compileExpr expr
