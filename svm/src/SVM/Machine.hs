------------------------------------------------------------------------------
-- サンプル仮想機械1号(SVM1)
------------------------------------------------------------------------------
module SVM.Machine where

import Control.Monad   (when)
import Data.ByteString (readFile, unpack)
import Data.Int
import Data.Word
import Prelude         hiding (readFile)
import SVM.Stack
import System.IO       (FilePath)

-- | サンプル仮想機械
data Svm1 = Svm1 { code :: [Int8]
                 , codeLength :: Int
                 , operandStack :: Stack Int8
                 , programCounter :: Int
                 } deriving Show

-- | コマンド
cmdAdd   =  96 :: Int8
cmdSub   = 100 :: Int8
cmdMul   = 104 :: Int8
cmdDiv   = 108 :: Int8
cmdPush  =  16 :: Int8
cmdPrint = -48 :: Int8

-- | バイナリファイルを読み込む。
loadCode :: FilePath -> IO [Int8]
loadCode binpath = do
  bs <- readFile binpath
  return $ map fromIntegral $ unpack bs

-- | コマンドをもとにSVMを新規作成
newSvm1 :: [Int8] -> Svm1
newSvm1 code = Svm1 code (length code) newStack 0

-- | コマンド実行
--
-- >>> executeCommand cmdAdd $ Svm1 [] 0 (Stack [1,2,99]) 0
-- Svm1 {code = [], codeLength = 0, operandStack = Stack [3,99], programCounter = 0}
-- >>> executeCommand cmdSub $ Svm1 [] 0 (Stack [1,2,99]) 0
-- Svm1 {code = [], codeLength = 0, operandStack = Stack [1,99], programCounter = 0}
-- >>> executeCommand cmdMul $ Svm1 [] 0 (Stack [3,2,99]) 0
-- Svm1 {code = [], codeLength = 0, operandStack = Stack [6,99], programCounter = 0}
-- >>> executeCommand cmdDiv $ Svm1 [] 0 (Stack [3,9,99]) 0
-- Svm1 {code = [], codeLength = 0, operandStack = Stack [3,99], programCounter = 0}
-- >>> executeCommand cmdPush $ Svm1 [5] 1 (Stack [1,3]) 0
-- Svm1 {code = [5], codeLength = 1, operandStack = Stack [5,1,3], programCounter = 1}
-- >>> executeCommand cmdPrint $ Svm1 [] 0 (Stack [100,99]) 0
-- 100
-- Svm1 {code = [], codeLength = 0, operandStack = Stack [100,99], programCounter = 0}
executeCommand :: Int8 -> Svm1 -> IO Svm1
executeCommand cmd svm1@(Svm1 _code _len _stack _pc)
  -- スタックへプッシュ
  | cmd == cmdPush  = return $ svm1 { operandStack = push _stack (_code !! _pc), programCounter = _pc + 1}
  -- 四則演算
  | cmd == cmdAdd   = return $ binOp (+)
  | cmd == cmdSub   = return $ binOp (-)
  | cmd == cmdMul   = return $ binOp (*)
  | cmd == cmdDiv   = return $ binOp (div)
  -- 出力
  | cmd == cmdPrint = (print $ peek _stack) >> (return svm1)
  -- 未定義コマンド
  | otherwise       = error $ concat ["Undefined command:", show cmd]
  where
    binOp op = let (b, _stack1) = pop _stack
                   (a, _stack2) = pop _stack1
               in  svm1 { operandStack = push _stack2 (op a b) }

-- | SVM実行(内部用)
-- | デバッグモードの時はSVMの内部状態を出力する。
--
-- >>> let cmds = [cmdPush,10,cmdPush,5,cmdAdd,cmdPush,7,cmdSub,cmdPush,2,cmdMul,cmdPush,4,cmdDiv,cmdPrint]
-- >>> let svm = newSvm1 cmds
-- >>> (_runSvm1 False svm) >> return ()
-- 4
_runSvm1 :: Bool -> Svm1 -> IO Svm1
_runSvm1 isDebug svm1 = do
  when isDebug $ print svm1
  _runSvm1' svm1
  where
    _runSvm1' svm@(Svm1 _code _len _stack _pc) = do
      if (_pc < _len) then do
        let cmd = _code !! _pc
        svm' <- executeCommand cmd $ svm{programCounter = _pc + 1}
        when isDebug $ print svm'
        _runSvm1' svm'
      else
        return svm

-- | SVM実行
runSvm1 :: FilePath -> IO ()
runSvm1 svmpath = do
  codes <- loadCode svmpath
  let svm1 = newSvm1 codes
  _runSvm1 False svm1
  return ()
