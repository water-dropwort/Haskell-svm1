------------------------------------------------------------------------------
-- スタック
------------------------------------------------------------------------------
module SVM.Stack where

data Stack a = Stack [a] deriving Show

pop :: Stack a -> (a, Stack a)
pop (Stack [])     = error "Stack is empty."
pop (Stack (x:xs)) = (x, Stack xs)

push :: Stack a -> a -> Stack a
push (Stack xs) x = Stack (x:xs)

peek :: Stack a -> a
peek (Stack [])    = error "Stack is empty."
peek (Stack (x:_)) = x

count :: Stack a -> Int
count (Stack xs) = length xs

newStack :: Stack a
newStack = Stack []
