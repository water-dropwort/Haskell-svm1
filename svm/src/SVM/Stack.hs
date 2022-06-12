------------------------------------------------------------------------------
-- スタック
------------------------------------------------------------------------------
module SVM.Stack where

data Stack a = Stack [a] deriving Show

-- | Stack Test
--
-- >>> let st = newStack :: Stack Int
-- >>> let st1 = push st 3
-- >>> print st1
-- Stack [3]
-- >>> let st2 = push st1 5
-- >>> print st2
-- Stack [5,3]
-- >>> pop st2
-- (5,Stack [3])
-- >>> peek st2
-- 5
-- >>> count st2
-- 2
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
