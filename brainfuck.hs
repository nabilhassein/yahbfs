module Brainfuck where

import Data.Char (ord)
import Data.Word (Word8)


type Zipper a = ([a], a, [a])

goRight :: Zipper a -> Zipper a
goRight (ls, x, r:rs) = (x:ls, r, rs)
goRight (_,  _, []  ) = error "illegal: cannot go right past end of program"

goLeft :: Zipper a -> Zipper a
goLeft (l:ls, x, rs) = (ls, l, x:rs)
goLeft ([]  , _, _ ) = error "illegal: cannot go left past cell 0"


type Program  = Zipper Char
type Memory   = Zipper Word8

initialMemory :: Memory
initialMemory = ([], 0, repeat 0)


-- (>): increment the data pointer (to point to the next cell to the right)
incrementDataPointer :: Memory -> Memory
incrementDataPointer = goRight

-- (<): decrement the data pointer (to point to the next cell to the left)
decrementDataPointer :: Memory -> Memory
decrementDataPointer = goLeft

-- (+): increment the byte at the data pointer
increment :: Memory -> Memory
increment (ls, x, rs) = (ls, x+1, rs)

-- (-): decrement the byte at the data pointer.
decrement :: Memory -> Memory
decrement (ls, x, rs) = (ls, x-1, rs)

-- (.): output the byte at the data pointer.
output :: Memory -> IO ()
output (_, x, _) = print x

-- (,): accept 1 byte of input; store its value in the byte at the data pointer
input :: Memory -> IO Memory
input (ls, _, rs) = do
  char <- getChar
  let byte :: Word8
      byte = toEnum $ ord char
  return (ls, byte, rs)

-- ([): If the byte at the data pointer (i.e. the focus of the Memory zipper) is
-- zero, then this function jumps the instruction pointer (i.e. the focus of the
-- Program zipper) FORWARD to the command after the matching ']' command.
-- If the byte at the data pointer is nonzero, then this function simply
-- increments the instruction pointer forward to the command.
lb :: Program -> Memory -> Program
lb program (_, 0, _) = jumpPast ']' program
  where jumpPast :: Char -> Program -> Program
        jumpPast char (_, _, [])       = error (char:" not found in program")
        jumpPast char prog@(_, _, r:_) = if r == char
                                         then goRight (goRight prog)
                                         else jumpPast char (goRight prog)

lb program _         = goRight program

-- (]): If the byte at the data pointer (i.e. the focus of the Memory zipper) is
-- zero,then this function simply increments the instruction pointer (i.e. the
-- focus of the Program zipper).
-- If the byte at the data pointer is nonzero then instead of moving the
-- instruction pointer forward to the next command, this function jumps the
-- instruction pointer BACK to the command after the matching '[' command.
rb :: Program -> Memory -> Program
rb program (_, 0, _) = goRight program
rb program _         = jumpBackTo '[' program
  where jumpBackTo :: Char -> Program -> Program
        jumpBackTo char ([], _, _)       = error (char:" not found in program")
        jumpBackTo char prog@(l:_, _, _) = if l == char
                                           then goLeft (goLeft prog)
                                           else jumpBackTo char (goLeft prog)
