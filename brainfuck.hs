module Brainfuck where

import Data.Char (ord, chr)
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
output (_, x, _) = print . chr . fromEnum $ x

-- (,): accept 1 byte of input; store its value in the byte at the data pointer
input :: Memory -> IO Memory
input (ls, _, rs) = do
  c <- getChar
  let byte :: Word8
      byte = toEnum $ ord c
  return (ls, byte, rs)

-- ([): If the byte at the data pointer (i.e. the focus of the Memory zipper) is
-- zero, then this function jumps the instruction pointer (i.e. the focus of the
-- Program zipper) FORWARD to the command after the matching ']' command.
-- If the byte at the data pointer is nonzero, then this function simply
-- increments the instruction pointer forward to the command.
lb :: Program -> Memory -> Program
lb program (_, 0, _) = jumpPast ']' program
  where jumpPast :: Char -> Program -> Program
        jumpPast c (_, _, [])       = error (c:" was not found in program")
        jumpPast c prog@(_, _, r:_) = if r == c
                                      then goRight (goRight prog)
                                      else jumpPast c (goRight prog)

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
        jumpBackTo c ([], _, _)       = error (c:" was not found in program")
        jumpBackTo c prog@(l:_, _, _) = if l == c
                                        then goLeft (goLeft prog)
                                        else jumpBackTo c (goLeft prog)


readProgram :: String -> Program
readProgram (i:is) = ("", i, is)
readProgram ""     = ("", '\0', "") -- no-op

initialMemory :: Memory
initialMemory = ([], 0, repeat 0)


execute :: (Program, Memory) -> IO ()
execute (program@(_, i, is), memory) = case i of
  '>' -> execute (goRight program, incrementDataPointer memory)
  '<' -> execute (goRight program, decrementDataPointer memory)
  '+' -> execute (goRight program, increment memory)
  '-' -> execute (goRight program, decrement memory)
  '.' -> output memory >> execute (goRight program, memory)
  ',' -> input memory >>= \newMemory -> execute (goRight program, newMemory)
  '[' -> execute (lb program memory, memory)
  ']' -> execute (rb program memory, memory)
  _   -> case is of
    _:_ -> execute (goRight program, memory)
    []  -> return ()

main :: IO ()
main = do
  text <- readFile "test.bf"
  let program = readProgram text
  execute (program, initialMemory)
