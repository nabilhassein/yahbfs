module Brainfuck where

import Data.Char           (ord, chr)
import Data.Word           (Word8)
import System.Environment  (getArgs)


-- see http://learnyouahaskell.com/zippers
type Zipper a = ([a], a, [a])

goRight :: Zipper a ->     Either String (Zipper a)
goRight    (ls, x, r:rs) = Right (x:ls, r, rs)
goRight    (_ , _, []  ) = Left  "illegal: cannot go right past end of program"

goLeft :: Zipper a ->     Either String (Zipper a)
goLeft    (l:ls, x, rs) = Right (ls, l, x:rs)
goLeft    ([]  , _, _ ) = Left  "illegal: cannot go left past cell 0"

-- from Wikipedia: https://en.wikipedia.org/wiki/Brainfuck
-- "The brainfuck language uses a simple machine model consisting of the program
-- and instruction pointer, as well as an array of at least 30,000 byte cells
-- initialized to zero; a movable data pointer (initialized to point to the
-- leftmost byte of the array); and two streams of bytes for input and output
-- (most often connected to a keyboard and a monitor respectively, and using
-- the ASCII character encoding)."

-- a Zipper is very natural model for the instruction and data pointers:
-- the current instruction or byte respectively is the focus
type Program  = Zipper Char
type Memory   = Zipper Word8

-- below follow the eight commands of the brainfuck language
-- (>): increment the data pointer (to point to the next cell to the right)
incrementDataPointer :: Memory -> Either String Memory
incrementDataPointer = goRight

-- (<): decrement the data pointer (to point to the next cell to the left)
decrementDataPointer :: Memory -> Either String Memory
decrementDataPointer = goLeft

-- (+): increment the byte at the data pointer
incrementByte :: Memory ->     Memory
incrementByte    (ls, b, rs) = (ls, b+1, rs)

-- (-): decrement the byte at the data pointer
decrementByte :: Memory ->     Memory
decrementByte    (ls, b, rs) = (ls, b-1, rs)

-- (.): output the byte at the data pointer.
output :: Memory ->   IO ()
output    (_, b, _) = print . chr . fromEnum $ b

-- (,): accept 1 byte of input; store its value in the byte at the data pointer
input :: Memory ->    IO Memory
input   (ls, _, rs) = do c <- getChar
                         let byte :: Word8
                             byte = toEnum $ ord c
                         return (ls, byte, rs)

-- ([): If the byte at the data pointer (i.e. the focus of the Memory zipper) is
-- zero, then this function jumps the instruction pointer (i.e. the focus of the
-- Program zipper) FORWARD to the command after the matching ']' command.
-- If the byte at the data pointer is nonzero, then this function simply
-- increments the instruction pointer forward to the command.
leftBracket :: Memory -> Program -> Either String Program
leftBracket    (_, x, _) program  = case x of 0 -> jumpPast ']' program
                                              _ -> goRight program
  where jumpPast :: Char -> Program ->         Either String Program
        jumpPast    c            (_, _, [] ) = Left $ "Illegal program: \
                                                      \missing " ++ [c]
        jumpPast    c       prog@(_, _, r:_) = if r == c
                                               then goRight prog >>= goRight
                                               else goRight prog >>= jumpPast c

-- (]): If the byte at the data pointer (i.e. the focus of the Memory zipper) is
-- zero, then this function simply increments the instruction pointer (i.e. the
-- focus of the Program zipper).
-- If the byte at the data pointer is nonzero then instead of moving the
-- instruction pointer forward to the next command, this function jumps the
-- instruction pointer BACK to the command after the matching '[' command.

-- it is possible to implement this behavior as an unconditional jump back to
-- the corresponding left bracket; but then, if the byte at the data pointer is
-- zero, the program will unnecessarily jump twice, which is inefficient
rightBracket :: Memory -> Program -> Either String Program
rightBracket    (_, x, _) program  = case x of
  0 -> goRight program
  _ -> jumpBack '[' program
  where jumpBack :: Char -> Program ->         Either String Program
        jumpBack    c            ([] , _, _) = Left $ "Illegal program: \
                                                      \ missing " ++ [c]
        jumpBack    c       prog@(l:_, _, _) = if l == c
                                               then Right prog
                                               else goLeft prog >>= jumpBack c


-- done implementing brainfuck commands; this is the heart of the interpreter
execute :: Memory -> Program ->            IO String
execute    _         (_, _, [] )         = return "" -- end of program
execute    memory    program@(_, i, _:_) =
  let step :: (Memory  -> Either String Memory ) -> Memory  ->
              (Program -> Either String Program) -> Program -> IO String
      step updateMemory m updateProgram p = case updateMemory m of
        Left  s  -> return s
        Right m' -> either return (execute m') (updateProgram p)
  in case i of
    '>' -> step incrementDataPointer     memory    goRight               program
    '<' -> step decrementDataPointer     memory    goRight               program
    '+' -> step (return . incrementByte) memory    goRight               program
    '-' -> step (return . decrementByte) memory    goRight               program
    '.' -> output memory >>
           step return                   memory    goRight               program
    ',' -> input memory >>= \newMemory ->
           step return                   newMemory goRight               program
    '[' -> step return                   memory    (leftBracket  memory) program
    ']' -> step return                   memory    (rightBracket memory) program
    _   -> step return                   memory    goRight               program


-- In `execute` above, end of program is signified by an empty list to the right
-- of the focus of the Zipper. But the focus is the current instruction, which
-- must be executed even if no instructions follow it. So in the second pattern,
-- we add a dummy instruction to ensure we execute the final real instruction.
readProgram :: String -> Program
readProgram []         = ([], '\0', []        ) -- no-op
readProgram (i:is)     = ([], i   , is ++ "\0")

initialMemory :: Memory
initialMemory = ([], 0, repeat 0)

run :: String -> IO String
run = execute initialMemory . readProgram


main :: IO String
main = do
  args <- getArgs
  case args of
    []         -> return "Usage: runhaskell brainfuck.hs [brainfuck source file]"
    filename:_ -> readFile filename >>= run
