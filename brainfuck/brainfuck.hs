module Brainfuck where

import Data.Char          (ord, chr)
import Data.Word          (Word8)
import System.Environment (getArgs)


-- see http://learnyouahaskell.com/zippers
type Zipper a = ([a], a, [a])

goRight :: Zipper a ->     Zipper a
goRight    (ls, x, r:rs) = (x:ls, r, rs)
goRight    (_,  _, []  ) = error "illegal: cannot go right past end of program"

goLeft :: Zipper a ->     Zipper a
goLeft    (l:ls, x, rs) = (ls, l, x:rs)
goLeft    ([]  , _, _ ) = error "illegal: cannot go left past cell 0"

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
incrementDataPointer :: Memory -> Memory
incrementDataPointer = goRight

-- (<): decrement the data pointer (to point to the next cell to the left)
decrementDataPointer :: Memory -> Memory
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
lb :: Program -> Memory ->   Program
lb    program    (_, x, _) = case x of 0 -> jumpPast ']' program
                                       _ -> goRight program
  where jumpPast :: Char -> Program ->         Program
        jumpPast    c            (_, _, [] ) = error (c:" not found in program")
        jumpPast    c       prog@(_, _, r:_) = if r == c
                                               then goRight (goRight prog)
                                               else jumpPast c (goRight prog)

-- (]): If the byte at the data pointer (i.e. the focus of the Memory zipper) is
-- zero, then this function simply increments the instruction pointer (i.e. the
-- focus of the Program zipper).
-- If the byte at the data pointer is nonzero then instead of moving the
-- instruction pointer forward to the next command, this function jumps the
-- instruction pointer BACK to the command after the matching '[' command.
rb :: Program -> Memory ->   Program
rb    program    (_, x, _) = case x of 0 -> goRight program
                                       _ -> jumpBack '[' program
  where jumpBack :: Char -> Program ->         Program
        jumpBack    c            ([] , _, _) = error (c:" not found in program")
        jumpBack    c       prog@(l:_, _, _) = if l == c
                                               then prog
                                               else jumpBack c (goLeft prog)


-- done implementing brainfuck commands; this is the heart of the interpreter
execute :: Program ->          Memory -> IO ()
execute            (_, _, [] ) _       = return () -- no more instructions; end
execute    program@(_, i, _:_) memory  =
  let step :: (Memory -> Memory) -> Program -> Memory -> IO ()
      step    update                prog       mem     = execute (goRight prog)
                                                                 (update mem)
  in case i of
    '>' -> step incrementDataPointer program memory
    '<' -> step decrementDataPointer program memory
    '+' -> step incrementByte        program memory
    '-' -> step decrementByte        program memory
    '.' -> output memory >> step id  program memory
    ',' -> input memory >>= execute (goRight program)
    '[' -> execute (lb program memory) memory
    ']' -> execute (rb program memory) memory
    _   -> step id program memory -- any other byte is a comment/no-op


-- this program can do just one thing: read a filename as an argument,
-- interpret that file's contents as a brainfuck program, and execute it
-- all command line arguments except the first are ignored
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: runhaskell brainfuck.hs [brainfuck source file]"
    filename:_ -> do
      program <- readFile filename
      execute (readProgram program) initialMemory

  where readProgram :: String -> Program
        readProgram ""         = (undefined, undefined, []) -- no-op
        readProgram (i:is)     = (""       , i        , is ++ ['\0'])

        initialMemory :: Memory
        initialMemory = ([], 0, repeat 0) -- infinite Zipper of zeroes
