-- This is an interpreter for the brainfuck programming language.

-- To read this program, I assume only that you can read a very modest amount of
-- the C programming language, the relevant excerpt of which is on Wikipedia,
-- and that you are moderately proficient as a programmer in Haskell.
-- I assume no knowledge of brainfuck or programming language implementations.

-- TODO: translate this program into idiomatic Agda, and for any case that is
-- now handled as an allegedly impossible call to 'error' (i.e. bottom), provide
-- a static guarantee that it does not occur, using dependent types.

module Brainfuck where

import Prelude hiding (putStr, repeat)

import Data.ByteString (hGet, putStr, singleton, unpack)
import Data.List       (elemIndex)
import Data.Stream     (Stream(Cons), repeat)
import Data.Word       (Word8)
import System.IO       (stdin)


-- If you are unfamiliar with brainfuck, see
-- https://en.wikipedia.org/wiki/Brainfuck#Commands
-- and in particular, the second table gives a direct translation of the entire
-- language into C. Read that, and then read this program, and you understand
-- a very simple model of how a universal computing machine can work.

-- from Wikipedia: https://en.wikipedia.org/wiki/Brainfuck
-- "The brainfuck language uses a simple machine model consisting of the program
-- and instruction pointer, as well as an array of at least 30,000 byte cells
-- initialized to zero; a movable data pointer (initialized to point to the
-- leftmost byte of the array); and two streams of bytes for input and output
-- (most often connected to a keyboard and a monitor respectively, and using
-- the ASCII character encoding)."

-- A Zipper is a natural model for brainfuck's instruction and data pointers:
-- the current instruction or byte respectively is the focus.
-- If you are unfamiliar with zippers, for an introduction see
-- http://learnyouahaskell.com/zippers


-- Memory is a tape, infinite in both directions, where each cell is one byte.
-- So we diverge from the specification that the data pointer is initialized
-- to the leftmost byte of some array. I think this is more elegant.
type Memory = (Stream Word8, Word8, Stream Word8)

-- Programs are finite lists of instructions.
-- All but eight characters are simply ignored as comments.
-- This is possibly infinite, but we only use it as finite in this program.
type Program = ([Char], Char, [Char])


-- Here are the two basic functions our interpreter will use for manipulating
-- the program which is its input. Later we will see two more advanced ones.

-- Below, readProgram guarantees that a Right Program is well-formed, i.e. that
-- there are an equal number of '[' and ']', with a '[' always preceding a ']'.
-- As the only control flow in a brainfuck program is to increment the
-- instruction pointer or conditionally jump between brackets, it is impossible
-- for a well-formed Program to attempt to go left past the beginning of the
-- Program, or go right past the end of the Program.
-- So in the second case we call to error because this is impossible.

-- A question: would it still be possible to give a similar static guarantee for
-- a programming language with a notion of self-modifying code? (This is
-- impossible in brainfuck; the program can only operate on the memory tape.)

goRight :: Program      -> Program
goRight    (ls, x, r:rs) = (x:ls, r, rs)
goRight    (_ , _, [] )  = error "bug: this should never occur because in \
                                 \'execute', when we reach this case, \
                                 \we end execution."

goLeft :: Program      -> Program
goLeft    (l:ls, x, rs) = (ls, l, x:rs)
goLeft    ([],   _, _)  = error "bug: this should never occur because \
                                \readProgram guarantees that the input has \
                                \no syntax errors. So we should only move \
                                \right or jump between brackets."


-- below follow the eight commands of the brainfuck language

-- (>): increment the data pointer (to point to the next cell to the right)
incrementDataPointer :: Memory              -> Memory
incrementDataPointer    (ls, x, r `Cons` rs) = (x `Cons` ls, r, rs)

-- (<): decrement the data pointer (to point to the next cell to the left)
decrementDataPointer :: Memory              -> Memory
decrementDataPointer    (l `Cons` ls, x, rs) = (ls, l, x `Cons` rs)

-- (+): increment the byte at the data pointer
-- byte arithmetic is modulo 256; overflow is not an error
incrementByte :: Memory     -> Memory
incrementByte    (ls, b, rs) = (ls, b+1, rs)

-- (-): decrement the byte at the data pointer
-- byte arithmetic is modulo 256; overflow is not an error
decrementByte :: Memory     -> Memory
decrementByte    (ls, b, rs) = (ls, b-1, rs)

-- (.): output the byte at the data pointer
output :: Memory   -> IO ()
output    (_, b, _) = putStr $ singleton b

-- (,): accept 1 byte of input; store its value in the byte at the data pointer
-- if you enter multiple bytes when the program is expecting just one, the
-- other bytes remain in the buffer, and will be passed to future calls to ','
input :: Memory    -> IO Memory
input   (ls, _, rs) = hGet stdin 1 >>= \ b -> case unpack b of
  [byte] -> return (ls, byte, rs)
  _      -> error "could not read a byte of input"

-- not a brainfuck command; see readProgram, and usage in next 2 commands, below
readProgramErrorMessage :: String
readProgramErrorMessage = "jump instruction blew up because of a bug \
                           \in readProgram: this should be impossible because \
                           \readProgram should only accept well-formed \
                           \programs, i.e. programs with a matching number of \
                           \brackets, with a '[' always preceding a ']'"

-- ([): If the byte at the data pointer (i.e. the focus of the Memory zipper) is
-- zero, then this function jumps the instruction pointer (i.e. the focus of the
-- Program zipper) FORWARD to the command AFTER the matching ']' command.
-- If the byte at the data pointer is nonzero, then this function simply
-- increments the instruction pointer forward to the next command.
loopL :: Memory -> Program -> Program
loopL    (_, x, _) program  = case x of 0 -> jumpPast 0 program
                                        _ -> goRight program
  where
    jumpPast :: Int -> Program ->         Program
    jumpPast    _           (_, _, [] ) = error readProgramErrorMessage
    jumpPast    count  prog@(_, _, r:_) = case (r, count) of
      (']', 0) -> goRight $ goRight prog -- go just past the matching brace
      (']', _) -> jumpPast (count - 1) (goRight prog)
      ('[', _) -> jumpPast (count + 1) (goRight prog)
      _        -> jumpPast count       (goRight prog)

-- (]): If the byte at the data pointer (i.e. the focus of the Memory zipper) is
-- zero, then this function simply increments the instruction pointer (i.e. the
-- focus of the Program zipper) forward to the next command.
-- If the byte at the data pointer is nonzero then instead of moving the
-- instruction pointer forward to the next command, this function jumps the
-- instruction pointer BACK to the command AFTER the matching '[' command.

-- It is possible to implement this behavior as an unconditional jump back to
-- the corresponding left bracket; but then, if the byte at the data pointer is
-- zero, the program will unnecessarily jump twice, which is inefficient.
loopR :: Memory -> Program -> Program
loopR    (_, x, _) program  = case x of 0 -> goRight program
                                        _ -> jumpBack 0 program
  where
    jumpBack :: Int -> Program         -> Program
    jumpBack    _           ([] , _, _) = error readProgramErrorMessage
    jumpBack    count  prog@(l:_, _, _) = case (l, count) of
      ('[', 0) -> prog -- we are already just past the matching brace
      ('[', _) -> jumpBack (count - 1) (goLeft prog)
      (']', _) -> jumpBack (count + 1) (goLeft prog)
      _        -> jumpBack count       (goLeft prog)


-- In the first pattern, the empty program is legal, and results in a no-op.
-- In the second pattern, we add a dummy instruction to ensure we perform the
-- final real instruction. This is necessary because in `interpret` and
-- `compile` below, end of program is signified by an empty list to the right of
-- the focus of the Zipper. But the focus is the current instruction, which must
-- be executed regardless of whether any more instructions follow it.
-- Before that, we check that the input is well-formed by ensuring that there is
-- an equal number of '[' and ']', and that every ']' is preceded by a '['.
readProgram :: String        -> Either String Program
readProgram    ""             = Right (undefined, undefined, "")
readProgram    program@(i:is) = case brackets of
  Left  index   -> Left $ "Illegal program: ']' at character " ++ show index ++
                          " of input program, before any matching '['"
  Right n
    | n < 0     -> error "bug: should be impossible because firstMismatched \
                         \should already catch the case of more ']' than '['"
    | n > 0     -> Left $ "Illegal program: " ++ show n ++ " more '[' than ']'"
    | otherwise -> Right ([], i, is ++ "\0")
  where
    brackets :: Either Int Int
    brackets = maybe (Right $ last runningBracketCount) Left firstMismatched
    
    firstMismatched :: Maybe Int
    firstMismatched = (-1) `elemIndex` runningBracketCount

    runningBracketCount :: [Int]
    runningBracketCount = scanl (flip countBrackets) 0 program

    countBrackets :: Char -> Int -> Int
    countBrackets '[' = (+1)
    countBrackets ']' = subtract 1
    countBrackets _   = id


-- The heart of the interpreter.
interpret :: Memory -> Program            -> IO ()
interpret    _                 (_, _, "" ) = return () -- end legal program
interpret    memory    program@(_, i, _:_) = case i of
  '>' -> interpret (incrementDataPointer memory)      (goRight program)
  '<' -> interpret (decrementDataPointer memory)      (goRight program)
  '+' -> interpret (incrementByte memory)             (goRight program)
  '-' -> interpret (decrementByte memory)             (goRight program)
  '.' -> output memory >>            interpret memory (goRight program)
  ',' -> input memory >>= \newMem -> interpret newMem (goRight program)
  '[' -> interpret memory                             (loopL memory program)
  ']' -> interpret memory                             (loopR memory program)
  _   -> interpret memory                             (goRight program)


initialMemory :: Memory
initialMemory = (repeat 0, 0, repeat 0)

run :: String -> IO ()
run = either putStrLn (interpret initialMemory) . readProgram

main :: IO ()
main = getContents >>= run
