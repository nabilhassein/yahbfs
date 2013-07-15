module Brainfuck where
-- see https://en.wikipedia.org/wiki/Brainfuck

import Control.Monad.Error () -- instance Monad Either String
import Data.Char           (ord, chr)
import Data.List           (elemIndex)
import Data.Word           (Word8)
import System.Environment  (getArgs)


-- see http://learnyouahaskell.com/zippers
type Zipper a = ([a], a, [a])

-- from Wikipedia:
-- "The brainfuck language uses a simple machine model consisting of the program
-- and instruction pointer, as well as an array of at least 30,000 byte cells
-- initialized to zero; a movable data pointer (initialized to point to the
-- leftmost byte of the array); and two streams of bytes for input and output
-- (most often connected to a keyboard and a monitor respectively, and using
-- the ASCII character encoding)."

-- a Zipper is a natural model for brainfuck's instruction and data pointers:
-- the current instruction or byte respectively is the focus
type Program  = Zipper Char
type Memory   = Zipper Word8

goRight :: Zipper a ->     Zipper a
goRight    (ls, x, r:rs) = (x:ls, r, rs)
goRight    (_ , _, []  ) = error "bug: this should never occur because in the \
                                  \Program Zipper, when we reach this case, \
                                  \we end execution; and in the Memory Zipper, \
                                  \memory is infinite. So this is impossible."

-- below, readProgram guarantees that a Right Program is well-formed, i.e. that
-- there are an equal number of '[' and ']', with a '[' always preceding a ']'.
-- As the only control flow in a brainfuck program is to increment the
-- instruction pointer or conditionally jump between brackets, it is impossible
-- for a well-formed Program to attempt to go left past the beginning of the
-- Program. However, a well-formed Program might illegally attempt to move the
-- data pointer further left than the start of memory. We handle this exception.
goLeft :: Zipper a ->     Either String (Zipper a)
goLeft    (l:ls, x, rs) = Right (ls, l, x:rs)
goLeft    ([]  , _, _ ) = Left "illegal: cannot decrement data pointer when \
                                \it is pointing at cell 0 of memory"


-- below follow the eight commands of the brainfuck language
-- (>): increment the data pointer (to point to the next cell to the right)
-- always succeeds because there is an infinite space of memory to the right
incrementDataPointer :: Memory -> Memory
incrementDataPointer = goRight

-- (<): decrement the data pointer (to point to the next cell to the left)
-- Left if program decrements data pointer when the data pointer (i.e. the
-- focus of the data pointer) is focused on cell 0; Right otherwise
decrementDataPointer :: Memory -> Either String Memory
decrementDataPointer = goLeft

-- (+): increment the byte at the data pointer
incrementByte :: Memory ->     Memory
incrementByte    (ls, b, rs) = (ls, b+1, rs)

-- (-): decrement the byte at the data pointer
decrementByte :: Memory ->     Memory
decrementByte    (ls, b, rs) = (ls, b-1, rs)

-- (.): output the byte at the data pointer
output :: Memory ->   IO ()
output    (_, b, _) = putChar . chr . fromEnum $ b

-- (,): accept 1 byte of input; store its value in the byte at the data pointer
input :: Memory ->    IO Memory
input   (ls, _, rs) = getChar >>= \ c -> return (ls, (toEnum . ord) c, rs)

-- not a brainfuck command; see usage in next two commands below
readProgramErrorMessage :: String
readProgramErrorMessage = "jump instruction blew up because of a bug \
                           \in readProgram: this should be impossible because \
                           \readProgram should only accept well-formed \
                           \programs, i.e. programs with a matching number of \
                           \brackets, with a '[' always preceding a ']'"

-- ([): If the byte at the data pointer (i.e. the focus of the Memory zipper) is
-- zero, then this function jumps the instruction pointer (i.e. the focus of the
-- Program zipper) FORWARD to the command after the matching ']' command.
-- If the byte at the data pointer is nonzero, then this function simply
-- increments the instruction pointer forward to the command.
jumpIfZero :: Memory -> Program -> Program
jumpIfZero    (_, x, _) program  = case x of 0 -> jumpPast ']' program
                                             _ -> goRight program
  where jumpPast :: Char -> Program ->         Program
        jumpPast    _            (_, _, [] ) = error readProgramErrorMessage
        jumpPast    c       prog@(_, _, r:_) = if r == c
                                               then goRight $ goRight prog
                                               else jumpPast c $ goRight prog

-- (]): If the byte at the data pointer (i.e. the focus of the Memory zipper) is
-- zero, then this function simply increments the instruction pointer (i.e. the
-- focus of the Program zipper).
-- If the byte at the data pointer is nonzero then instead of moving the
-- instruction pointer forward to the next command, this function jumps the
-- instruction pointer BACK to the command after the matching '[' command.

-- it is possible to implement this behavior as an unconditional jump back to
-- the corresponding left bracket; but then, if the byte at the data pointer is
-- zero, the program will unnecessarily jump twice, which is inefficient
stepIfZero :: Memory -> Program -> Program
stepIfZero    (_, x, _) program  = case x of 0 -> goRight program
                                             _ -> jumpBack '[' program
  where jumpBack :: Char -> Program ->         Program
        jumpBack    _         ([] , _, _) = error readProgramErrorMessage
        jumpBack    c       p@(l:_, _, _) =
          if l == c
          then p
          else either (error readProgramErrorMessage) (jumpBack c) (goLeft p)


-- Done implementing brainfuck commands. This is the heart of the interpreter.
execute :: Memory -> Program ->         IO (Maybe String)
execute    _              (_, _, [] ) = return Nothing -- end legal program
execute    mem       prog@(_, i, _:_) =
  -- At every step, try to update the memory. If that fails, pass the exception
  -- to the user. If it succeeds, then update the program and continue execution
  -- with the updated memory and program. (Note that, as explained in comments
  -- above, readProgram guarantees that our programs are well-formed, and
  -- updating a well-formed program cannot fail.)
  -- Repeat until we encounter an exception or reach the end of a legal program.
  let step :: (Memory  -> Either String Memory ) -> Memory  ->
              (Program -> Program)               -> Program -> IO (Maybe String)
      step updateMemory m updateProgram p =
        either (return . Just) (flip execute (updateProgram p)) (updateMemory m)
  in case i of
    '>' -> step (return . incrementDataPointer) mem    goRight          prog
    '<' -> step decrementDataPointer            mem    goRight          prog
    '+' -> step (return . incrementByte)        mem    goRight          prog
    '-' -> step (return . decrementByte)        mem    goRight          prog
    '.' -> output mem >>
           step return                          mem    goRight          prog
    ',' -> input mem >>= \ newMem ->
           step return                          newMem goRight          prog
    '[' -> step return                          mem    (jumpIfZero mem) prog
    ']' -> step return                          mem    (stepIfZero mem) prog
    _   -> step return                          mem    goRight          prog


-- In the first pattern, the empty program is legal, and results in a no-op.
-- In the second pattern, we add a dummy pattern to ensure we execute the final
-- real instruction. This is necessary because in `execute` above,
-- end of program is signified by an empty list to the right of the focus
-- of the Zipper. But the focus is the current instruction, which must be
-- executed even if no instructions follow it.
-- But first we check that the input is well-formed by ensuring that there is an
-- equal number of '[' and ']', and that every ']' is preceded by a '['.
readProgram :: String ->     Either String Program
readProgram []             = Right (undefined, undefined, [])
readProgram program@(i:is) = case brackets of
  Left  index    -> Left $ "Illegal program: ']' at character " ++ show index ++
                           " of input, before any matching '['"
  Right n
    | n < 0     -> error "bug: should be impossible because firstMismatched \
                         \should already catch the case of more ']' than '['"
    | n > 0     -> Left $ "Illegal program: " ++ show n ++ " more '[' than ']'"
    | otherwise -> Right ([], i, is ++ "\0")

  where
    brackets :: Either Int Int
    brackets = let runningBracketCount :: [Int]
                   runningBracketCount =  scanl (flip countBrackets) 0 program
                   firstMismatched     :: Maybe Int
                   firstMismatched     =  (-1) `elemIndex` runningBracketCount
               in  maybe (Right $ last runningBracketCount) Left firstMismatched
    countBrackets :: Char -> Int -> Int
    countBrackets c = case c of
      '[' -> (+ 1)
      ']' -> subtract 1
      _   -> id

-- brainfuck's specification mandates "at least" 30000 bytes of memory.
-- We implement an infinite memory space, which is trivial thanks to laziness.
initialMemory :: Memory
initialMemory = ([], 0, repeat 0)

-- If program is well-formed, execute it with the initial memory of all zeroes.
-- If program is malformed, display the problem to the user, and exit.
run :: String -> IO (Maybe String)
run = either (return . Just) (execute initialMemory) . readProgram


main :: IO ()
main = getArgs >>= \ args -> case args of
  []         -> putStrLn "Usage: runhaskell brainfuck.hs [brainfuck source file]"
  filename:_ -> readFile filename >>= run >>= maybe (return ()) putStrLn
