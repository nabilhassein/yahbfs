% brainfuck interpreter
% nabil hassein
% 11 July 2013

# what is this i don't even

## according to wikipedia:
The brainfuck programming language is an esoteric programming language noted
for its extreme minimalism. It is a Turing tarpit, designed to challenge and
amuse programmers, and was not made to be suitable for practical use.

## according to me:
Lots of fun!

# how extreme is this 'extreme minimalism'?

On a scale of one to ten, it's extreme.

There are eight commands; everything else is ignored as a comment.

Soon I'll provide English descriptions and C translations, lifted from Wikipedia.

# language model
It's a lot like a Turing machine.

The brainfuck language uses a simple machine model consisting of:

- the program (basically a string) and instruction pointer

- an array of at least 30,000 byte cells initialized to zero

- a movable data pointer (initialized to point to the leftmost byte of the array)

- two streams of bytes for input and output

# language spec: initialization
char array[30000 or more];

char *ptr=array;


# language spec: moving the data pointer
```
> increment the data pointer

C: ++ptr;

< decrement the data pointer

C: --ptr;
```

# language spec: manipulating data
```
+ increment the byte at the data pointer

C: ++*ptr;

- decrement the byte at the data pointer

C: --*ptr;
```

# language spec: input/output
```
. output the byte at the data pointer

C: putchar(*ptr);

, accept one byte of input, storing its value in
  the byte at the data pointer

C: *ptr=getchar();
```

# language spec: flow control

```
[ if the byte at the data pointer is zero,
then instead of moving the instruction pointer
forward to the next command, jump it forward
to the command after the matching ']' command

C: while (*ptr) {

] if the byte at the data pointer is nonzero,
then instead of moving the instruction pointer
forward to the next command, jump it back
 to the command after the matching '[' command

C: }
```

# is that really enough to do anything?

- Yes.

# enough talk

> - example 0: enumerating the printable ASCII characters

> - example 1: hello, world

# my implementation

> - I wrote it in Haskell because HASKELL

> - First I wrote functions corresponding to each of the eight commands.
The first six were easy. The last two were a bit harder, but not so bad.

> - The main function of the interpreter
("execute", but perhaps "eval" would have been more traditional)
is basically a switch statement on the eight possible characters of input,
with a no-op on the fall-through.
A real parser is overkill for such a trivial language.

> - The most interesting part of this mini-project was: How do I efficiently
and naturally represent a mutable array in a purely functional language?

# zippers

For this I used a really cool data structure called a *zipper*.
They can be used not only for keeping a "context" or "location" within a list,
but also within more complex structures such as trees.

```
type Zipper a = ([a], a, [a])

goRight :: Zipper a ->     Zipper a
goRight    (ls, x, r:rs) = (x:ls, r, rs)
goRight    (_,  _, []  ) = error "illegal"

goLeft :: Zipper a ->     Zipper a
goLeft    (l:ls, x, rs) = (ls, l, x:rs)
goLeft    ([]  , _, _ ) = error "illegal"
```

# use the source, Luke

https://github.com/nabilhassein/exercises/blob/master/brainfuck/brainfuck.hs

If you read Haskell, the code is very simple.
Comments and whitespace are the only reason it's longer than 100 lines.

If you don't read Haskell, I can walk you through it and
I promise you'll understand -- just ask!


# references
- https://en.wikipedia.org/wiki/Brainfuck

- http://learnyouahaskell.com/zippers

# thanks!

email:   nabil.hassein@gmail.com

twitter: @nabilhassein

github:  nabilhassein
