yahbfs is yet another haskell brainfuck system.

It currently includes a simple interpreter and a non-optimizing compiler which
produces C code, which can be given as input to a C compiler like gcc.

Future goals include a more efficient interpreter, a more sophisticated
compilation system, and a visualizer/debugger.

If you are unfamiliar with the brainfuck programming language, see
[Wikipedia](https://en.wikipedia.org/wiki/Brainfuck) for a clear, concise and
exhaustive explanation.

I've striven to make the source code readable (and it is commented extensively).
If anything is confusing, or if you have a suggestion (or even better, code!) to
improve yahbfs, please tell me.

# Notes
If you enter multiple bytes when the program is expecting just one,
the other bytes remain in the buffer, and will be passed to future calls to ','.
This is intended behavior, but may be surprising.

Byte arithmetic is modulo 256; overflow is not an exception.

In fact, a program without syntax errors (i.e. mismatched braces) should never
encounter any runtime error. Syntax errors are caught at compile-time; the user
receives a descriptive error message and the faulty program is rejected.

# License
yahbfs is licensed under the WTFPL, a permissive free software license.
See LICENSE.
