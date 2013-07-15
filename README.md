yahbfs is yet another haskell brainfuck system.

It strives for elegance of implementation over efficiency.
I hope that it will eventually be useful as a teaching aid,
after I have learned a little bit more about interpreters and compilers myself.

Note that if you enter multiple bytes when the program is expecting just one,
the other bytes remain in the buffer, and will be passed to future calls to ','.
This is intended behavior, but may be surprising.

Also note that byte arithmetic is mod 256; byte overflow is not an exception.


yahbfs is licensed under the WTFPL, a permissive free software license.
