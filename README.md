# `list-transformer` v1.1.1

This library provides a "`ListT` done right implementation" that obeys the
`Monad` laws.

Here is an example of how you build and consume `ListT` values:

```haskell
import List.Transformer

import qualified System.IO

stdin :: ListT IO String
stdin = ListT (do
    eof <- System.IO.isEOF
    if eof
        then return Nil
        else do
            string <- getLine
            return (Cons string stdin) )

stdout :: ListT IO String -> IO ()
stdout strings = do
    s <- next strings
    case s of
        Nil                  -> return ()
        Cons string strings' -> do
            putStrLn string
            stdout strings'

main :: IO ()
main = stdout stdin
```

... and here's an example use of this library to print every natural number
using a style resembling list comprehensions:

```haskell
import List.Transformer

main :: IO ()
main = runListT (do
    n <- select [0..]
    liftIO (print n) )
```

To learn more, read the
[tutorial](http://hackage.haskell.org/package/list-transformer/docs/List-Transformer.html).

## Quick start

Install [the `stack` tool](http://haskellstack.org/) and then run:

```bash
$ stack setup
$ stack ghci list-transformer
Prelude> import List.Transformer
Prelude List.Transformer> runListT (do n <- select [0..]; liftIO (print n) )
0
1
2
3
...
```

## Development Status

[![Build Status](https://travis-ci.org/Gabriel439/Haskell-List-Transformer-Library.png)](https://travis-ci.org/Gabriel439/Haskell-List-Transformer-Library)

This library is pretty small so I don't expect there to be breaking changes
since there's not that much that could possibly change.

## License (BSD 3-clause)

Copyright (c) 2016 Gabriel Gonzalez
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice, this
  list of conditions and the following disclaimer in the documentation and/or
  other materials provided with the distribution.

* Neither the name of Gabriel Gonzalez nor the names of other contributors may
  be used to endorse or promote products derived from this software without
  specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
