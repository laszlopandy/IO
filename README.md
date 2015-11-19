Elm Console [![Build Status](https://travis-ci.org/laszlopandy/elm-console.png?branch=master)](https://travis-ci.org/laszlopandy/elm-console)
=========

This library allows reading and writing from the console in Node.
It is a replacement for `maxsnew/IO`, which is no longer updated.

Example
-------
An elm Program:
```elm
module Main where

import Console exposing (IO, (>>>), (>>=), forever, getLine, pure, exit, putStrLn)
import Task

import List
import Maybe
import String

echo : IO ()
echo = forever (getLine >>= putStrLn)

loop : IO ()
loop = getLine >>= \s ->
       if s == "exit"
       then pure ()
       else putStrLn s >>> loop
       
hello : IO ()
hello = putStrLn "Hello, Console!" >>>
        putStrLn "I'll echo your input until you say \"exit\":" >>>
        loop >>>
        putStrLn "That's all, folks!" >>>
        exit 0

port runner : Signal (Task.Task x ())
port runner = Console.run hello
```

link in some javascript and then run:
```
$ elm-make --yes test/Test.elm --output raw-test.js
...
$ ./elm-io.sh raw-test.js test.js
$ node test.js
Hello, Console!
I'll echo your input:
hooray
hooray
That's all, folks!
```

Design and Implementation
-------------------------
The implementation is based on the
[IOSpec](http://hackage.haskell.org/package/IOSpec) haskell library.
