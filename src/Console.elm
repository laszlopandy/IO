module Console (putChar, putStr, putStrLn, getChar, getLine, readUntil, writeFile,
           exit, map, mapIO, forEach, pure, apply, (<*>), andThen, (>>=),
           seq, (>>>), forever, IO, run, getContents) where

{-|

A library for writing terminal-based scripts in elm.  The IO type
provides an interface for constructing "computations" that may perform
IO effects. Something with type `IO a` is a lazy computation that when
run will produce an `a`, possibly IO side effects. See Console.Runner for
how to run such a computation.

# IO Type
@docs IO, run

# Stdout
@docs putChar, putStr, putStrLn

# Stdin
@docs getChar, getLine, getContents, readUntil

# File Operations
@docs writeFile

# Exit code
@docs exit

# Plumbing
@docs map, mapIO, forEach, pure, apply,
      (<*>), andThen, (>>=), seq, (>>>), forever
-}

import Console.Core as Core
import Console.Runner as Runner
import Task exposing (Task)

-- IO Actions
{-| Print a character to stdout -}
putChar : Char -> Core.IO ()
putChar = Core.putChar

{-| Read a character from stdin -}
getChar : Core.IO Char
getChar = Core.getChar

{-| Exit the program with the given exit code. -}
exit : Int -> Core.IO ()
exit = Core.exit

{-| Print a string to stdout. -}
putStr : String -> Core.IO ()
putStr = Core.putStr

{-| Print a string to stdout, followed by a newline. -}
putStrLn : String -> Core.IO ()
putStrLn = Core.putStrLn

{-| Read characters from stdin until one matches the given character. -}
readUntil : Char -> Core.IO String
readUntil = Core.readUntil

{-| Write content to a file -}
writeFile : { file : String, content : String } -> Core.IO ()
writeFile = Core.writeFile

{-| Read a line from stdin -}
getLine : Core.IO String
getLine = Core.getLine

{-| Read from stdin until EOF -}
getContents : Core.IO String
getContents = Core.getContents

{-| Apply a pure function to an IO value -}
map : (a -> b) -> Core.IO a -> Core.IO b
map = Core.map

{-| Alternative interface to forEach  -}
mapIO : (a -> Core.IO ()) -> List a -> Core.IO ()
mapIO = Core.mapIO

{-| Run an IO computation for each element of a list -}
forEach : List a -> (a -> Core.IO ()) -> Core.IO ()
forEach = Core.forEach

{-| Use a pure value where an IO computation is expected. -}
pure : a -> Core.IO a
pure = Core.pure

{-| Apply an IO function to an IO value -}
apply : Core.IO (a -> b) -> Core.IO a -> Core.IO b
apply = Core.apply

{-| Convenient operator for apply, similar to ~ in the Signal module -}
(<*>) : Core.IO (a -> b) -> Core.IO a -> Core.IO b
(<*>) = Core.apply

{-| Chain together IO computations -}
andThen : Core.IO a -> (a -> Core.IO b) -> Core.IO b
andThen = Core.andThen

{-| Operator version of andThen -}
(>>=) : Core.IO a -> (a -> Core.IO b) -> Core.IO b
(>>=) = Core.andThen

{-| Run one computation and then another, ignoring the first's output -}
seq : Core.IO a -> Core.IO b -> Core.IO b
seq = Core.seq

{-| Operator version of seq -}
(>>>) : Core.IO a -> Core.IO b -> Core.IO b
(>>>) = Core.seq

-- Has to be >>= not >>> because of strictness!
{-| Run the same computation over and over again forever. -}
forever : Core.IO a -> Core.IO ()
forever = Core.forever

-- The type of I/O computations.
{-| An `IO a` is a computation that does some I/O and eventually
    returns an `a` -}
type alias IO a = Core.IO a

{-| Run an IO computation as a Task -}
run : Core.IO () -> Signal (Task x ())
run = Runner.run
