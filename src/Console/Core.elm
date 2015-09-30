module Console.Core where

import String

-- IO Actions
{-| Print a character to stdout -}
putChar : Char -> IO ()
putChar c = Impure (PutS (String.cons c "") (\_ -> Pure ()))

{-| Read a character from stdin -}
getChar : IO Char
getChar = Impure (GetC Pure)

{-| Exit the program with the given exit code. -}
exit : Int -> IO ()
exit = Impure << Exit

{-| Print a string to stdout. -}
putStr : String -> IO ()
putStr s = Impure (PutS s (\_ -> Pure ()))

{-| Print a string to stdout, followed by a newline. -}
putStrLn : String -> IO ()
putStrLn s = putStr s >>> putChar '\n'

{-| Read characters from stdin until one matches the given character. -}
readUntil : Char -> IO String
readUntil end = let go s = getChar >>= \c ->
                           if c == end
                           then pure s
                           else go (String.append s (String.cons c ""))
                in go ""

{-| Write content to a file -}
writeFile : { file : String, content : String } -> IO ()
writeFile obj = Impure (WriteF obj (\_ -> Pure ()))

{-| Read a line from stdin -}
getLine : IO String
getLine = readUntil '\n'


{-| Read all stdin into a single string. -}
getContents : IO String
getContents = readUntil '\0'

-- | IO Combinators

{-| Apply a pure function to an IO value -}
map : (a -> b) -> IO a -> IO b
map f io = case io of
  Pure   a   -> Pure (f a)
  Impure iof -> Impure (mapF (map f) iof)

{-| Alternative interface to forEach  -}
mapIO : (a -> IO ()) -> List a -> IO ()
mapIO f xs = List.foldr (seq << f) (pure ()) xs

{-| Run an IO computation for each element of a list -}
forEach : List a -> (a -> IO ()) -> IO ()
forEach xs f = mapIO f xs

{-| Use a pure value where an IO computation is expected. -}
pure : a -> IO a
pure = Pure

{-| Apply an IO function to an IO value -}
apply : IO (a -> b) -> IO a -> IO b
apply iof iom = iof >>= \f ->
                iom >>= \m ->
                pure (f m)

{-| Convenient operator for apply, similar to ~ in the Signal module -}
(<*>) : IO (a -> b) -> IO a -> IO b
(<*>) = apply

{-| Chain together IO computations -}
andThen : IO a -> (a -> IO b) -> IO b
andThen io f = case io of
  Pure x     -> f x
  Impure iof -> Impure (mapF (flip andThen f) iof)

{-| Operator version of andThen -}
(>>=) : IO a -> (a -> IO b) -> IO b
(>>=) = andThen

{-| Run one computation and then another, ignoring the first's output -}
seq : IO a -> IO b -> IO b
seq x y = x >>= \_ -> y

{-| Operator version of seq -}
(>>>) : IO a -> IO b -> IO b
(>>>) = seq

-- Has to be >>= not >>> because of strictness!
{-| Run the same computation over and over again forever. -}
forever : IO a -> IO ()
forever m = m >>= (\_ -> forever m)

{-| IMPLEMENTATION DETAILS!!! -}
type IOF a = PutS String (() -> a)    -- ^ the a is the next computation
           | GetC (Char -> a) -- ^ the (Char -> a) is the continuation
           | Exit Int
           | WriteF { file : String, content : String} (() -> a)

-- The type of I/O computations.
{-| An `IO a` is a computation that does some I/O and eventually
    returns an `a` -}
type IO a = Pure a
          | Impure (IOF (IO a))

mapF : (a -> b) -> IOF a -> IOF b
mapF f iof = case iof of
  PutS p k -> PutS p (f << k)
  GetC k   -> GetC (f << k)
  Exit n   -> Exit n
  WriteF obj k -> WriteF obj (f << k)
