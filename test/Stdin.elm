module Main where

import Console exposing ((>>=), putStr, getContents)
import String
import Task


port runner : Signal (Task.Task x ())
port runner = Console.run (getContents >>= putStr)
