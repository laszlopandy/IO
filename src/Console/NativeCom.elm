module Console.NativeCom where

import Task exposing (Task)

import Console.NativeTypes exposing (IResponse, IRequest)

import Native.Console.NativeCom


sendRequests : Signal (List IRequest) -> Signal (Task x ())
sendRequests requests =
    Signal.map sendRequestBatch requests

sendRequestBatch : List IRequest -> Task x ()
sendRequestBatch requests =
    Native.Console.NativeCom.sendRequestBatch requests

responses : Signal IResponse
responses =
    Native.Console.NativeCom.responses
