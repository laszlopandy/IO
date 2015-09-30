module Console.NativeTypes where

type IRequest
  = Init
  | Get
  | Put String
  | WriteFile { file : String, content : String }
  | Exit Int

type IResponse
  = Empty
  | Data String
  | EOF
