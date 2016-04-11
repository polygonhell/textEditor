module Keys where

data Keys = CursorRight
          | CursorLeft
          | CursorUp
          | CursorDown
          | Alpha Char
          | CarriageReturn
          | UnknownKey

