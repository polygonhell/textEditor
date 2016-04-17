module Keys where

data Keys = CursorRight
          | CursorLeft
          | CursorUp
          | CursorDown
          | Alpha Char
          | CarriageReturn
          | Backspace
          | Home
          | End
          | Ctrl Char
          | UnknownKey

