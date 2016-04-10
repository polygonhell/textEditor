{-# LANGUAGE RecordWildCards #-}
module TextBuffer where

import Data.Sequence as S
import Data.List as L
import Data.Char
import Data.Maybe
import Debug.Trace

data Cursor = Cursor { preferredCol :: Int
                     , line :: Int
                     , col :: Int
                     } deriving (Show)
type Line = String
type BufferContent = Seq Line
 
data Buffer = Buffer { content  :: BufferContent
                     , cursor  :: Cursor 
                     } deriving (Show)



isFirstLine :: Buffer -> Bool
isFirstLine Buffer{..} =  line cursor == 0

isLastLine :: Buffer -> Bool
isLastLine Buffer{..} = line cursor == S.length content - 1

lineLength :: Int -> Buffer -> Int
lineLength line Buffer{..} = L.length (index content line)

curLineLength :: Buffer -> Int
curLineLength b@Buffer{..} = lineLength (line cursor) b

-- Navigation
setCursorColumn :: Int -> Buffer -> Buffer
setCursorColumn pos b@Buffer{..} = buffer where
  buffer = b{cursor = cursor{col = pos', preferredCol = pos'}}
  pos' = min pos (curLineLength b)

startOfLine :: Buffer -> Buffer
startOfLine b@Buffer{..} = b{cursor = cursor{col = 0, preferredCol = 0}}

endOfLine :: Buffer -> Buffer
endOfLine b@Buffer{..} = b{cursor = cursor{col = pos, preferredCol = pos}} where
  pos = curLineLength b

-- Cursor Motion
cursorLeft :: Buffer -> Buffer
cursorLeft b@Buffer{..} = setCursorColumn pos b where 
  Cursor{..} = cursor
  pos = max 0 (col-1)

cursorRight :: Buffer -> Buffer
cursorRight b@Buffer{..} = setCursorColumn pos b where 
  Cursor{..} = cursor
  pos = min (curLineLength b) (col+1)

cursorUp :: Buffer -> Buffer
cursorUp b@Buffer{..} = b{cursor = cursor'} where 
  Cursor{..} = cursor
  newLine = max 0 (line-1)
  newCol = min (lineLength newLine b) preferredCol
  cursor' = cursor{ line = newLine, col = newCol}

cursorDown :: Buffer -> Buffer
cursorDown b@Buffer{..} = b{cursor = cursor'} where 
  Cursor{..} = cursor
  newLine = min (S.length content-1) (line+1)
  newCol = min (lineLength newLine b) preferredCol
  cursor' = cursor{line = newLine, col = newCol}


