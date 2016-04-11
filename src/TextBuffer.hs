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
cursorLeft b@Buffer{..} | col == 0 && line /= 0 = b' where 
  Cursor{..} = cursor
  b' = endOfLine $ b {cursor = cursor{line = line - 1}}
cursorLeft b@Buffer{..} | col == 0 = b where 
  Cursor{..} = cursor
cursorLeft b@Buffer{..} = setCursorColumn pos b where 
  Cursor{..} = cursor
  pos = col-1

cursorRight :: Buffer -> Buffer
cursorRight b@Buffer{..} | (col == curLineLength b) && line /= (S.length content - 1) = b' where
  Cursor{..} = cursor
  b' = startOfLine $ b {cursor = cursor{line = line + 1}}
cursorRight b@Buffer{..} | col == curLineLength b = b where 
  Cursor{..} = cursor
cursorRight b@Buffer{..} = setCursorColumn pos b where 
  Cursor{..} = cursor
  pos = col+1

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

insertCharacter :: Char -> Buffer -> Buffer
insertCharacter c b@Buffer{..} = cursorRight b{content = content'} where
  Cursor{..} = cursor
  (l, r) = L.splitAt col $ index content line
  ln' = l ++ (c:r)
  content' = update line ln' content

breakLine :: Buffer -> Buffer
breakLine b@Buffer{..} = b{content = content', cursor = cursor'} where
  Cursor{..} = cursor
  (l, r) = L.splitAt col ln
  (s, e) = S.splitAt line content
  ln :< eResid = viewl e
  content' = (s |> l |> r) >< eResid
  cursor' = cursor{line = line + 1, col = 0, preferredCol = 0}

unbreakLine :: Buffer -> Buffer
unbreakLine b@Buffer{..} = b{content = content', cursor = cursor'} where
  Cursor{..} = cursor
  (s, e) = S.splitAt line content
  (s' :> ln1) = viewr s
  (ln2 :< e') = viewl e
  content' = (s' |> (ln1 ++ ln2)) >< e'
  cursor' = cursor {col = L.length ln1, line = line - 1, preferredCol = L.length ln1}

  
deleteCharacter :: Buffer -> Buffer
deleteCharacter b@Buffer{..} | col cursor == 0 && not (isFirstLine b) = unbreakLine b
deleteCharacter b@Buffer{..} | col cursor == 0 = b
deleteCharacter b@Buffer{..} = cursorLeft b{ content = content' } where
  Cursor{..} = cursor
  (l, r) = L.splitAt col $ index content line
  ln' = L.take (L.length l - 1) l ++ r
  content' = update line ln' content


