{-# LANGUAGE RecordWildCards #-}
module TextBuffer where

import Data.Sequence as S
import Data.Text as T
import Data.Foldable as F
import Data.Char
import Data.Maybe
import Debug.Trace
import Text.Printf


data Cursor = Cursor { preferredCol :: Int
                     , line :: Int
                     , col :: Int
                     } deriving (Show)

data RegionStyle = Selected
                    | Comment deriving (Show)

data Region = Region { startOffset :: Int
                     , endOffset :: Int
                     , style :: RegionStyle
                     , test :: (Int, Int)
                     , test2 :: (Int, Int)
                     } deriving (Show)

type Selection = Region

type Line = Text
type BufferContent = Seq Line
 
data Buffer = Buffer { content  :: BufferContent
                     , cursor  :: Cursor 
                     , selection :: [Selection]
                     } deriving (Show)



isFirstLine :: Buffer -> Bool
isFirstLine Buffer{..} =  line cursor == 0

isLastLine :: Buffer -> Bool
isLastLine Buffer{..} = line cursor == S.length content - 1

lineLength :: Int -> Buffer -> Int
lineLength line Buffer{..} = T.length (S.index content line)

curLineLength :: Buffer -> Int
curLineLength b@Buffer{..} = lineLength (line cursor) b

offsetToPos :: Buffer -> Int -> (Int, Int)
offsetToPos b offset | offset <= lineLength 0 b = (0, offset)
offsetToPos b@Buffer{..} offset = out  where 
  length = lineLength 0 b
  (line, off) = offsetToPos b{content = S.drop 1 content} (offset - (length + 1))
  out = (line + 1, off)



posToOffset :: Buffer -> Int -> Int -> Int
posToOffset b@Buffer{..} line col = lineOffset + col where
  bSlice = S.take line content
  lineOffset = F.foldl fn 0 bSlice
  fn x y = x + T.length y + 1

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
  (l, r) = T.splitAt col $ S.index content line
  ln' = l `append` (c `cons` r)
  content' = update line ln' content

breakLine :: Buffer -> Buffer
breakLine b@Buffer{..} = b{content = content', cursor = cursor'} where
  Cursor{..} = cursor
  (l, r) = T.splitAt col ln
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
  content' = (s' |> (ln1 `append` ln2)) >< e'
  cursor' = cursor {col = T.length ln1, line = line - 1, preferredCol = T.length ln1}

  
deleteCharacter :: Buffer -> Buffer
deleteCharacter b@Buffer{..} | col cursor == 0 && not (isFirstLine b) = unbreakLine b
deleteCharacter b@Buffer{..} | col cursor == 0 = b
deleteCharacter b@Buffer{..} = cursorLeft b{ content = content' } where
  Cursor{..} = cursor
  (l, r) = T.splitAt col $ S.index content line
  ln' = T.take (T.length l - 1) l `append` r
  content' = update line ln' content


