{-# LANGUAGE RecordWildCards #-}
module TextBuffer where

import Data.Sequence as S
import Data.Text as T
import Data.Foldable as F
import Data.Char
import Data.Maybe
import Debug.Trace
import Text.Printf
import Prelude as P

data Cursor = Cursor { preferredCol :: Int
                     , line :: Int
                     , col :: Int
                     } deriving (Show)

data RegionStyle = Normal 
                 | Selected
                 | Comment 
                 | Number
                 | StringStyle
                 | RSNamed String deriving (Show, Eq, Ord)

data Region = Region { startOffset :: Int
                     , endOffset :: Int
                     , styles :: [RegionStyle]
                     } deriving (Show)

type Selection = Region

type Line = Text
type BufferContent = Seq Line
 
data Buffer = Buffer { content  :: BufferContent
                     , cursor  :: Cursor 
                     , selection :: [Selection]
                     , initialSelectionOffset :: Int
                     , regions :: [Region]
                     , contentChanged :: Bool
                     } deriving (Show)



isFirstLine :: Buffer -> Bool
isFirstLine Buffer{..} =  line cursor == 0

isLastLine :: Buffer -> Bool
isLastLine Buffer{..} = line cursor == S.length content - 1

lineLength :: Int -> Buffer -> Int
lineLength line Buffer{..} | line >= S.length content = 0
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
insertCharacter c b@Buffer{..} = dirty $ updateRegions cpos 1 (cursorRight b{content = content'}) where
  Cursor{..} = cursor
  cpos = posToOffset b line col
  (l, r) = T.splitAt col $ S.index content line
  ln' = l `append` (c `cons` r)
  content' = update line ln' content

breakLine :: Buffer -> Buffer
breakLine b@Buffer{..} = dirty $ updateRegions cpos 1 b{content = content', cursor = cursor'} where
  Cursor{..} = cursor
  cpos = posToOffset b line col
  (l, r) = T.splitAt col ln
  (s, e) = S.splitAt line content
  ln :< eResid = viewl e
  content' = (s |> l |> r) >< eResid
  cursor' = cursor{line = line + 1, col = 0, preferredCol = 0}

unbreakLine :: Buffer -> Buffer
unbreakLine b@Buffer{..} = dirty $ updateRegions (cpos-1) (-1) b{content = content', cursor = cursor'} where
  Cursor{..} = cursor
  cpos = posToOffset b line col
  (s, e) = S.splitAt line content
  (s' :> ln1) = viewr s
  (ln2 :< e') = viewl e
  content' = (s' |> (ln1 `append` ln2)) >< e'
  cursor' = cursor {col = T.length ln1, line = line - 1, preferredCol = T.length ln1}

  
deleteCharacter :: Buffer -> Buffer
deleteCharacter b@Buffer{..} | col cursor == 0 && not (isFirstLine b) = unbreakLine b
deleteCharacter b@Buffer{..} | col cursor == 0 = b
deleteCharacter b@Buffer{..} = dirty $ updateRegions (cpos-1) (-1) (cursorLeft b{ content = content' }) where
  Cursor{..} = cursor
  cpos = posToOffset b line col
  (l, r) = T.splitAt col $ S.index content line
  ln' = T.take (T.length l - 1) l `append` r
  content' = update line ln' content


clearSelection :: Buffer -> Buffer
clearSelection b@Buffer{..} = b{selection = []}

startSelection :: Buffer -> Buffer
startSelection b@Buffer{..} = b{selection = [Region cpos cpos [Selected]], initialSelectionOffset = cpos} where
  Cursor{..} = cursor
  cpos = posToOffset b line col


updateSelection b@Buffer{..} | F.null selection = b  
updateSelection b@Buffer{..} = b{selection = selection'} where  
  Cursor{..} = cursor 
  cpos = posToOffset b line col
  selection' = P.map fn selection
  fn r@Region{..} = r{startOffset = o, endOffset = p, styles = s} where
    (o, p, s) = case cpos of
      _ | cpos == startOffset -> (initialSelectionOffset, cpos, [Normal])
      _ | cpos > startOffset  -> (initialSelectionOffset, cpos-1, [Selected])  
      -- Special case for reverse selection of EOL Character -- reverse selections are inclusive
      _ | cpos == startOffset - 1 && col == lineLength line b - 1 -> (initialSelectionOffset-1, cpos, [Normal])
      _                       -> (initialSelectionOffset- 1 , cpos, [Selected])
     

deleteSelection b@Buffer{..} | F.null selection = b 
deleteSelection b@Buffer{..} | styles (P.head selection) == [Normal] = b{selection = []}
deleteSelection b@Buffer{..} = dirty $ updateRegions minPos (minPos - maxPos - 1) b' where
  [s@Region{..}] = selection  
  Cursor{..} = cursor
  minPos = min startOffset endOffset
  maxPos = max startOffset endOffset
  (minLine, minCol) = offsetToPos b minPos 
  (maxLine, maxCol) = offsetToPos b maxPos
  (before, rem) = S.splitAt minLine content
  (selLine, after) = S.splitAt (maxLine - minLine + 1) rem
  (fl :< _) = viewl selLine
  (_ :> ll) = viewr selLine
  lineOut = T.take minCol fl `T.append` T.drop (maxCol+1) ll
  cursor' = cursor{line = minLine, col = minCol, preferredCol = minCol}
  b' = b{selection = [], content = (before |> lineOut) >< after, cursor = cursor'}

updateRegion :: Int -> Int -> Region -> [Region]
updateRegion offset added r@Region{..} = r' where
  rStart = min startOffset endOffset
  rEnd = max startOffset endOffset
  r' = case offset of 
    _ | offset > rEnd -> [r]
    _ | offset <= rStart && added >= 0 -> [r{startOffset = rStart + added, endOffset = rEnd + added}]
    _ | added >= 0 -> [r{startOffset = rStart, endOffset = rEnd + added}]
    _ | offset - added <= rStart -> [r{startOffset = rStart + added, endOffset = rEnd + added}]
    _ | offset <= rStart && offset - added > rEnd -> []
    -- Must cover part of it
    _ | offset < rStart -> [r{startOffset = offset, endOffset = rEnd + added}]
    _ | offset - added -1  <= rEnd -> [r{startOffset = rStart, endOffset = rEnd + added}]
    _ -> [r{startOffset = rStart + added, endOffset = offset}]
    

updateRegions :: Int -> Int -> Buffer -> Buffer
updateRegions offset added b@Buffer{..} = b{regions = r'} where
  r' = foldMap (updateRegion offset added) regions


dirty :: Buffer -> Buffer
dirty b@Buffer{..}  = b{contentChanged = True}
