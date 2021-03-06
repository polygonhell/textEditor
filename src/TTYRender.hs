{-# LANGUAGE RecordWildCards #-}
module TTYRender where

import Control.Monad
import Data.Char
import Data.Foldable
import Data.Sequence as S
import Data.List as L
import Data.Map as M
import Data.Maybe as M
import Data.Text as T
import Debug.Trace
import System.IO
import System.Posix.IO (fdRead, stdInput)
import System.Posix.Terminal
import Text.Printf
import Prelude as P

import TextBuffer
import View
import Keys

-- TODO this needs state for the following
-- TODO only redraw changed lines

styleMapping :: Map String String
styleMapping = M.fromList [("keyword", fgColor 208), ("moduleName", fgColor 221), ("identifier", fgColor 47), ("infixOp", fgColor 47), ("selected", bgColor 12), ("comment", fgColor 242), ("number", fgColor 12), ("string", fgColor 45)]
defaultPrefix :: String
defaultPrefix = bgColor 220


lastPosOnScreen :: ViewState -> Buffer -> Int
lastPosOnScreen ViewState{..} b@Buffer{..} = lastLineEnd where
  endLine = top + height-1
  lastLineStart = posToOffset b endLine 0
  lastLineEnd = lastLineStart + lineLength endLine b 

getSortedRegions :: Int -> ViewState -> Buffer -> [Region] -> [Region]
getSortedRegions pos v b [] | pos < lastPosOnScreen v b = [Region pos (lastPosOnScreen v b) []]
getSortedRegions _ _ _ [] = []
getSortedRegions pos v b _ | pos > lastPosOnScreen v b = []
getSortedRegions pos v b (r@Region{..}:rs)  | pos < startOffset = rs' where
  maxPos = lastPosOnScreen v b
  rEnd = min maxPos (startOffset-1)
  rs' = Region pos rEnd [] : getSortedRegions startOffset v b (r:rs)
getSortedRegions pos v b (Region{..}:rs)  | pos >= startOffset = rs' where
  maxPos = lastPosOnScreen v b
  rEnd = min maxPos endOffset
  rs' = Region pos rEnd styles : getSortedRegions (endOffset+1) v b rs
getSortedRegions  _ _ _ _ = error "Uncaught case"

-- Assumes regions are sorted and offsets are ordered
mergeSortedRegions :: [Region] -> [Region] -> [Region] 
mergeSortedRegions [] r2s = r2s
mergeSortedRegions r1s [] = r1s
mergeSortedRegions (r1:r1s) (r2:r2s) = rOut where
  rOut = case r1 of
    -- None overlapping cases
    _ | endOffset r1 < startOffset r2 -> r1 : mergeSortedRegions r1s (r2:r2s)
    _ | endOffset r2 < startOffset r1 -> r2 : mergeSortedRegions (r1:r1s) r2s
    -- Overlap with single start region
    _ | startOffset r1 < startOffset r2 -> r' : mergeSortedRegions r1s' (r2:r2s) where
      r' = Region (startOffset r1) (startOffset r2 - 1) (styles r1)
      r1s' = Region (startOffset r2) (endOffset r1) (styles r1) : r1s
    _ | startOffset r2 < startOffset r1 -> r' : mergeSortedRegions (r1:r1s) r2s' where
      r' = Region (startOffset r2) (startOffset r1 - 1) (styles r2)
      r2s' = Region (startOffset r1) (endOffset r2) (styles r2) : r2s
    -- Both regions start simultaneously
    _ -> r' : mergeSortedRegions r1s' r2s' where
      r' = Region (startOffset r1) (min (endOffset r1) (endOffset r2)) (styles r1 `L.union` styles r2)
      (r1s', r2s') = case r1 of 
        _ | endOffset r1 < endOffset r2 -> (r1s, Region (endOffset r1 + 1) (endOffset r2) (styles r2) : r2s)
        _ | endOffset r2 < endOffset r1 -> (Region (endOffset r2 + 1) (endOffset r1) (styles r1) : r1s, r2s)
        _ -> (r1s, r2s)
      

orderOffsets :: Region -> Region
orderOffsets r@Region{..} = r{startOffset = so, endOffset = eo} where
  so = min startOffset endOffset
  eo = max startOffset endOffset
  

getRegions :: ViewState -> Buffer -> [Region]
getRegions v@ViewState{..} b@Buffer{..} = getSortedRegions initialPos v b regs where
  initialPos = posToOffset b top 0
  -- Selection region has offsets that are not ordered
  selection' = L.sortOn startOffset $ P.map orderOffsets selection
  regs' = L.sortOn startOffset regions
  regs'' = mergeSortedRegions selection' regs'
  regs = L.dropWhile fn regs''
  fn x = endOffset x < initialPos


stylePrefix :: Region -> String
stylePrefix Region{..} = prefix where 
  styleNames = L.map (\x -> case x of
    RS str -> str) styles
  styleOptions = L.map (`M.lookup` styleMapping) styleNames
  styleStrings =  L.map (fromMaybe defaultPrefix) styleOptions
  prefix = normal ++ L.concat styleStrings



drawLine :: Int -> Line -> [Region] -> IO [Region]
drawLine _ l [] = do 
  putStr normal
  putStr $ unpack l 
  return []
drawLine _ line rs | line == T.empty = return rs 
drawLine o l (Region{..}:rs) | o > endOffset = drawLine o l rs  
--drawLine o l (r@Region{..}:rs) | o < startOffset = drawLine o l rs  
drawLine o line (r@Region{..}:rs) | o + T.length line < endOffset = do
  putStr $ stylePrefix r
  putStr $ unpack line
  return $ r:rs
drawLine o l (r@Region{..}:rs) = do
  let (p, l') = T.splitAt (endOffset - o + 1) l 
  putStr $ stylePrefix r
  putStr $ unpack p
  drawLine (endOffset+1) l' rs

   
drawLines :: ViewState -> Int -> Buffer -> [Region] -> IO ()
drawLines ViewState{..} lNum _ _ | lNum == height = return ()
drawLines v@ViewState{..} lNum b@Buffer{..} rs = do
  let l = lNum + bTop
      h = if l < S.length content then S.index content l else T.empty 
      offset = posToOffset b l 0 + bLeft
      lne = T.take width $ T.drop bLeft h
      padding = L.replicate (width - T.length lne) ' '

  rs' <- drawLine offset lne rs
  -- Need to clear the state if a new regions starts on the unprinted Char at the end of the line
  let nextRs = if L.null rs' then Region 0 0 [] else L.head rs'
      (nRsLine, _) = offsetToPos b $ startOffset nextRs
  when (l == nRsLine) $ putStr (stylePrefix nextRs)

  -- putStr $ "**" ++ show nRsLine ++ " " ++ show nRsCol ++ " " ++ show (T.length h) -- ++ show nextRs
  putStr padding

  when (lNum /= height - 1) $ putStr $ toPos (lNum+top+1) left
  drawLines v (lNum+1) b rs'

cls :: String
cls = "\ESC[2J"

toPos :: Int -> Int -> String
toPos = printf "\ESC[%d;%dH"

index :: String
index = "\ESCD"

revIndex :: String
revIndex = "\ESCM"

smoothScroll :: String
smoothScroll = "\ESC[?4h"

setTopAndBottom :: Int -> Int -> String
setTopAndBottom = printf "\ESC[%d;%dr"

resetTopAndBottom :: String
resetTopAndBottom = "\ESC[r"

hideCursor :: String
hideCursor = "\ESC[?25l"

showCursor :: String
showCursor = "\ESC[?25h"

printable :: Char -> Bool
printable x = (fromEnum x :: Int) >= 0x20

normal :: String
normal = "\ESC[0m"

inverted :: String
inverted = "\ESC[7m"

fgColor :: Int -> String
fgColor = printf "\ESC[38;5;%dm"

bgColor :: Int -> String
bgColor = printf "\ESC[48;5;%dm"

saveCursor :: String
saveCursor = "\ESC[s"

restoreCursor :: String
restoreCursor = "\ESC[u"


printKey :: Char -> String
printKey c = str where
  i = fromEnum c
  str = printf "%d " i

printKeys :: String -> String
printKeys = foldMap printKey 



readKeys :: IO Keys
readKeys = do
  (str, _) <- fdRead stdInput 10 
  return $ case str of
    "\ESC[A" -> CursorUp
    "\ESC[B" -> CursorDown
    "\ESC[C" -> CursorRight
    "\ESC[D" -> CursorLeft
    "\ESC[F" -> End  -- Mac/Windows terminals
    "\ESCOF" -> End  -- Gnome Terminal
    "\ESCOH" -> Home
    "\ESC[H" -> Home
    "\x7f" -> Backspace  -- Delete key on OSX Keyboard
    "\n" -> CarriageReturn
    ['\ESC', a] -> Alt a
    [a] | printable a -> Alpha a
    [a] | a <= '\26' -> Ctrl $ toEnum ((fromEnum 'a' :: Int) + (fromEnum a - 1))
    a -> trace (toPos 36 0 ++ "[ " ++ printKeys a ++ " ]") UnknownKey


initTTY :: IO ()
initTTY = do 
  putStr cls
  oldTermSettings <- getTerminalAttributes stdInput
  {- modify settings -}
  let newTermSettings = flip withoutMode  EnableEcho   . -- don't echo keystrokes
                        flip withoutMode  ProcessInput $ -- turn on non-canonical mode
                        oldTermSettings
    -- flip withTime     vtime        . -- wait at most vtime decisecs per read
    -- flip withMinInput vmin         $ -- wait for >= vmin bytes per read

  setTerminalAttributes stdInput newTermSettings Immediately
  -- application `finally` setTerminalAttributes stdInput oldTermSettings Immediately
  putStrLn "Inited" 

draw :: ViewState -> Buffer -> IO ()
draw v@ViewState{..} b@Buffer{..} = do 

  --let buffSlice = S.take height $ S.drop top content
  -- putStr cls
  -- putStr $ setTopAndBottom 0 5
  -- let regions = getRegions v b
  putStr resetTopAndBottom
  putStr hideCursor
  putStr $ toPos top left
  drawLines v 0 b regions

  let Cursor {..} = cursor
      cursorX = col - bLeft + left
      cursorY = line - bTop + top
      drty = if contentChanged then "*" else " "

  
  -- putStr $ toPos 32 0 ++ show (getRegions v b) ++ "               "
  putStr $ toPos 52 0 ++ printf "%sLine: %-3d Col: %-3d (%d) -- %s   " drty line col (posToOffset b line col) (show v)
  putStr $ toPos cursorY cursorX
  putStr showCursor
  hFlush stdout

