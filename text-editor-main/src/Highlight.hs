{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Highlight where 

import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import Data.Foldable as F
import Data.Char
import Control.Monad
import Control.Applicative
import TextBuffer

-- TODO Start of line

-- Variadic function to make applicative syntax easier to use
class BuildList r where
    rList :: [Region] -> r

instance BuildList [Region] where
    rList = id

instance (BuildList r) => BuildList ([Region] -> r) where
    rList x = rList . (x ++) 



type Text = T.Text

data ParseState = ParseState {stPos :: Int, stCol :: Int} deriving (Show)
initialState = ParseState 0 0
type PText = (ParseState, T.Text)

newtype SyntaxParser a = SyntaxParser { parse :: ParseState -> Text -> [(a, PText)] }


runParse :: SyntaxParser a -> T.Text -> a
runParse m s =
  case parse m initialState s of
    [(res, (st, t))] | T.null t -> res
    [(res, rs)]   -> error $ "Parser did not consume entire stream. -- " ++ show rs
    _           -> error "Parser error."

getPos :: SyntaxParser Int
getPos = SyntaxParser $ \st t -> [(stPos st, (st, t))]

getCol :: SyntaxParser Int
getCol = SyntaxParser $ \st t -> [(stCol st, (st, t))]

item :: SyntaxParser Char
item = SyntaxParser fn where 
  fn st t = case t of 
    a | T.null a -> []
    a -> [(T.head a, (st{stPos = stPos st + 1, stCol = col'}, T.tail a))] where
      col' = if T.head a `elem` "\n\r" then 0 else stCol st + 1

bind :: SyntaxParser a -> (a -> SyntaxParser b) -> SyntaxParser b
bind p f = SyntaxParser parseFn where
  parseFn st t = concatMap fn parsed where
    fn (a, (st', t')) = parse (f a) st' t'
    parsed = parse p st t


unit :: a -> SyntaxParser a
unit a = SyntaxParser (\st s -> [(a,(st, s))])

instance Functor SyntaxParser where
  fmap f (SyntaxParser cs) = SyntaxParser (\st s -> [(f a, b) | (a, b) <- cs st s])

instance Applicative SyntaxParser where
  pure = return
  (SyntaxParser cs1) <*> (SyntaxParser cs2) = SyntaxParser (\st s -> [(f a, s2) | (f, (st1, s1)) <- cs1 st s, (a, s2) <- cs2 st1 s1])

instance Monad SyntaxParser where
  return = unit
  (>>=)  = bind

instance MonadPlus SyntaxParser where
  mzero = failure
  mplus = combine

instance Alternative SyntaxParser where
  empty = mzero
  (<|>) = option

combine :: SyntaxParser a -> SyntaxParser a -> SyntaxParser a
combine p q = SyntaxParser (\st s -> parse p st s ++ parse q st s)

failure :: SyntaxParser a
failure = SyntaxParser (\a b -> [])

option :: SyntaxParser a -> SyntaxParser a -> SyntaxParser a
option  p q = SyntaxParser $ \st s ->
  case parse p st s of
    []     -> parse q st s
    res    -> res

satisfy :: (Char -> Bool) -> SyntaxParser Char
satisfy p = item `bind` \c ->
  if p c
  then unit c
  else SyntaxParser (\a b -> [])

try :: SyntaxParser a -> SyntaxParser (Maybe a)
try p = SyntaxParser $ \st s -> 
  case parse p st s of
    [] -> [(Nothing, (st, s))]
    [(a, t')] -> [(Just a, t')]

oneOf :: String -> SyntaxParser Char
oneOf s = satisfy (`elem` s)

notOneOf :: String -> SyntaxParser Char
notOneOf s = satisfy (not . (`elem` s))

char :: Char -> SyntaxParser Char
char c = satisfy (c ==)

notChar :: Char -> SyntaxParser Char
notChar c = satisfy (c /=)


string :: String -> SyntaxParser String
string [] = return []
string (c:cs) = do { char c; string cs; return (c:cs)}

spaces :: SyntaxParser String
spaces = many $ oneOf " \n\r"

digit :: SyntaxParser Char
digit = satisfy isDigit

-- many1 = some

ignored :: SyntaxParser [Region]
ignored = do 
  item
  return [] 

region :: String -> SyntaxParser a -> SyntaxParser [Region]
region l p = do 
  start <- getPos
  p
  end <- getPos
  return [Region start (end-1) [RS l]]

noregion :: SyntaxParser a -> SyntaxParser [Region]
noregion p = do {p ; return []}

manyTill :: SyntaxParser a -> SyntaxParser a -> SyntaxParser [a]
manyTill p end = do { end; return [] }
                 <|>
                 do {x <- p; xs <- manyTill p end; return (x:xs)}

anyChar :: SyntaxParser String
anyChar = do 
  c <- item
  return [c]

oneStringOf :: String -> [String] -> SyntaxParser [Region]
oneStringOf rs strs = foldl1 (<|>) $ map (region rs . string) strs

-- The actual syntax parser


-- Note can't use spaces here because many of nothing is an infinite loop
wsChars :: String
wsChars = "\r\n\t "
lowerCase = "abcdefghijklmnopqrstuvwxyz"
upperCase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
letters = lowerCase ++ upperCase
numbers = "1234567890"
alphaNum = letters ++ numbers ++ "_"
identifierBodyChars = alphaNum ++ "'"
keywords = ["module", "where", "do", "let", "if", "then", "else", "case", "of", "instance", "class", "error"]


inlineCommentR :: SyntaxParser [Region]
inlineCommentR = region "comment" $ do
  string "{-"
  manyTill anyChar (string "-}")

eolCommentR :: SyntaxParser [Region]
eolCommentR = region "comment" $ do
  string "--"
  manyTill anyChar (string "\n")


wsOrCommentR :: SyntaxParser [Region]
wsOrCommentR = F.fold <$> many (inlineCommentR <|> eolCommentR <|> someSpaces) where 
  someSpaces = noregion (some (oneOf "\r\n "))

br :: SyntaxParser [Region]
br = F.fold <$> some (inlineCommentR <|> eolCommentR <|> someSpaces) where 
  someSpaces = noregion (some (oneOf "\r\n "))

-- Parameterized because we may Want to color differently based on context


nothing :: SyntaxParser [Region]
nothing = return []

token :: String -> SyntaxParser a -> SyntaxParser [Region]
token r p = rList <$> region r p <*> br


identifierR :: SyntaxParser [Region]
identifierR = region "identifier" ident where 
  ident = oneOf lowerCase <* many (oneOf identifierBodyChars)

moduleNameR :: SyntaxParser [Region]
moduleNameR = region "moduleName" mName where
  mName = mPart <* many (char '.' <* mPart)
  mPart = oneOf upperCase <* many (oneOf identifierBodyChars)

infixOpR :: SyntaxParser [Region]
infixOpR = region "infixOp" inf where
  inf = char '(' *> some (notOneOf ( ')':'\\':' ':alphaNum )) *> char ')'

multiLineStringBreak :: SyntaxParser [Region]
multiLineStringBreak = do 
  _ <- char '\\' *> oneOf "\r\n" *> many (oneOf wsChars) *> char '\\'
  return []

moduleExportsR :: SyntaxParser [Region]
moduleExportsR = rList <$> noregion (char '(') <*> wsOrCommentR <*> exports <*> (noregion (char ')') <|> nothing) <*> wsOrCommentR where
  exports = fold <$> many (nested <|> identifierR <|> moduleNameR <|> infixOpR <|> br <|> noregion (notChar ')'))
  nested = rList <$> moduleNameR <*> wsOrCommentR <*> noregion (char '(') <*> nestedExports <*> (noregion (char ')') <|> nothing) :: SyntaxParser[Region]
  nestedExports = fold <$> many (identifierR <|> moduleNameR <|> infixOpR <|> br <|> noregion (notChar ')'))

moduleR :: SyntaxParser [Region]
moduleR = rList <$> moduleP <*> containedP <*> whereP where
  moduleP = token "keyword" (string "module") 
  containedP = fold <$> many ((rList <$> moduleNameR <*> br) <|> moduleExportsR)
  whereP = token "keyword" (string "where") <|> nothing 


importR :: SyntaxParser [Region]
importR = rList <$> token "keyword" (string "import") <*> patts <*> wsOrCommentR where 
  keywords = token "keyword" (string "qualified" <|> string "as" <|> string "hiding")
  patt = keywords <|> moduleNameR <|> moduleExportsR <|> br
  patts = fold <$> many patt


topLevel :: SyntaxParser [Region]
topLevel =  moduleR <|> importR


highLight :: BufferContent -> [Region]
highLight b = fold $ runParse (many (topLevel <|> ignored)) t where 
  withCR = fmap ((`T.snoc` '\n') . T.fromStrict) b  -- Note adds an extra CR at the end of the file (shouldn't matter)
  t = foldl T.append T.empty withCR


