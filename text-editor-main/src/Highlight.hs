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



-- Variadic function to make applicative syntax easier to use
class BuildList r where
    rList :: [Region] -> r

instance BuildList [Region] where
    rList = id

instance (BuildList r) => BuildList ([Region] -> r) where
    rList x = rList . (x ++) 



type Text = T.Text

newtype ParseState = ParseState {stPos :: Int} deriving (Show)
initialState = ParseState 0
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

item :: SyntaxParser Char
item = SyntaxParser fn where 
  fn st t = case t of 
    a | T.null a -> []
    a -> [(T.head a, (st{stPos = stPos st + 1}, T.tail a))]

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

many1 = some

ignored :: SyntaxParser [Region]
ignored = do 
  item
  return [] 

region :: RegionStyle -> SyntaxParser a -> SyntaxParser [Region]
region l p = do 
  start <- getPos
  p
  end <- getPos
  return [Region start (end-1) [l]]

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

-- The actual syntax parser
-- TODO How best to deal with Comments -- include them in WhiteSpace?


-- Note can't use spaces here because many of nothing is an infinite loop
wsChars :: String
wsChars = "\r\n\t "
lowerCase = "abcdefghijklmnopqrstuvwxyz"
upperCase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
letters = lowerCase ++ upperCase
numbers = "1234567890"
alphaNum = letters ++ numbers
identifierBodyChars = alphaNum ++ "'"
keywords = ["where", "do", "let", "if", "then", "else", "case", "of", "instance", "class", "error"]


wsOrCommentR :: SyntaxParser [Region]
wsOrCommentR = F.fold <$> many (inlineCommentR <|> someSpaces) where 
  someSpaces = noregion (some (oneOf "\r\n "))

-- Parameterized because we may Want to color differently based on context
identifierR :: RegionStyle -> SyntaxParser [Region]
identifierR rs = rList <$> region rs ident <*> wsOrCommentR where
  ident = oneOf lowerCase *> some (oneOf alphaNum)

typeDeclarationR :: SyntaxParser [Region]
typeDeclarationR =  rList <$> identifierR Comment <*> region Number (string "::") <*> wsOrCommentR <*> region StringStyle (many (notOneOf "\n\r")) 



integerR :: SyntaxParser [Region]
integerR =  region Number $ do
  oneOf "+-" <|> return '0'
  many1 digit

stringR :: SyntaxParser [Region]
stringR =  region StringStyle $ do
  char '\"'
  manyTill item (char '\"')

inlineCommentR :: SyntaxParser [Region]
inlineCommentR = region Comment $ do
  string "{-"
  manyTill anyChar (string "-}")


topLevel :: SyntaxParser [Region]
topLevel =  integerR <|> stringR <|> inlineCommentR <|> typeDeclarationR


highLight :: BufferContent -> [Region]
highLight b = fold $ runParse (many (topLevel <|> ignored)) t where 
  withCR = fmap ((`T.snoc` '\n') . T.fromStrict) b  -- Note adds an extra CR at the end of the file (shouldn't matter)
  t = foldl T.append T.empty withCR


