{-# LANGUAGE InstanceSigs #-}
-- {-# LANGUAGE DerivingVia #-}

module Parser where

import Control.Monad.State(StateT(..))
import Control.Applicative(Applicative(..), Alternative(..))
import Control.Monad(void)
import Data.Char(isSpace, isLetter, isDigit)
import Data.List(uncons, isPrefixOf)

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }
  -- deriving (Functor, Applicative, Alternative, Monad) via StateT String Maybe

eof :: Parser ()
eof = Parser $ \s ->
  case s of
    [] -> Just ((), [])
    _ : _ -> Nothing

anyChar :: Parser Char
anyChar = Parser uncons

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s ->
  case s of
    c : rest | p c -> Just (c, rest)
    _ -> Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

letter :: Parser Char
letter = satisfy isLetter

digit :: Parser Char
digit = satisfy isDigit

string :: String -> Parser String
string a = Parser $ \s ->
  if a `isPrefixOf` s
  then Just (a, drop (length a) s)
  else Nothing

spaces :: Parser ()
spaces = Parser $ \s -> Just ((), dropWhile isSpace s)

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ \s ->
    case p s of
      Nothing -> Nothing
      Just (a, rest) -> Just (f a, rest)

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ \s -> Just (a, s)

  liftA2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
  liftA2 f (Parser pa) (Parser pb) = Parser $ \s -> do
    (a, rest) <- pa s
    (b, rest') <- pb rest
    pure (f a b, rest')

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  Parser a <|> Parser b = Parser $ \s -> a s <|> b s

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser p >>= f = Parser $ \s ->
    case p s of
      Nothing -> Nothing
      Just (a, rest) -> runParser (f a) rest

endOfLine :: Parser ()
endOfLine = void $ string "\r\n" <|> string "\n"

optional :: Parser a -> Parser ()
optional p = void p <|> pure ()

between :: Parser b -> Parser e -> Parser a -> Parser a
between b e a = b *> a <* e

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy a sep = ((:) <$> a <*> many (sep *> a)) <|> pure []

sepEndBy :: Parser a -> Parser sep -> Parser [a]
sepEndBy a sep = ((:) <$> a <*> many (sep *> a) <* optional sep) <|> pure []

skipManyTill :: Parser a -> Parser ()
skipManyTill p = Parser $ \s ->
  runParser (void p) s
  <|> case s of
    [] -> Nothing
    _ : rest -> runParser (skipManyTill p) rest
