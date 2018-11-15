{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module MiniParsec where

import Data.Char
import Control.Monad
import Control.Applicative

newtype Parser a = Parser { parse :: String -> [(a, String)] }

instance Functor Parser where
  fmap f (Parser g) = Parser (\s -> let [(a, s1)] = g s
                                      in [(f a, s1)])

instance Applicative Parser where
  pure a = Parser (\s -> [(a, s)])
  (<*>) (Parser sab) (Parser fa) =  
    Parser (\s -> let [(a, s1)] = fa s 
                      [(fab, s2)] = sab s1
                    in [(fab a, s2)])

instance Monad Parser where
  return = pure
  (>>=) p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

instance MonadPlus Parser where
  mzero = Parser (\cs -> [])
  mplus p q = Parser (\s -> parse p s ++ parse q s)

instance Alternative Parser where
  empty = mzero
  (<|>) p q = Parser $ \s ->
    case parse p s of
      [] -> parse q s
      res -> res

runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res, [])] -> res
    [(_, rs)] -> error "Parser did not consume entire stream."
    _ -> error "Parser error."

item :: Parser Char
item = Parser $ \s ->
  case s of
    [] -> []
    (c:cs) -> [(c, cs)]

-- Another version of "some" and "many" in Control.Applicative
some' :: (Alternative f) => f a -> f [a]
some' v = some_v
  where
    many_v = some_v <|> pure []
    some_v = (:) <$> v <*> many_v

many' :: (Alternative f) => f a -> f [a]
many' v = many_v
  where
    many_v = some_v <|> pure []
    some_v = (:) <$> v <*> many_v

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= \c ->
  if p c 
  then return c
  else (Parser (\cs -> []))

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (flip elem s)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
  where rest a = (do f <- op
                     b <- p
                     b <- p
                     rest (f a b))
                 <|> return a

char :: Char -> Parser Char
char c = satisfy (c==)

natural :: Parser Integer
natural = read <$> some' (satisfy isDigit)

string :: String -> Parser String
string [] = return []
string (c:cs) = do { char c; string cs; return (c:cs) }

token :: Parser a -> Parser a
token p = do { a <- p; spaces; return a}

reserved :: String -> Parser String
reserved s = token (string s)
 
spaces :: Parser String
spaces = many' $ oneOf " \n\r"

digit :: Parser Char
digit = satisfy isDigit

number :: Parser Int
number = do
  s <- string "-" <|> return []
  cs <- some' digit
  return $ read (s ++ cs)

parens :: Parser a -> Parser a
parens m = do
  reserved "("
  n <- m
  reserved ")"
  return n
