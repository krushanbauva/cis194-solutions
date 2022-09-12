{- CIS 194 HW 10
   due Monday, 1 April
-}

{-# LANGUAGE InstanceSigs #-}

module Hw10 where

import Control.Applicative

import Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f :: String -> Maybe(Char, String)
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  -- f :: a -> b
  -- g :: String -> Maybe (a, String)
  -- x :: String -> Maybe (b, String)
  fmap f (Parser g) = Parser x
    where x inp = fmap (first f) (g inp)

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser f
    where f inp = Just (a, inp)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) p1 p2 = Parser f
    where
      f inp = case runParser p1 inp of
        -- f :: String -> Maybe (b, String)
        -- runParser p2 remain :: Maybe (a, String)
        Nothing -> Nothing
        Just (resf, remain) -> fmap (first resf) (runParser p2 remain)

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

-- >>> runParser abParser "abcde"
-- Just (('a','b'),"cde")

-- >>> runParser abParser "akcde"
-- Nothing

abParser_ :: Parser ()
abParser_ = (\x y -> ()) <$> char 'a' <*> char 'b'

-- >>> runParser abParser_ "abcde"
-- Just ((),"cde")

-- >>> runParser abParser_ "akbcde"
-- Nothing

intPair :: Parser [Integer]
intPair = (\x _ z -> [x,z]) <$> posInt <*> char ' ' <*> posInt

-- >>> runParser intPair "12 34"
-- Just ([12,34],"")

instance Alternative Parser where
  empty :: Parser a
  empty = Parser f where
    f inp = Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) p1 p2 = Parser f where
    f inp = case runParser p1 inp of
      Nothing -> runParser p2 inp
      Just(_, _) -> runParser p1 inp

intOrUppercase :: Parser ()
intOrUppercase = (\_ -> ()) <$> posInt <|> (\_ -> ()) <$> satisfy isUpper

-- >>> runParser intOrUppercase "342abcd"
-- Just ((),"abcd")

-- >>> runParser intOrUppercase "XYZ"
-- Just ((),"YZ")

-- >>> runParser intOrUppercase "foo"
-- Nothing
