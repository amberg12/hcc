module HCC.Parser
  ( Parser (..)
  , unitParser
  , listParser
  ) where

import Control.Applicative

newtype Parser stream out = Parser
  { runParser :: [stream] -> Maybe ([stream], out)
  }

instance Functor (Parser stream) where
  fmap f (Parser l) = Parser $ \input -> do
    (input', x) <- l input
    Just (input', f x)

instance Applicative (Parser stream) where
  pure x = Parser $ \input -> Just (input, x)
  (Parser l1) <*> (Parser l2) = Parser $ \input -> do
    (input', f) <- l1 input
    (input'', a) <- l2 input'
    Just (input'', f a)

instance Alternative (Parser stream) where
  empty = Parser $ \_ -> Nothing
  (Parser l1) <|> (Parser l2) = Parser $ \input -> l1 input <|> l2 input

instance Monad (Parser stream) where
  (Parser p) >>= f = Parser $ \input -> do
    (input', x) <- p input
    runParser (f x) input'

unitParser :: (Eq a) => a -> Parser a a
unitParser x = Parser $ \input -> case input of
  y : ys | x == y -> Just (ys, y)
  _ -> Nothing

listParser :: (Eq a) => [a] -> Parser a [a]
listParser = traverse unitParser
