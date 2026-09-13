module HCC.Parser where

import Control.Applicative
import HCC.Lexer (Token (..))

data Program = Program
  { function :: Function
  }

data Function = Function
  { identifier :: String
  , statement :: Statement
  }
  deriving (Show)

data Statement
  = Return Expression
  deriving (Show)

data Expression
  = Integer Integer
  deriving (Show)

newtype Parser a = Parser
  { runParser :: [Token] -> Maybe ([Token], a)
  }

instance Functor Parser where
  fmap f (Parser l) = Parser $ \input -> do
    (input', x) <- l input
    Just (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser l1) <*> (Parser l2) = Parser $ \input -> do
    (input', f) <- l1 input
    (input'', a) <- l2 input'
    Just (input'', f a)

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (Parser l1) <|> (Parser l2) = Parser $ \input -> l1 input <|> l2 input

tokenParser :: Token -> Parser Token
tokenParser tok = Parser $ \input -> case input of
  (x : xs) | tok == x -> Just (xs, x)
  _ -> Nothing

parser :: [Token] -> Program
parser = undefined
