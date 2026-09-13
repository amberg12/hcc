module HCC.Parser where

import Control.Applicative
import HCC.Lexer (Keyword (..), Token (..))

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
  = IntegerConstant Integer
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

integerParser :: Parser Integer
integerParser = Parser $ \input -> case input of
  (IntegerLiteral n : xs) -> Just (xs, n)
  _ -> Nothing

identifierParser :: Parser String
identifierParser = Parser $ \input -> case input of
  (Identifier n : xs) -> Just (xs, n)
  _ -> Nothing

expressionParser :: Parser Expression
expressionParser = IntegerConstant <$> integerParser

returnParser :: Parser Statement
returnParser =
  Return <$> (tokenParser (Keyword CReturn) *> expressionParser <* tokenParser Semicolon)

statementParser :: Parser Statement
statementParser = returnParser

functionParser :: Parser Function
functionParser =
  Function
    <$> (tokenParser (Keyword CInt) *> identifierParser)
    <*> ( tokenParser OpenParenthesis
            *> tokenParser CloseParenthesis
            *> tokenParser OpenBrace
            *> statementParser
            <* tokenParser CloseBrace
        )

parser :: [Token] -> Program
parser = undefined
