module HCC.AST
  ( Program (..)
  , Function (..)
  , Statement (..)
  , Expression (..)
  , ast
  ) where

import Control.Applicative
import qualified HCC.Lexer as Lexer
import qualified HCC.Parser as Parser

data Program = Program
  { function :: Function
  }
  deriving (Show)

data Function = Function
  { functionIdentifier :: String
  , functionStatement :: Statement
  }
  deriving (Show)

data Statement
  = CReturn Expression
  deriving (Show)

data Expression
  = IntegerConstant Integer
  | Negation Expression
  | BitwiseCompliment Expression
  | LogicalNegation Expression
  deriving (Show)

type AstParser = Parser.Parser Lexer.Token

tokenParser :: Lexer.Token -> AstParser Lexer.Token
tokenParser = Parser.unitParser

integerParser :: AstParser Integer
integerParser = Parser.Parser $ \input -> case input of
  (Lexer.IntegerLiteral n : xs) -> Just (xs, n)
  _ -> Nothing

identifierParser :: AstParser String
identifierParser = Parser.Parser $ \input -> case input of
  (Lexer.Identifier n : xs) -> Just (xs, n)
  _ -> Nothing

integerConstantParser :: AstParser Expression
integerConstantParser = IntegerConstant <$> integerParser

negationParser :: AstParser Expression
negationParser = Negation <$> ((tokenParser Lexer.Negative) *> expressionParser)

bitwiseComplimentParser :: AstParser Expression
bitwiseComplimentParser = BitwiseCompliment <$> ((tokenParser Lexer.Tilde) *> expressionParser)

logicalNegationParser :: AstParser Expression
logicalNegationParser = LogicalNegation <$> ((tokenParser Lexer.Bang) *> expressionParser)

expressionParser :: AstParser Expression
expressionParser =
  integerConstantParser
    <|> negationParser
    <|> bitwiseComplimentParser
    <|> logicalNegationParser

returnParser :: AstParser Statement
returnParser =
  CReturn <$> (tokenParser (Lexer.Keyword Lexer.CReturn) *> expressionParser <* tokenParser Lexer.Semicolon)

statementParser :: AstParser Statement
statementParser = returnParser

functionParser :: AstParser Function
functionParser =
  Function
    <$> (tokenParser (Lexer.Keyword Lexer.CInt) *> identifierParser)
    <*> ( tokenParser Lexer.OpenParenthesis
            *> tokenParser Lexer.CloseParenthesis
            *> tokenParser Lexer.OpenBrace
            *> statementParser
            <* tokenParser Lexer.CloseBrace
        )

programParser :: AstParser Program
programParser =
  Program <$> functionParser

ast :: [Lexer.Token] -> Maybe Program
ast tokens = case Parser.runParser programParser tokens of
  Just ([], program) -> Just program
  _ -> Nothing
