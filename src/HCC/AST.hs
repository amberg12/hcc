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
  | Addition (Expression, Expression)
  | Subtraction (Expression, Expression)
  | Multiplication (Expression, Expression)
  | Division (Expression, Expression)
  | Modulo (Expression, Expression)
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

expressionParser :: AstParser Expression
expressionParser = do
  first <- termParser
  rest <- many ((,) <$> (addOp <|> subOp) <*> termParser)
  pure $ foldl (\acc (ctor, next) -> ctor (acc, next)) first rest
 where
  addOp = tokenParser Lexer.Plus *> pure Addition
  subOp = tokenParser Lexer.Negative *> pure Subtraction

termParser :: AstParser Expression
termParser = do
  first <- factorParser
  rest <- many ((,) <$> (mulOp <|> divOp <|> modOp) <*> factorParser)
  pure $ foldl (\acc (ctor, next) -> ctor (acc, next)) first rest
 where
  mulOp = tokenParser Lexer.Asterisk *> pure Multiplication
  divOp = tokenParser Lexer.ForwardSlash *> pure Division
  modOp = tokenParser Lexer.Percentage *> pure Modulo

factorParser :: AstParser Expression
factorParser =
  parenParser <|> negParser <|> logicalNegParser <|> bitwiseComplimentParser <|> integerConstantParser
 where
  parenParser = (tokenParser (Lexer.OpenParenthesis) *> expressionParser <* tokenParser (Lexer.CloseParenthesis))
  negParser = Negation <$> ((tokenParser Lexer.Negative) *> expressionParser)
  logicalNegParser = LogicalNegation <$> ((tokenParser Lexer.Bang) *> expressionParser)
  bitwiseComplimentParser = BitwiseCompliment <$> ((tokenParser Lexer.Tilde) *> expressionParser)
  integerConstantParser = IntegerConstant <$> integerParser

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
