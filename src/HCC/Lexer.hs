module HCC.Lexer
  ( Keyword (..)
  , Token (..)
  , lexer
  ) where

import Control.Applicative
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import qualified HCC.Parser as Parser

-- Keywords are prefixed with C to avoid confusion
data Keyword
  = CInt
  | CReturn
  deriving (Show, Eq)

data Token
  = OpenParenthesis
  | CloseParenthesis
  | OpenBrace
  | CloseBrace
  | Semicolon
  | Plus
  | Negative
  | Asterisk
  | ForwardSlash
  | Percentage
  | Tilde
  | Bang
  | Keyword Keyword
  | Identifier String
  | IntegerLiteral Integer
  deriving (Show, Eq)

type LexerParser = Parser.Parser Char

stringParser :: String -> LexerParser String
stringParser = Parser.listParser

openParenthesisParser :: LexerParser Token
openParenthesisParser = (\_ -> OpenParenthesis) <$> stringParser "("

closerParenthesisParser :: LexerParser Token
closerParenthesisParser = (\_ -> CloseParenthesis) <$> stringParser ")"

openBraceParser :: LexerParser Token
openBraceParser = (\_ -> OpenBrace) <$> stringParser "{"

closeBraceParser :: LexerParser Token
closeBraceParser = (\_ -> CloseBrace) <$> stringParser "}"

semiColonParser :: LexerParser Token
semiColonParser = (\_ -> Semicolon) <$> stringParser ";"

plusParser :: LexerParser Token
plusParser = (\_ -> Plus) <$> stringParser "+"

negativeParser :: LexerParser Token
negativeParser = (\_ -> Negative) <$> stringParser "-"

asteriskParser :: LexerParser Token
asteriskParser = (\_ -> Asterisk) <$> stringParser "*"

forwardSlashParser :: LexerParser Token
forwardSlashParser = (\_ -> ForwardSlash) <$> stringParser "/"

percentageParser :: LexerParser Token
percentageParser = (\_ -> Percentage) <$> stringParser "%"

tildeParser :: LexerParser Token
tildeParser = (\_ -> Tilde) <$> stringParser "~"

bangParser :: LexerParser Token
bangParser = (\_ -> Bang) <$> stringParser "!"

grammarParser :: LexerParser Token
grammarParser =
  openParenthesisParser
    <|> closerParenthesisParser
    <|> openBraceParser
    <|> closeBraceParser
    <|> semiColonParser
    <|> plusParser
    <|> negativeParser
    <|> asteriskParser
    <|> forwardSlashParser
    <|> percentageParser
    <|> tildeParser
    <|> bangParser

spanParser :: (Char -> Bool) -> LexerParser String
spanParser f = Parser.Parser $ \input ->
  case span f input of
    ("", _) -> Nothing
    (output, input') -> Just (input', output)

integerLiteralParser :: LexerParser Token
integerLiteralParser = (\input -> IntegerLiteral $ read input) <$> spanParser isDigit

identifierParser :: LexerParser Token
identifierParser = Parser.Parser $ \input ->
  case Parser.runParser (spanParser isAlphaNum) input of
    Just (output, i@(h : _)) | isAlpha h -> Just (output, Identifier i)
    _ -> Nothing

keywordParser :: LexerParser Token
keywordParser = Parser.Parser $ \input -> do
  (rest, kw) <- Parser.runParser (spanParser isAlphaNum) input
  case kw of
    "int" -> Just (rest, Keyword CInt)
    "return" -> Just (rest, Keyword CReturn)
    _ -> Nothing

tokenParser :: LexerParser Token
tokenParser = grammarParser <|> keywordParser <|> identifierParser <|> integerLiteralParser

lexer :: String -> Maybe [Token]
lexer [] = Just []
lexer input@(c : cs)
  | isSpace c = lexer cs
  | otherwise = do
      (input', tok) <- Parser.runParser tokenParser input
      toks <- lexer input'
      Just (tok : toks)
