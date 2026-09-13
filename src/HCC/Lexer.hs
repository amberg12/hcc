module HCC.Lexer
  ( Keyword (..)
  , Token (..)
  , lexer
  ) where

import Control.Applicative
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)

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
  | Keyword Keyword
  | Identifier String
  | IntegerConstant Integer
  deriving (Show, Eq)

newtype Lexer a = Lexer
  { runLexer :: String -> Maybe (String, a)
  }

instance Functor Lexer where
  fmap f (Lexer l) = Lexer $ \input -> do
    (input', x) <- l input
    Just (input', f x)

instance Applicative Lexer where
  pure x = Lexer $ \input -> Just (input, x)
  (Lexer l1) <*> (Lexer l2) = Lexer $ \input -> do
    (input', f) <- l1 input
    (input'', a) <- l2 input'
    Just (input'', f a)

instance Alternative Lexer where
  empty = Lexer $ \_ -> Nothing
  (Lexer l1) <|> (Lexer l2) = Lexer $ \input -> l1 input <|> l2 input

charLexer :: Char -> Lexer Char
charLexer c = Lexer $ \input -> case input of
  (x : xs) | x == c -> Just (xs, c)
  _ -> Nothing

stringLexer :: String -> Lexer String
stringLexer = traverse charLexer

openParenthesisLexer :: Lexer Token
openParenthesisLexer = (\_ -> OpenParenthesis) <$> stringLexer "("

closeParenthesisLexer :: Lexer Token
closeParenthesisLexer = (\_ -> CloseParenthesis) <$> stringLexer ")"

openBraceLexer :: Lexer Token
openBraceLexer = (\_ -> OpenBrace) <$> stringLexer "{"

closeBraceLexer :: Lexer Token
closeBraceLexer = (\_ -> CloseBrace) <$> stringLexer "}"

semicolonLexer :: Lexer Token
semicolonLexer = (\_ -> Semicolon) <$> stringLexer ";"

grammarLexer :: Lexer Token
grammarLexer =
  openParenthesisLexer
    <|> closeParenthesisLexer
    <|> openBraceLexer
    <|> closeBraceLexer
    <|> semicolonLexer

spanLexer :: (Char -> Bool) -> Lexer String
spanLexer f = Lexer $ \input ->
  case span f input of
    ("", _) -> Nothing
    (output, input') -> Just (input', output)

integerConstantLexer :: Lexer Token
integerConstantLexer = (\input -> IntegerConstant $ read input) <$> spanLexer isDigit

identifierLexer :: Lexer Token
identifierLexer = Lexer $ \input ->
  case runLexer (spanLexer isAlphaNum) input of
    Just (output, i@(h : _)) | isAlpha h -> Just (output, Identifier i)
    _ -> Nothing

keywordLexer :: Lexer Token
keywordLexer = Lexer $ \input -> do
  (rest, kw) <- runLexer (spanLexer isAlphaNum) input
  case kw of
    "int" -> Just (rest, Keyword CInt)
    "return" -> Just (rest, Keyword CReturn)
    _ -> Nothing

tokenLexer :: Lexer Token
tokenLexer = grammarLexer <|> keywordLexer <|> identifierLexer <|> integerConstantLexer

lexer :: String -> Maybe [Token]
lexer [] = Just []
lexer input@(c : cs)
  | isSpace c = lexer cs
  | otherwise = do
      (input', tok) <- runLexer tokenLexer input
      toks <- lexer input'
      Just (tok : toks)
