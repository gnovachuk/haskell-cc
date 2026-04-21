module Lexer (lexer) where

import Data.Char
import Token (Keyword (..), Token (..))

lexer :: String -> Either String [Token]
lexer input =
  case input of
    (ch : rest) -> case ch of
      '+' -> (Plus :) <$> lexer rest
      '-' -> (Minus :) <$> lexer rest
      '*' -> (Multiply :) <$> lexer rest
      '=' -> (Equal :) <$> lexer rest
      ';' -> (Semicolon :) <$> lexer rest
      '(' -> (OpenParen :) <$> lexer rest
      ')' -> (CloseParen :) <$> lexer rest
      '{' -> (OpenBrace :) <$> lexer rest
      '}' -> (CloseBrace :) <$> lexer rest
      _
        | isDigit ch ->
            let (digits, rest') = span isDigit input
             in (Literal (read digits) :) <$> lexer rest'
        | isLetter ch ->
            let (word, rest') = span isAlphaNum input
             in (wordToToken word :) <$> lexer rest'
        | isSpace ch -> lexer rest
        | otherwise -> Left ("[lexer] unexpected character: `" ++ [ch] ++ "`")
    [] -> Right []

wordToToken :: String -> Token
wordToToken word = case word of
  "int" -> Keyword IntKw
  "print" -> Keyword PrintKw
  "if" -> Keyword IfKw
  "else" -> Keyword ElseKw
  "while" -> Keyword WhileKw
  _ -> Identifier word
