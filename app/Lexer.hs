module Lexer (lexer) where

import Data.Char
import Token

lexer :: String -> Either String [Located Token]
lexer = lexer' (SourcePos 1 1)

advance :: SourcePos -> Char -> SourcePos
advance pos ch = case ch of
  '\n' -> SourcePos (line pos + 1) 1
  _ -> SourcePos (line pos) (col pos + 1)

single :: Token -> SourcePos -> Char -> String -> Either String [Located Token]
single tok pos ch rest = (Located tok pos :) <$> lexer' (advance pos ch) rest

-- emits `tok` at `pos`, then continues lexing with `rest` starting at `pos` advanced by `consumed` chars
multi :: Token -> SourcePos -> String -> String -> Either String [Located Token]
multi tok pos consumed rest =
  (Located tok pos :) <$> lexer' (advanceBy pos consumed) rest

advanceBy :: SourcePos -> String -> SourcePos
advanceBy = foldl advance

lexer' :: SourcePos -> String -> Either String [Located Token]
lexer' pos input =
  case input of
    (ch : rest) -> case ch of
      '+' -> single Plus pos ch rest
      '-' -> single Minus pos ch rest
      '*' -> single Multiply pos ch rest
      '=' -> single Equal pos ch rest
      ';' -> single Semicolon pos ch rest
      '(' -> single OpenParen pos ch rest
      ')' -> single CloseParen pos ch rest
      '{' -> single OpenBrace pos ch rest
      '}' -> single CloseBrace pos ch rest
      ',' -> single Comma pos ch rest
      _
        | isDigit ch ->
            let (digits, rest') = span isDigit input
             in multi (Literal $ read digits) pos digits rest'
        | isLetter ch ->
            let (word, rest') = span isAlphaNum input
             in multi (wordToToken word) pos word rest'
        | isSpace ch -> lexer' (advance pos ch) rest
        | otherwise -> Left ("unexpected character: `" ++ [ch] ++ "`")
    [] -> Right []

wordToToken :: String -> Token
wordToToken word = case word of
  "int" -> Keyword IntKw
  "print" -> Keyword PrintKw
  "if" -> Keyword IfKw
  "else" -> Keyword ElseKw
  "while" -> Keyword WhileKw
  "break" -> Keyword BreakKw
  "continue" -> Keyword ContinueKw
  "void" -> Keyword VoidKw
  "return" -> Keyword ReturnKw
  _ -> Identifier word
