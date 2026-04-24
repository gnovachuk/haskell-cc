module Lexer (lexer) where

import Data.Char (isAlphaNum, isDigit, isLetter, isSpace)
import Data.List (stripPrefix)
import Token

lexer :: String -> Either String [Located Token]
lexer = lexer' (SourcePos 1 1)

advance :: SourcePos -> Char -> SourcePos
advance pos '\n' = SourcePos (line pos + 1) 1
advance pos _ = SourcePos (line pos) (col pos + 1)

advanceBy :: SourcePos -> String -> SourcePos
advanceBy = foldl advance

-- | Longer prefixes appear first so we try them before shorter ones.
operators :: [(String, Token)]
operators =
  -- 3-character
  [ ("<<=", ShiftLeftEqual),
    (">>=", ShiftRightEqual),
    -- 2-character
    ("==", EqualEqual),
    ("!=", NotEqual),
    ("<=", LessEqual),
    (">=", GreaterEqual),
    ("&&", And),
    ("||", Or),
    ("<<", ShiftLeft),
    (">>", ShiftRight),
    ("++", Increment),
    ("--", Decrement),
    ("+=", PlusEqual),
    ("-=", MinusEqual),
    ("*=", MultiplyEqual),
    ("/=", DivideEqual),
    ("%=", ModulusEqual),
    ("&=", AmpersandEqual),
    ("|=", PipeEqual),
    ("^=", CaretEqual),
    ("->", Arrow),
    -- 1-character
    ("+", Plus),
    ("-", Minus),
    ("*", Multiply),
    ("/", Divide),
    ("%", Modulus),
    ("=", Equal),
    ("<", Less),
    (">", Greater),
    ("!", Bang),
    ("&", Ampersand),
    ("|", Pipe),
    ("^", Caret),
    ("~", Tilde),
    ("(", OpenParen),
    (")", CloseParen),
    ("{", OpenBrace),
    ("}", CloseBrace),
    ("[", OpenBracket),
    ("]", CloseBracket),
    (",", Comma),
    (";", Semicolon),
    (":", Colon),
    ("?", Question),
    (".", Dot)
  ]

-- | Try each operator prefix in order.
matchOperator :: String -> Maybe (Token, String, String)
matchOperator input = go operators
  where
    go [] = Nothing
    go ((prefix, tok) : rest) = case stripPrefix prefix input of
      Just remaining -> Just (tok, prefix, remaining)
      Nothing -> go rest

-- | Checks if this character is valid inside an identifier.
isWordChar :: Char -> Bool
isWordChar c = isAlphaNum c || c == '_'

-- | Checks if this character start an identifier.
isWordStart :: Char -> Bool
isWordStart c = isLetter c || c == '_'

lexer' :: SourcePos -> String -> Either String [Located Token]
lexer' _ [] = Right []
lexer' pos input@(ch : rest)
  | isSpace ch =
      lexer' (advance pos ch) rest
  | isDigit ch =
      let (digits, rest') = span isDigit input
       in emit (Literal (read digits)) digits rest'
  | isWordStart ch =
      let (word, rest') = span isWordChar input
       in emit (wordToToken word) word rest'
  | otherwise = case matchOperator input of
      Just (tok, consumed, rest') -> emit tok consumed rest'
      Nothing ->
        Left $
          "unexpected character `"
            ++ [ch]
            ++ "` at line "
            ++ show (line pos)
            ++ ", col "
            ++ show (col pos)
  where
    -- Emit one located token, then keep lexing with an updated position.
    emit tok consumed rest' =
      (Located tok pos :) <$> lexer' (advanceBy pos consumed) rest'

wordToToken :: String -> Token
wordToToken word = case word of
  "int" -> Keyword IntKw
  "print" -> Keyword PrintKw
  "if" -> Keyword IfKw
  "else" -> Keyword ElseKw
  "while" -> Keyword WhileKw
  "for" -> Keyword ForKw
  "do" -> Keyword DoKw
  "switch" -> Keyword SwitchKw
  "case" -> Keyword CaseKw
  "default" -> Keyword DefaultKw
  "break" -> Keyword BreakKw
  "continue" -> Keyword ContinueKw
  "void" -> Keyword VoidKw
  "return" -> Keyword ReturnKw
  "sizeof" -> Keyword SizeofKw
  _ -> Identifier word
