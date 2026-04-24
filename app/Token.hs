{-# LANGUAGE GADTs #-}

module Token (Located (..), SourcePos (..), Token (..), Keyword (..)) where

data SourcePos = SourcePos
  { line :: Int,
    col :: Int
  }
  deriving (Show, Eq)

data Located a = Located
  { unLoc :: a,
    locPos :: SourcePos
  }
  deriving (Show, Eq)

data Keyword
  = IntKw
  | PrintKw -- Temporary Solution
  | IfKw
  | ElseKw
  | WhileKw
  | ForKw
  | DoKw
  | SwitchKw
  | CaseKw
  | DefaultKw
  | BreakKw
  | ContinueKw
  | VoidKw
  | ReturnKw
  | SizeofKw
  deriving (Show, Eq)

data Token
  = Literal Int
  | Keyword Keyword
  | Identifier String
  | -- Punctuation
    Comma
  | Semicolon
  | Colon
  | Question
  | OpenParen
  | CloseParen
  | OpenBrace
  | CloseBrace
  | OpenBracket
  | CloseBracket
  | Dot
  | Arrow
  | -- Arithmetic
    Plus
  | Minus
  | Multiply
  | Divide
  | Modulus
  | Increment
  | Decrement
  | -- Assignment
    Equal
  | PlusEqual
  | MinusEqual
  | MultiplyEqual
  | -- \*=
    DivideEqual
  | ModulusEqual
  | AmpersandEqual
  | PipeEqual
  | -- | =
    CaretEqual
  | -- \^=
    ShiftLeftEqual
  | ShiftRightEqual
  | -- Comparison
    EqualEqual
  | NotEqual
  | Less
  | Greater
  | LessEqual
  | GreaterEqual
  | -- Logical
    And
  | Or
  | Bang
  | -- Bitwise
    Ampersand
  | Pipe
  | Caret
  | Tilde
  | ShiftLeft
  | ShiftRight
  deriving (Show, Eq)
