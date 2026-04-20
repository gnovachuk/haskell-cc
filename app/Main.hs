module Main where

import Codegen
import Lexer
import Parser
import System.Environment (getArgs)

main :: IO ()
main = do
  [filePath] <- getArgs
  contents <- readFile filePath
  let tokens = lexer contents
  case tokens of
    Left err -> putStrLn err
    Right ts -> case runParser parseExpr ts of
      Left err -> putStrLn err
      Right (expr, _) -> writeFile "output.asm" (emitProgram expr)
