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
    Right ts -> case runParser parseStmt ts of
      Left err -> putStrLn err
      Right (stmt, _) -> writeFile "output.asm" (emitProgram stmt)
