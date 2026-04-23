module Main where

import Codegen
import Lexer
import Parser
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  [filePath] <- getArgs
  contents <- readFile filePath
  let tokens = lexer contents
  case tokens of
    Left err -> do putStrLn ("[lexer]: " ++ err); exitFailure
    Right ts -> case runParser parseProgram ts of
      Left err -> do putStrLn ("[parser]: " ++ err); exitFailure
      Right (program, _) -> writeFile "output.asm" (emitProgram program)

-- Right (stmt, _) -> print stmt
