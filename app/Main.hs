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
      SoftErr err -> do putStrLn ("[parser]: " ++ err); exitFailure
      HardErr err -> do putStrLn ("[parser]: " ++ err); exitFailure
      Ok program _ -> writeFile "output.asm" (emitProgram program)

-- Right (stmt, _) -> print stmt
