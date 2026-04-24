module Main where

import Codegen
import ErrFmt (formatError)
import Lexer
import Parser
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStr, stderr)

main :: IO ()
main = do
  [filePath] <- getArgs
  contents <- readFile filePath
  case lexer contents of
    Left err -> do
      hPutStr stderr (formatError filePath contents ("[lexer] " ++ err) Nothing)
      exitFailure
    Right ts -> case runParser parseProgram ts of
      SoftErr msg pos -> reportErr filePath contents msg pos
      HardErr msg pos -> reportErr filePath contents msg pos
      Ok program _ -> writeFile "output.asm" (emitProgram program)
  where
    reportErr path src msg pos = do
      hPutStr stderr (formatError path src msg pos)
      exitFailure
