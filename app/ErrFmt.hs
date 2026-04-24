module ErrFmt (formatError) where

import Token (SourcePos (..))

red, bold, cyan, reset :: String
red = "\ESC[31m"
bold = "\ESC[1m"
cyan = "\ESC[36m"
reset = "\ESC[0m"

padLeft :: Int -> String -> String
padLeft n s = replicate (n - length s) ' ' ++ s

getLineAt :: Int -> String -> Maybe String
getLineAt n src
  | n < 1 || n > length ls = Nothing
  | otherwise = Just (ls !! (n - 1))
  where
    ls = lines src

-- Format a parser/lexer error in a Rust-style multi-line layout.
-- Example:
--   error: expected `(` after if
--    --> test.c:3:6
--     |
--   3 |   if x
--     |      ^ expected `(` after if
--     |
formatError ::
  FilePath ->
  String -> -- source contents
  String -> -- error message
  Maybe SourcePos ->
  String
formatError path _ msg Nothing =
  unlines
    [ bold ++ red ++ "error:" ++ reset ++ " " ++ msg,
      " " ++ cyan ++ "-->" ++ reset ++ " " ++ path
    ]
formatError path src msg (Just pos) =
  let l = line pos
      c = col pos
      gutterW = length (show l)
      blankGutter = padLeft gutterW "" ++ " " ++ cyan ++ "|" ++ reset
      header = bold ++ red ++ "error:" ++ reset ++ " " ++ msg
      location =
        " "
          ++ cyan
          ++ "-->"
          ++ reset
          ++ " "
          ++ path
          ++ ":"
          ++ show l
          ++ ":"
          ++ show c
   in case getLineAt l src of
        Nothing ->
          -- Position refers to a line we can't find.
          unlines [header, location]
        Just codeLine ->
          let lineNumShown =
                cyan ++ padLeft gutterW (show l) ++ " |" ++ reset ++ " " ++ codeLine
              caretLine =
                blankGutter
                  ++ " "
                  ++ replicate (c - 1) ' '
                  ++ bold
                  ++ red
                  ++ "^ "
                  ++ msg
                  ++ reset
           in unlines
                [ header,
                  location,
                  blankGutter,
                  lineNumShown,
                  caretLine,
                  blankGutter
                ]
