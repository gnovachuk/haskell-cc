{-# LANGUAGE InstanceSigs #-}

{- HLINT ignore "Use const" -}
{- HLINT ignore "Use tuple-section" -}

module Codegen (emitProgram) where

import AST

type SymTable = [(String, Int)]

type LabelCount = Int

type CurrentLoop = Maybe Int

data CodegenState = CodegenState
  { symTable :: SymTable,
    labelCount :: LabelCount,
    currentLoop :: CurrentLoop
  }

newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
  fmap f sa = State (\st -> let (a, st') = runState sa st in (f a, st'))

instance Applicative (State s) where
  pure :: a -> State s a
  pure a = State (\st -> (a, st))
  sf <*> sa =
    State
      ( \st ->
          let (f, st') = runState sf st
              (a, st'') = runState sa st'
           in (f a, st'')
      )

instance Monad (State s) where
  sa >>= f =
    State
      ( \st ->
          let (a, s') = runState sa st
           in runState (f a) s'
      )

get :: State s s
get = State (\st -> (st, st))

-- Replace old state entirely
put :: s -> State s ()
put newSt = State (\_ -> ((), newSt))

modify :: (s -> s) -> State s ()
modify f = State (\st -> ((), f st))

evalState :: State s a -> s -> a
evalState sa st = fst (runState sa st)

emitProgram :: Stmt -> String
emitProgram stmt =
  let initState = CodegenState {symTable = [], labelCount = 0, currentLoop = Nothing}
   in unlines
        [ ".data",
          "fmt:",
          "  .asciz \"%d\\n\"",
          "",
          ".text",
          ".global _main ; tell the linker `_main` exists and is visible",
          ".align 2      ; align code to 4-byte boundary (ARM64 requirement)",
          "_main:        ; the entry point label",
          "  ; x29 = frame pointer (like rbp on x86)",
          "  ; x30 = link register (return addr after function ends)",
          "  stp x29, x30, [sp, -16]!    ; push old frame and link register onto stack",
          "  mov x29, sp                 ; x29 -> base of stack frame"
        ]
        ++ evalState (codegenStmt stmt) initState
        ++ unlines
          [ "; exit cleanly",
            "  ldp x29, x30, [sp], 16    ; restore frame & link register",
            "  mov x0,  #0  ; exit code 0",
            "  mov x16, #1  ; syscall number for exit on macOS", -- exit syscall
            "  svc #0x80    ; make the syscall"
          ]

codegenStmt :: Stmt -> State CodegenState String
codegenStmt stmt = case stmt of
  Print expr -> do
    exprCode <- codegenExpr expr
    pure $
      unlines
        [ exprCode,
          "  ; expr result remains on stack (arg for printf)", -- pop final result
          "  adrp x0, fmt@PAGE ; load format string address",
          "  add x0, x0, fmt@PAGEOFF ; into x0 (1st arg)",
          "  bl _printf   ; call printf",
          "  add sp, sp, 16    ; cleanup stack (pop expr result)"
        ]
  VarDecl name expr -> do
    st <- get
    exprCode <- codegenExpr expr
    let offset = 1 + length (symTable st)
    modify (\st -> st {symTable = (name, offset) : symTable st})
    pure $
      unlines
        [ exprCode,
          "  ; store `" ++ name ++ "` at offset " ++ show offset ++ " (* 16)",
          "  ldr x0, [sp], 16",
          "  str x0, [x29, #-" ++ show (offset * 16) ++ "]",
          "  sub sp, sp, #16"
        ]
  Block stmts -> do
    st <- get
    code <- codegenStmts stmts
    modify (\s -> s {symTable = symTable st}) -- Restore original table (so symbols in inner block scope dissapear)
    pure code
  If cond thenStmt elseStmt -> do
    st <- get
    let lc = labelCount st
    modify (\s -> s {labelCount = labelCount st + 1}) -- increment labelCount
    condCode <- codegenExpr cond
    thenCode <- codegenStmt thenStmt
    elseCode <- case elseStmt of
      Nothing -> pure ""
      Just elseStmt' -> codegenStmt elseStmt'
    pure $
      unlines
        [ condCode,
          "  ldr x0, [sp], 16",
          "  cmp x0, #0      ; if condition false (== 0), jump to end/else (skip if stmt)",
          case elseStmt of
            Nothing -> "  beq endif_" ++ show lc
            Just _ -> "  beq else_" ++ show lc,
          thenCode, -- ignore symbol table, right?
          "  b endif_" ++ show lc, -- skip past ElseStmt (redundant when there's no else branch)
          case elseStmt of
            Nothing -> ""
            Just _ -> "else_" ++ show lc ++ ":\n" ++ elseCode,
          "endif_" ++ show lc ++ ":"
        ]
  While cond body -> do
    st <- get
    let lc = labelCount st
    let prevLoop = currentLoop st

    modify (\s -> s {labelCount = labelCount st + 1, currentLoop = Just (labelCount st)})
    condCode <- codegenExpr cond
    bodyCode <- codegenStmt body
    modify (\s -> s {currentLoop = prevLoop}) -- restore previous loop
    pure $
      unlines
        [ "while_" ++ show lc ++ ":",
          condCode, -- eval cond
          "  ldr x0, [sp], 16", -- x0 <- condition expr result
          "  cmp x0, #0", -- compare x0 t0 0
          "  beq endwhile_" ++ show lc, -- if false, exit loop
          bodyCode,
          "  b while_" ++ show lc, -- continue loop
          "endwhile_" ++ show lc ++ ":"
        ]
  Break -> do
    st <- get
    let curLoop = currentLoop st
    pure $
      "  b endwhile_"
        ++ ( case curLoop of
               Just n -> show n
               Nothing -> error "break outside of loop"
           )
        ++ "\n"
  Continue -> do
    st <- get
    let curLoop = currentLoop st
    pure $
      "  b while_"
        ++ ( case curLoop of
               Just n -> show n
               Nothing -> error "continue outside of loop"
           )
        ++ "\n"
  ExprStmt expr -> do
    exprCode <- codegenExpr expr
    pure $
      unlines
        [ exprCode,
          "  add sp, sp, 16    ; cleanup stack (pop expr result)"
        ]

codegenStmts :: [Stmt] -> State CodegenState String
codegenStmts [] = pure ""
codegenStmts (stmt : rest) = do
  code <- codegenStmt stmt
  restCode <- codegenStmts rest
  pure (code ++ restCode)

codegenExpr :: Expr -> State CodegenState String
codegenExpr expr = case expr of
  LitExpr n ->
    pure $
      unlines
        [ "  mov x0, #" ++ show n,
          "  str x0, [sp, -16]!"
        ]
  VarExpr name -> do
    st <- get
    let table = symTable st
    let offset = case lookup name table of
          Just o -> o
          Nothing -> error $ "Variable not found: " ++ name
    pure $
      unlines
        [ "  ldr x0, [x29, #-" ++ show (offset * 16) ++ "]",
          "  str x0, [sp, -16]!"
        ]
  BinOp op e1 e2 -> do
    lhsCode <- codegenExpr e1
    rhsCode <- codegenExpr e2
    pure $
      unlines
        [ lhsCode,
          rhsCode,
          "  ldr x1, [sp], 16",
          "  ldr x0, [sp], 16",
          "  " ++ opToInstr op ++ " x0, x0, x1",
          "  str x0, [sp, -16]!"
        ]
  Assign lhs rhs -> do
    rhsCode <- codegenExpr rhs
    st <- get
    let table = symTable st
    let varName = exprToLValue lhs
        offset = case lookup varName table of
          Just o -> o
          Nothing -> error $ "Variable not found: " ++ varName
    pure $
      unlines
        [ rhsCode,
          "  ldr x0, [sp]", -- peek top of stack (without popping result of rhs)
          "  str x0, [x29, #-" ++ show (offset * 16) ++ "]"
        ]
  _ -> error $ "Unsupported expression: " ++ show expr

exprToLValue :: Expr -> String -- Temporary helper that extracts lvalue
exprToLValue (VarExpr name) = name -- Should be done in semantic analysis stage
exprToLValue e = error $ "Invalid lvalue: " ++ show e

opToInstr :: Op -> String
opToInstr Add = "add"
opToInstr Sub = "sub"
opToInstr Mul = "mul"

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x
