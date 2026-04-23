{-# LANGUAGE InstanceSigs #-}

{- HLINT ignore "Use if" -}

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
    currentLoop :: CurrentLoop,
    funcTable :: [String]
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

emitProgram :: [Decl] -> String
emitProgram decls =
  let initState = CodegenState {symTable = [], labelCount = 0, currentLoop = Nothing, funcTable = []}
   in unlines
        [ ".data",
          "fmt:",
          "  .asciz \"%d\\n\"",
          "",
          ".text",
          ".global _main ; tell the linker `_main` exists and is visible",
          ".align 2      ; align code to 4-byte boundary (ARM64 requirement)",
          evalState (codegenProgram decls) initState
        ]

codegenProgram :: [Decl] -> State CodegenState String
codegenProgram [] = pure ""
codegenProgram (decl : rest) = do
  code <- codegenDecl decl
  restCode <- codegenProgram rest
  pure $ code ++ "\n" ++ restCode

codegenDecl :: Decl -> State CodegenState String
codegenDecl decl = case decl of
  FuncDecl name params body -> do
    modify (\s -> s {funcTable = name : funcTable s})
    prevState <- get
    modify (\s -> s {symTable = [], currentLoop = Nothing}) -- reset symTable and currentLoop for function scope
    paramCode <- codegenParamLoad params
    bodyCode <- codegenStmt body
    newLc <- labelCount <$> get -- labelCount should persist between declarations
    put prevState {labelCount = newLc}
    -- x29 = frame pointer (like rbp on x86)
    -- x30 = link register (return addr after function ends)
    pure $
      unlines
        [ case name of
            "main" -> "_main: "
            _ -> "func_" ++ name ++ ":",
          "  stp x29, x30, [sp, -16]!    ; push old frame and link register onto stack",
          "  mov x29, sp                 ; x29 -> base of stack frame",
          "  ; copy args from registers to stack frame",
          paramCode, -- TODO: when there are more than 8 params, rest should be loaded from stack
          "  sub sp, sp, #" ++ show (length params * 16),
          bodyCode,
          "  mov sp, x29", -- deallocate locals, sp back to saved x29/x30
          "  ldp x29, x30, [sp], 16    ; restore frame & link register",
          "  mov x0, #69", -- test return value of 69
          case name of
            "main" ->
              unlines -- note x0, (ret value) contains exit code used for syscall;
                [ "  mov x16, #1  ; syscall number for exit on macOS", -- exit syscall
                  "  svc #0x80    ; make the syscall"
                ]
            _ -> "  ret"
        ]

codegenParamLoad :: [String] -> State CodegenState String
codegenParamLoad [] = pure ""
codegenParamLoad (param : rest) = do
  st <- get
  let offset = 1 + length (symTable st)
  modify (\s -> s {symTable = (param, offset) : symTable s})
  -- offset 1 corresponds to first arg (stored in x0), thus, (offset - 1) is used for register number.
  let code = "  str x" ++ show (offset - 1) ++ ", [x29, #-" ++ show (16 * offset) ++ "]  ; load onto stack: " ++ param
  restCode <- codegenParamLoad rest
  pure (code ++ restCode)

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
    modify (\s -> s {symTable = (name, offset) : symTable s})
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
    case name `elem` funcTable st of
      True -> do
        let funcName = case name of
              "main" -> "_main"
              _ -> "func_" ++ name
        -- if function, emit func addr on stack
        pure $
          unlines
            [ "  adrp x16, " ++ funcName ++ "@PAGE",
              "  add x16, x16, " ++ funcName ++ "@PAGEOFF",
              "  str x16, [sp, -16]!"
            ]
      False -> do
        let offset = case lookup name (symTable st) of
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
  Call func args -> do
    argLoadCode <- codegenArgLoad (length args)
    funcCode <- codegenExpr func
    argsCode <- mapM codegenExpr args
    pure $
      unlines
        [ unlines argsCode,
          funcCode,
          "  ldr x16, [sp], 16", -- load func addr into x16
          argLoadCode, -- load args into x0..x(argCount-1) (fails if more than 8 args)
          "  blr x16", -- branch & link (stores ret addr into x30)
          "  str x0, [sp, -16]!" -- retrieve ret value (produced from func) and push onto stack
        ]

-- Pops argCount values off the stack into x0..x(argCount-1).
-- The top of stack is the last arg, so we pop from the highest register down to x0.
codegenArgLoad :: Int -> State CodegenState String
codegenArgLoad 0 = pure ""
codegenArgLoad argCount = do
  -- Pop the top of stack (the last-pushed arg) into the highest register first
  let regNum = argCount - 1
  restCode <- codegenArgLoad regNum
  pure $
    unlines
      [ "  ldr x" ++ show regNum ++ ", [sp], 16",
        restCode
      ]

exprToLValue :: Expr -> String -- Temporary helper that extracts lvalue
exprToLValue (VarExpr name) = name -- Should be done in semantic analysis stage
exprToLValue e = error $ "Invalid lvalue: " ++ show e

opToInstr :: Op -> String
opToInstr Add = "add"
opToInstr Sub = "sub"
opToInstr Mul = "mul"
