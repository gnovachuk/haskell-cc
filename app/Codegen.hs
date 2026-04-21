module Codegen (emitProgram) where

import AST

type SymTable = [(String, Int)]

type LabelCount = Int

type CurrentLoop = Maybe Int

emitProgram :: Stmt -> String
emitProgram stmt =
  unlines
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
    ++ fst3 (codegenStmt [] 0 Nothing stmt)
    ++ unlines
      [ "; exit cleanly",
        "  ldp x29, x30, [sp], 16    ; restore frame & link register",
        "  mov x0,  #0  ; exit code 0",
        "  mov x16, #1  ; syscall number for exit on macOS", -- exit syscall
        "  svc #0x80    ; make the syscall"
      ]

codegenStmt :: SymTable -> LabelCount -> CurrentLoop -> Stmt -> (String, SymTable, LabelCount)
codegenStmt table labelCount currentLoop stmt = case stmt of
  Print expr ->
    let code =
          unlines
            [ codegenExpr table expr,
              "  ; expr result remains on stack (arg for printf)", -- pop final result
              "  adrp x0, fmt@PAGE ; load format string address",
              "  add x0, x0, fmt@PAGEOFF ; into x0 (1st arg)",
              "  bl _printf   ; call printf",
              "  add sp, sp, 16    ; cleanup stack (pop expr result)"
            ]
     in (code, table, labelCount)
  VarDecl name expr ->
    let offset = 1 + length table
        code =
          unlines
            [ codegenExpr table expr,
              "  ; store `" ++ name ++ "` at offset " ++ show offset ++ " (* 16)",
              "  ldr x0, [sp], 16",
              "  str x0, [x29, #-" ++ show (offset * 16) ++ "]",
              "  sub sp, sp, #16"
            ]
        table' = (name, offset) : table
     in (code, table', labelCount)
  Block stmts ->
    let (code, labelCount') = codegenStmts table labelCount currentLoop stmts
     in (code, table, labelCount') -- Restore original table (so symbols in inner block scope dissapear)
  If cond thenStmt elseStmt ->
    let (thenCode, _, labelCount') = codegenStmt table (labelCount + 1) currentLoop thenStmt
        (elseCode, _, labelCount'') = case elseStmt of
          Nothing -> ("", table, labelCount')
          Just elseStmt' -> codegenStmt table (labelCount' + 1) currentLoop elseStmt'
        code =
          unlines
            [ codegenExpr table cond,
              "  ldr x0, [sp], 16",
              "  cmp x0, #0      ; if condition false (== 0), jump to end/else (skip if stmt)",
              case elseStmt of
                Nothing -> "  beq endif_" ++ show labelCount
                Just _ -> "  beq else_" ++ show labelCount,
              thenCode, -- ignore symbol table, right?
              "  b endif_" ++ show labelCount, -- skip past ElseStmt (redundant when there's no else branch)
              case elseStmt of
                Nothing -> ""
                Just _ -> "else_" ++ show labelCount ++ ":\n" ++ elseCode,
              "endif_" ++ show labelCount ++ ":"
            ]
     in (code, table, labelCount'')
  While cond body ->
    let (bodyCode, _, labelCount') = codegenStmt table (labelCount + 1) (Just labelCount) body
        code =
          unlines
            [ "while_" ++ show labelCount ++ ":",
              codegenExpr table cond, -- eval cond
              "  ldr x0, [sp], 16", -- x0 <- condition expr result
              "  cmp x0, #0", -- compare x0 t0 0
              "  beq endwhile_" ++ show labelCount, -- if false, exit loop
              bodyCode,
              "  b while_" ++ show labelCount, -- continue loop
              "endwhile_" ++ show labelCount ++ ":"
            ]
     in (code, table, labelCount')
  Break ->
    let code =
          "  b endwhile_"
            ++ ( case currentLoop of
                   Just n -> show n
                   Nothing -> error "break outside of loop"
               )
            ++ "\n"
     in (code, table, labelCount)
  Continue ->
    let code =
          "  b while_"
            ++ ( case currentLoop of
                   Just n -> show n
                   Nothing -> error "continue outside of loop"
               )
            ++ "\n"
     in (code, table, labelCount)
  ExprStmt expr ->
    let code =
          unlines
            [ codegenExpr table expr,
              "  add sp, sp, 16    ; cleanup stack (pop expr result)"
            ]
     in (code, table, labelCount)

codegenStmts :: SymTable -> LabelCount -> CurrentLoop -> [Stmt] -> (String, LabelCount)
codegenStmts _ lc _ [] = ("", lc)
codegenStmts table labelCount currentLoop (stmt : rest) =
  let (code, table', labelCount') = codegenStmt table labelCount currentLoop stmt
      (restCode, labelCount'') = codegenStmts table' labelCount' currentLoop rest
   in (code ++ restCode, labelCount'')

codegenExpr :: SymTable -> Expr -> String
codegenExpr table expr = case expr of
  LitExpr n ->
    unlines
      [ "  mov x0, #" ++ show n,
        "  str x0, [sp, -16]!"
      ]
  VarExpr name ->
    let offset = case lookup name table of
          Just o -> o
          Nothing -> error $ "Variable not found: " ++ name
        code =
          unlines
            [ "  ldr x0, [x29, #-" ++ show (offset * 16) ++ "]",
              "  str x0, [sp, -16]!"
            ]
     in code
  BinOp op e1 e2 ->
    unlines
      [ codegenExpr table e1,
        codegenExpr table e2,
        "  ldr x1, [sp], 16",
        "  ldr x0, [sp], 16",
        "  " ++ opToInstr op ++ " x0, x0, x1",
        "  str x0, [sp, -16]!"
      ]
  Assign lhs rhs ->
    let varName = exprToLValue lhs
        offset = case lookup varName table of
          Just o -> o
          Nothing -> error $ "Variable not found: " ++ varName
        code =
          unlines
            [ codegenExpr table rhs,
              "  ldr x0, [sp]", -- peek top of stack (without popping result of rhs)
              "  str x0, [x29, #-" ++ show (offset * 16) ++ "]"
            ]
     in code

exprToLValue :: Expr -> String -- Temporary helper that extracts lvalue
exprToLValue (VarExpr name) = name -- Should be done in semantic analysis stage
exprToLValue e = error $ "Invalid lvalue: " ++ show e

opToInstr :: Op -> String
opToInstr Add = "add"
opToInstr Sub = "sub"
opToInstr Mul = "mul"

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x
