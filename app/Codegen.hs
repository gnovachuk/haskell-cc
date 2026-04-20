module Codegen (emitProgram) where

import AST

type SymTable = [(String, Int)]

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
    ++ fst (codegenStmt [] stmt)
    ++ unlines
      [ "; exit cleanly",
        "  ldp x29, x30, [sp], 16    ; restore frame & link register",
        "  mov x0,  #0  ; exit code 0",
        "  mov x16, #1  ; syscall number for exit on macOS", -- exit syscall
        "  svc #0x80    ; make the syscall"
      ]

codegenStmt :: SymTable -> Stmt -> (String, SymTable)
codegenStmt table stmt = case stmt of
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
     in (code, table)
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
     in (code, table')
  Block stmts ->
    let code = codegenStmts table stmts
     in (code, table) -- Restore original table (so symbols in inner block scope dissapear)
  _ -> error "Unsupported statement"

codegenStmts :: SymTable -> [Stmt] -> String
codegenStmts _ [] = ""
codegenStmts table (stmt : rest) =
  let (code, table') = codegenStmt table stmt
   in code ++ codegenStmts table' rest

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

opToInstr :: Op -> String
opToInstr Add = "add"
opToInstr Mul = "mul"
