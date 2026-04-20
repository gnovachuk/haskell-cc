{-# LANGUAGE LambdaCase #-}

module Codegen (emitProgram) where

import AST

emitProgram :: Expr -> String
emitProgram e =
  unlines
    [ ".data",
      "fmt:",
      "  .asciz \"%d\\n\"",
      "",
      ".text",
      ".global _main ; tell the linker `_main` exists and is visible",
      ".align 2      ; align code to 4-byte boundary (ARM64 requirement)",
      "_main:        ; the entry point label"
    ]
    ++ codegen e
    ++ unlines
      [ "  ; expr result remains on stack (arg for printf)", -- pop final result
        "  adrp x0, fmt@PAGE ; load format string address",
        "  add x0, x0, fmt@PAGEOFF ; into x0 (1st arg)",
        "  bl _printf   ; call printf",
        "  add sp, sp, 16    ; cleanup stack (pop expr result)",
        "",
        "; exit cleanly",
        "  mov x0,  #0  ; exit code 0",
        "  mov x16, #1  ; syscall number for exit on macOS", -- exit syscall
        "  svc #0x80    ; make the syscall"
      ]

codegen :: Expr -> String
codegen =
  \case
    LitExpr n ->
      unlines
        [ "  mov x0, #" ++ show n,
          "  str x0, [sp, -16]!"
        ]
    BinOp op e1 e2 ->
      unlines
        [ codegen e1,
          codegen e2,
          "  ldr x1, [sp], 16",
          "  ldr x0, [sp], 16",
          "  " ++ opToInstr op ++ " x0, x0, x1",
          "  str x0, [sp, -16]!"
        ]
    _ -> error "Unsupported expression"

opToInstr :: Op -> String
opToInstr Add = "add"
opToInstr Mul = "mul"
