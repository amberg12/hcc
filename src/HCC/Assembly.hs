module HCC.Assembly
  ( assemble
  ) where

import HCC.Parser
  ( Expression (..)
  , Function (..)
  , Program (..)
  , Statement (..)
  )

assembleExpression :: Expression -> String
assembleExpression (IntegerConstant n) = "  movl $" ++ (show n) ++ ", %eax\n"
assembleExpression (Negation expr) = (assembleExpression expr) ++ "  neg %eax\n"
assembleExpression (BitwiseCompliment expr) = (assembleExpression expr) ++ "  not %eax\n"
assembleExpression (LogicalNegation expr) =
  (assembleExpression expr)
    ++ "  cmpl $0, %eax\n"
    ++ "  movl $0, %eax\n"
    ++ "  sete %al\n"

assembleStatement :: Statement -> String
assembleStatement (Return expr) = (assembleExpression expr) ++ "  ret\n"

assembleFunction :: Function -> String
assembleFunction (Function idnt stmt) =
  ".globl "
    ++ idnt
    ++ "\n"
    ++ idnt
    ++ ": \n"
    ++ (assembleStatement stmt)

assembleProgram :: Program -> String
assembleProgram program = assembleFunction $ function program

assemble :: Program -> String
assemble program = assembleProgram program
