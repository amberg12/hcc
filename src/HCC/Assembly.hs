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

assembleStatement :: Statement -> String
assembleStatement (Return expr) = (assembleExpression expr) ++ "  ret\n"

assembleFunction :: Function -> String
assembleFunction (Function "main" stmt) = assembleFunction (Function "_main" stmt)
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
