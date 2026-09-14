module HCC.Assembly
  ( assemble
  ) where

import qualified HCC.AST as AST

assembleExpression :: AST.Expression -> String
assembleExpression (AST.IntegerConstant n) = "  movl $" ++ (show n) ++ ", %eax\n"
assembleExpression (AST.Negation expr) = (assembleExpression expr) ++ "  neg %eax\n"
assembleExpression (AST.BitwiseCompliment expr) = (assembleExpression expr) ++ "  not %eax\n"
assembleExpression (AST.LogicalNegation expr) =
  (assembleExpression expr)
    ++ "  cmpl $0, %eax\n"
    ++ "  movl $0, %eax\n"
    ++ "  sete %al\n"

assembleStatement :: AST.Statement -> String
assembleStatement (AST.CReturn expr) = (assembleExpression expr) ++ "  ret\n"

assembleFunction :: AST.Function -> String
assembleFunction (AST.Function idnt stmt) =
  ".globl "
    ++ idnt
    ++ "\n"
    ++ idnt
    ++ ": \n"
    ++ (assembleStatement stmt)

assembleProgram :: AST.Program -> String
assembleProgram program = assembleFunction $ AST.function program

assemble :: AST.Program -> String
assemble program = assembleProgram program
