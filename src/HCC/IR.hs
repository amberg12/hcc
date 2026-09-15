module HCC.IR
  ( Program (..)
  , Function (..)
  , Constant (..)
  , Variable (..)
  , Value (..)
  , Instruction (..)
  , emitIR
  ) where

import Control.Monad.State
import qualified HCC.AST as AST

newtype Constant = Constant Integer deriving (Show)
newtype Variable = Variable String deriving (Show)

data Program = Program Function
  deriving (Show)

data Function = Function (String, [Instruction])
  deriving (Show)

data Value
  = Var Variable
  | Const Constant
  deriving (Show)

data Instruction
  = Return Value
  | UnaryNegation (Value, Value)
  | UnaryBitwiseCompliment (Value, Value)
  | UnaryLogicalNegation (Value, Value)
  | Addition (Value, Value, Value)
  | Multiplication (Value, Value, Value)
  deriving (Show)

data GeneratorState = GeneratorState
  { nextTemp :: Integer
  , instructions :: [Instruction]
  }

type Generator a = State GeneratorState a

tempVariable :: Generator Variable
tempVariable = state $ \s ->
  let n = nextTemp s
   in (Variable ("_Temp_" ++ show n), s {nextTemp = n + 1})

addInstruction :: Instruction -> Generator ()
addInstruction i = modify $ \s -> (s {instructions = (instructions s) ++ [i]})

emitIRStatement :: AST.Statement -> Generator ()
emitIRStatement (AST.CReturn expr) = do
  src <- emitIRExpression expr
  addInstruction $ Return src

emitIRExpression :: AST.Expression -> Generator Value
emitIRExpression (AST.IntegerConstant i) = do
  pure (Const $ Constant i)
emitIRExpression (AST.Negation expr) = do
  src <- emitIRExpression expr
  dst <- tempVariable
  addInstruction $ UnaryNegation (src, Var dst)
  pure (Var dst)
emitIRExpression (AST.BitwiseCompliment expr) = do
  src <- emitIRExpression expr
  dst <- tempVariable
  addInstruction $ UnaryBitwiseCompliment (src, Var dst)
  pure (Var dst)
emitIRExpression (AST.LogicalNegation expr) = do
  src <- emitIRExpression expr
  dst <- tempVariable
  addInstruction $ UnaryLogicalNegation (src, Var dst)
  pure (Var dst)
emitIRExpression (AST.Addition (expr, expr')) = do
  src <- emitIRExpression expr
  src' <- emitIRExpression expr'
  dst <- tempVariable
  addInstruction $ Addition (src, src', Var dst)
  pure $ Var dst
emitIRExpression (AST.Multiplication (expr, expr')) = do
  src <- emitIRExpression expr
  src' <- emitIRExpression expr'
  dst <- tempVariable
  addInstruction $ Multiplication (src, src', Var dst)
  pure $ Var dst

emitFunction :: AST.Function -> Function
emitFunction (AST.Function name body) = Function (name, instructions s)
 where
  s = execState (emitIRStatement body) $ GeneratorState {nextTemp = 0, instructions = []}

emitProgram :: AST.Program -> Program
emitProgram p = Program $ emitFunction $ AST.function p

emitIR :: AST.Program -> Program
emitIR = emitProgram
