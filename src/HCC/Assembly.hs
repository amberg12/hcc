module HCC.Assembly
  ( assemble
  ) where

import Control.Monad.State
import Data.List (find)
import qualified HCC.IR as IR

data Program = Program Function deriving (Show)

data Function = Function (String, [Instruction]) deriving (Show)

data Instruction
  = Mov (Operand, Operand)
  | UnaryNegation Operand
  | UnaryLogicalNegation Operand
  | UnaryBitwiseCompliment Operand
  | AllocateStack Integer
  | Ret
  deriving (Show)

data Operand
  = Imm Integer
  | Reg Register
  | Pseudo String
  | Stack Integer
  deriving (Show, Eq)

data Register
  = AX
  | R10
  deriving (Show, Eq)

data PseudoStack = PseudoStack
  { stackPositions :: [(String, Integer)]
  , maxStackSize :: Integer
  }

type PseudoStackState a = State PseudoStack a

resolvePseudoOperand :: Operand -> PseudoStackState Operand
resolvePseudoOperand (Pseudo identifier) = do
  PseudoStack map stackSize <- get
  case find ((==) identifier . fst) map of
    Just (_, stackOffset) -> pure $ Stack stackOffset
    Nothing -> do
      let newStackSize = stackSize + 4
      put $ PseudoStack ([(identifier, newStackSize)] ++ map) newStackSize
      pure $ Stack newStackSize
resolvePseudoOperand (other) = pure other

resolveInvalidMov :: Instruction -> [Instruction]
resolveInvalidMov (Mov (Stack src, Stack dst)) =
  [Mov (Stack src, Reg R10), Mov (Reg R10, Stack dst)]
resolveInvalidMov other = [other]

resolvePseudoInstruction :: Instruction -> PseudoStackState Instruction
resolvePseudoInstruction (Mov (src, dst)) = do
  src' <- resolvePseudoOperand src
  dst' <- resolvePseudoOperand dst
  pure $ Mov (src', dst')
resolvePseudoInstruction (UnaryNegation src) = do
  src' <- resolvePseudoOperand src
  pure $ UnaryNegation src'
resolvePseudoInstruction (UnaryLogicalNegation src) = do
  src' <- resolvePseudoOperand src
  pure $ UnaryLogicalNegation src'
resolvePseudoInstruction (UnaryBitwiseCompliment src) = do
  src' <- resolvePseudoOperand src
  pure $ UnaryBitwiseCompliment src'
resolvePseudoInstruction (AllocateStack n) =
  pure $ AllocateStack n
resolvePseudoInstruction Ret =
  pure Ret

resolvePseudo :: [Instruction] -> (Integer, [Instruction])
resolvePseudo instructions =
  (maxStackSize state, instructions')
 where
  (instructions', state) =
    runState
      (mapM resolvePseudoInstruction instructions)
      (PseudoStack [] 0)

assembleValue :: IR.Value -> Operand
assembleValue (IR.Var (IR.Variable identifier)) = Pseudo identifier
assembleValue (IR.Const (IR.Constant n)) = Imm n

assembleInstruction :: IR.Instruction -> [Instruction]
assembleInstruction (IR.Return src) =
  [Mov (aSrc, Reg AX), Ret]
 where
  aSrc = assembleValue src
assembleInstruction (IR.UnaryNegation (src, dst)) =
  [Mov (aSrc, aDst), UnaryNegation (aDst)]
 where
  aSrc = assembleValue src
  aDst = assembleValue dst
assembleInstruction (IR.UnaryBitwiseCompliment (src, dst)) =
  [Mov (aSrc, aDst), UnaryBitwiseCompliment (aDst)]
 where
  aSrc = assembleValue src
  aDst = assembleValue dst
assembleInstruction (IR.UnaryLogicalNegation (src, dst)) =
  [Mov (aSrc, aDst), UnaryLogicalNegation (aDst)]
 where
  aSrc = assembleValue src
  aDst = assembleValue dst

assembleFunction :: IR.Function -> Function
assembleFunction (IR.Function (name, instructions)) = Function (name, pass''')
 where
  pass = concat $ map assembleInstruction instructions
  (stackSize, pass') = resolvePseudo pass
  pass'' = concat $ map resolveInvalidMov pass'
  pass''' = [AllocateStack stackSize] ++ pass''

assembleProgram :: IR.Program -> Program
assembleProgram (IR.Program (function)) = Program $ assembleFunction function

emitOperand :: Operand -> String
emitOperand (Imm n) = "$" ++ (show n)
emitOperand (Reg AX) = "%eax"
emitOperand (Reg R10) = "%r10d"
emitOperand (Stack offset) = "-" ++ (show offset) ++ "(%rbp)"

emitInstruction :: Instruction -> String
emitInstruction (Mov (src, dst)) = "  movl " ++ emitOperand src ++ ", " ++ emitOperand dst ++ "\n"
emitInstruction (UnaryNegation src) = "  negl " ++ emitOperand src ++ "\n"
emitInstruction (UnaryLogicalNegation src) =
  "  cmpl $0 %eax\n"
    ++ "  movl $0 %eax\n"
    ++ "  sete %al\n"
emitInstruction (UnaryBitwiseCompliment src) = "  notl " ++ emitOperand src ++ "\n"
emitInstruction (AllocateStack n) = "  subq $" ++ show n ++ ", %rsp\n"
emitInstruction (Ret) =
  "  movq %rbp, %rsp\n"
    ++ "  popq %rbp\n"
    ++ "  ret\n"

emitFunction :: Function -> String
emitFunction (Function (identifier, instructions)) =
  "  .globl main\n"
    ++ "main:\n"
    ++ "  pushq %rbp\n"
    ++ "  movq %rsp, %rbp\n"
    ++ concat (map emitInstruction instructions)

emitProgram :: Program -> String
emitProgram (Program function) =
  (emitFunction function) ++ ".section .note.GNU-stack,\"\",@progbits\n"

assemble :: IR.Program -> String
assemble program = emitProgram $ assembleProgram program
