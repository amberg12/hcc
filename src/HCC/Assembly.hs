module HCC.Assembly
  ( assemble
  ) where

import Control.Monad.State
import Data.List (find)
import qualified HCC.IR as IR

data Program = Program Function

data Function = Function (String, Integer, [Instruction])

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
 where

resolvePseudoOperand (other) = pure other

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
assembleFunction (IR.Function (name, instructions)) = Function (name, stackSize, pass')
 where
  pass = concat $ map assembleInstruction instructions
  (stackSize, pass') = resolvePseudo pass

assembleProgram :: IR.Program -> Program
assembleProgram (IR.Program (function)) = Program $ assembleFunction function

assemble :: IR.Program -> String
assemble program = undefined
