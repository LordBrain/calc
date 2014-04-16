{-# LANGUAGE PatternGuards #-}
module Lang.LLVMInstructionCalc where

import Lang.AbsCalc
import Lang.ErrM
import LLVM.General.AST
import LLVM.General.AST.Instruction
--import LLVM.General.AST.Instruction ((:=))
import LLVM.General.Module
import LLVM.General.Context
import qualified LLVM.General.AST.Global as G
import qualified LLVM.General.AST.Float as Fl
import qualified LLVM.General.AST.Constant as C

import Data.Word

type Result = Err [Named Instruction]

-- | updateNames
--
--   updateNames iterates over the list of instructions
--   adding a specified amount to all of the unnames.
--   Also any conflicts with rName are remedied and the
--   final instruction is named rName
--
--      rName      -    requested name of final instruction
--      unNameOffset -    Highest used digit for unnamed instructions
--      ins        -    List of instructions to update
updateNames :: Name -> Word -> [Named Instruction] -> [Named Instruction] 
updateNames rName@(Name s) unNameOffset ins = update' ins 
   where
       update' [ _ := inst ] = [ rName := updateOperands inst] -- singleton list
       update' ((UnName n) := i :xs) = (UnName (n+unNameOffset) := updateOperands i):update' xs
       update' ((UnName n) := i :xs) = (UnName (n+unNameOffset) := updateOperands i):update' xs
       update' ((x := i):ins) | x == rName = (Name (s ++"0") := updateOperands i):update' ins
       update' x = x

       updateOperands = updateOperand1 . updateOperand0
       updateOperand0 inst | (LocalReference (UnName n)) <- operand0 inst 
            = inst { operand0=LocalReference (UnName (n+unNameOffset))}
       updateOperand0 inst | (LocalReference x) <- operand0 inst, x == rName
            = inst { operand0=LocalReference (Name (s ++ "0"))}
       updateOperand0 inst = inst

       updateOperand1 inst | (LocalReference (UnName n)) <- operand1 inst 
            = inst { operand1=LocalReference (UnName (n+unNameOffset))}
       updateOperand1 inst | (LocalReference x) <- operand1 inst, x == rName
            = inst { operand1=LocalReference (Name (s ++ "0"))}
       updateOperand1 inst = inst


failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident str  -> failure x


transExp :: Exp -> Result
transExp x = case x of
  EPos pos n typ exp  -> failure x
  EAs typ exp  -> failure x
  EAdd exp1 exp2  -> failure x
  ESub exp1 exp2  -> failure x
  EMul exp1 exp2  -> failure x
  EDiv exp1 exp2  -> failure x
  EInt n  -> failure x
  EFlo d  -> failure x
  EVar id  -> failure x


transTyp :: Typ -> Result
transTyp x = case x of
  TUnk  -> failure x
  TI32  -> failure x
  TFlo  -> failure x


transPos :: Pos -> Result
transPos x = case x of
  PLin  -> failure x
  PCol  -> failure x
  PUnk  -> failure x



