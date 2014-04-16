{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}
module Lang.LLVMInstructionCalc where

import Lang.AbsCalc
import Lang.ErrM
import qualified Lang.TypifyCalc as TY
import Lang.TypifyCalc (getTyp)

import LLVM.General.AST
import LLVM.General.AST.Instruction
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
--   adding a specified amount to each n of an (UnName n). 
--   Also any conflicts with rName are remedied and the
--   final instruction is named rName
--
--      rName        -    requested name of final instruction
--      unNameOffset -    highest used digit for unnamed instructions
--                        (or the amount to add to n)
--      ins          -    List of instructions to update
--
{-# ANN updateNames "HLint: ignore Eta reduce" #-}
updateNames :: Name -> Word -> [Named Instruction] -> [Named Instruction] 
updateNames rName unNameOffset [] = []
updateNames rName unNameOffset ins = update' ins 
   where
       update' [ _ := instruction ] = [ appropriate rName := updateOperands instruction ] -- singleton list
       update' ((x := i):is)  = (updateName x := updateOperands i):update' is

       -- approprate final instruction name
       -- if rName is an UnName then ignore it
       appropriate x | UnName _ <- rName = updateName x
       appropriate x@(UnName n) = updateName x -- UnName (n+unNameOffset)
       appropriate x = rName

       updateName :: Name -> Name
       updateName x@(Name s) = if rName == x then Name (s ++ "0") else x
       updateName (UnName n)  = UnName (n + unNameOffset)

       updateOperands = updateOperand1 . updateOperand0

       updateOperand0 inst | LocalReference x  <- operand0 inst 
            = inst { operand0=LocalReference (updateName x) }
       updateOperand0 inst = inst

       updateOperand1 inst | (LocalReference x) <- operand1 inst
            = inst { operand1=LocalReference (updateName x) }
       updateOperand1 inst = inst


failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident str  -> failure x

data BinOP = BAdd | BSub | BMul | BDiv | BNone deriving (Eq,Ord,Show)

getBinOP x = case x of
  EPos pos n typ exp  -> getBinOP exp
  EAs typ exp  -> getBinOP exp
  EAdd {}  -> BAdd
  ESub {}  -> BSub
  EMul {}  -> BMul
  EDiv {}  -> BDiv
  _        -> BNone

allbutlast xs@(length -> n) = take (n-1) xs

getBinOpInstruction :: Exp -> Operand -> Operand -> InstructionMetadata -> Instruction
getBinOpInstruction x = case (getTyp x, getBinOP x) of
    (TI32,BAdd) -> Add False False
    (TFlo,BAdd) -> FAdd
    (TI32,BSub) -> Sub False False
    (TFlo,BSub) -> FSub
    (TI32,BMul) -> Mul False False
    (TFlo,BMul) -> FMul
    (_,   BDiv) -> FDiv
    -- Using Add 0 for storing Terminals as instructions
    (TI32,BNone) -> Add False False
    (TFlo,BNone) -> FAdd
    (_,_ ) -> FAdd
    
zeroConstant TI32 = ConstantOperand (C.Int 32 0)
zeroConstant  _   = ConstantOperand (C.Float $ Fl.Double 0)

getTerminal :: Exp -> Err Operand
getTerminal (EPos _ _ typ x) = getTerminal (EAs typ x)
getTerminal (EAs TFlo (EInt n) ) = getTerminal (EFlo (fromIntegral n))
getTerminal (EAs typ x) = getTerminal x


getTerminal (EVar (Ident x)) = return $ LocalReference (Name x)
getTerminal (EInt n) = return $ ConstantOperand (C.Int 32 n)
getTerminal (EFlo n) = return $ ConstantOperand (C.Float $ Fl.Double n)
getTerminal x = error ("Expected terminal, syntax:" ++ show x)

getFirstParam :: Exp -> Err Exp
getFirstParam (EPos _ _ typ x) = getFirstParam (EAs typ x)
getFirstParam (EAs typ x) = getFirstParam x
getFirstParam x 
 | EAdd exp _ <- x = return exp
 | ESub exp _ <- x = return exp
 | EMul exp _ <- x = return exp
 | EDiv exp _ <- x = return exp
getFirstParam x = error ("Could not get first parameter, syntax: " ++ show x)

getSecondParam (EPos _ _ typ x) = getSecondParam (EAs typ x)
getSecondParam (EAs typ x) = getSecondParam x
getSecondParam x 
 | EAdd _ exp  <- x = return exp
 | ESub _ exp  <- x = return exp
 | EMul _ exp  <- x = return exp
 | EDiv _ exp  <- x = return exp
getSecondParam x = error ("Could not get second parameter, syntax: " ++ show x)


transExp :: Exp -> Result
transExp x = case (getTyp x, getBinOP x) of
    (typ, BNone) -> getTerminal x >>= \op -> return [UnName 0 := getBinOpInstruction x op (zeroConstant typ) [] ]
    (typ, _    ) -> do
        exp1 <- fmap (EAs typ) $ getFirstParam x
        exp2 <- fmap (EAs typ) $ getSecondParam x
        xs <- transExp exp1
        let lastn = fromIntegral $ length xs 
        ys <- transExp exp2 -- fmap (updateNames (UnName 0) lastn) (transExp exp2)
        let x1 := i1 = last xs
            y1 := i2 = last ys
            newname = Name (tail . show $ getBinOP x) --UnName n3
            xname = Name (tail . show $ getBinOP exp1) --UnName n3
            yname = Name (tail . show $ getBinOP exp2) --UnName n3
            (xs',op0') = if getBinOP exp1 == BNone then ([], getTerminal exp1)
                                                   else (xs, return $ LocalReference x1)
            (ys',op1') = if getBinOP exp2 == BNone then ([],  getTerminal exp2)
                                                   else (ys, return $ LocalReference y1)
        op0 <- op0'
        op1 <- op1'
        return $ updateNames xname 0 xs' ++ updateNames yname lastn ys' ++ [newname := getBinOpInstruction x op0 op1 []]


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



