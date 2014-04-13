{-# LANGUAGE ViewPatterns #-}
module ConvertCalcAbsToLLVM where

import Abscalc
import Parcalc
import LLVM.General.AST
import LLVM.General.Module
import LLVM.General.Context
import qualified LLVM.General.AST.Global as G
import qualified LLVM.General.AST.Float as Fl
import qualified LLVM.General.AST.Constant as C
import Control.Monad.Trans.Error
import ErrM


lowestRankNumT :: Type
lowestRankNumT = FloatingPointType 64 IEEE   --- Integers

highestRankNumT :: Type
highestRankNumT = FloatingPointType 64 IEEE  --- Floats

getNumT :: Exp -> Type
getNumT (EInt _) = lowestRankNumT
getNumT (EDiv _ _) = highestRankNumT
getNumT (EAdd x y) = promoteNumType (getNumT x) (getNumT y)
getNumT (ESub x y) = promoteNumType (getNumT x) (getNumT y)
getNumT (EMul x y) = promoteNumType (getNumT x) (getNumT y)
getNumT _ = lowestRankNumT

isLowestRankNumT :: Type -> Bool
isLowestRankNumT x = x == lowestRankNumT

isHighestRankNumT :: Type -> Bool
isHighestRankNumT x = x == highestRankNumT

promoteNumType :: Type -> Type -> Type
promoteNumType (isLowestRankNumT -> True) (isLowestRankNumT -> True)  = lowestRankNumT
promoteNumType (isHighestRankNumT -> True) _                          = highestRankNumT
promoteNumType _                          (isHighestRankNumT -> True) = highestRankNumT
promoteNumType _ _ = FloatingPointType 64 IEEE

getBinOpInstruction :: Exp -> Type -> Operand -> Operand -> InstructionMetadata -> Instruction
getBinOpInstruction (EAdd _ _) (isHighestRankNumT -> True) = FAdd
getBinOpInstruction (EAdd _ _) _ = Add False False

getBinOpInstruction (ESub _ _) (isHighestRankNumT -> True) = FSub
getBinOpInstruction (ESub _ _) _ = Sub False False

getBinOpInstruction (EMul _ _) (isHighestRankNumT -> True) = FMul
getBinOpInstruction (EMul _ _) _ = Mul False False

getBinOpInstruction (EDiv _ _) _ = FDiv
getBinOpInstruction _ _  = Add False False

meta0 :: [a]
meta0 = []

binOpParams :: Exp -> Exp -> Type -> [Parameter]
binOpParams arg1 arg2 numT = getParms arg1 numT' ++ getParms arg2 numT' 
    where numT' = promoteNumType numT (promoteNumType (getNumT arg1) (getNumT arg2))

getParms :: Exp -> Type -> [Parameter]
getParms (EAdd arg1 arg2) numT = binOpParams arg1 arg2 numT
getParms (ESub arg1 arg2) numT = binOpParams arg1 arg2 numT
getParms (EMul arg1 arg2) numT = binOpParams arg1 arg2 numT
getParms (EDiv arg1 arg2) _    = binOpParams arg1 arg2 highestRankNumT
getParms (EVar (Ident x)) numT = [Parameter numT (Name x) []]
getParms (EInt _) _= []

----------- EAdd
getInsts retname _ ex@(EAdd arg1@(EVar (Ident x1)) arg2@(EVar (Ident x2))) =
                                  [ retname := getBinOpInstruction ex (getNumT ex) 
                                                     (LocalReference (Name x1)) 
                                                     (LocalReference (Name x2)) 
                                                     meta0
                                  ]
getInsts retname _ ex@(EAdd arg1@(EVar (Ident x1)) arg2@(EInt x2)) =
                                  [ retname := getBinOpInstruction ex (getNumT ex) 
                                                     (LocalReference (Name x1)) 
                                                     (ConstantOperand (C.Float $ Fl.Double (fromIntegral x2))) 
                                                     meta0
                                  ]
getInsts retname _ ex@(EAdd arg1@(EInt x2) arg2@(EVar (Ident x1)) ) =
                                  [ retname := getBinOpInstruction ex (getNumT ex) 
                                                     (ConstantOperand (C.Float $ Fl.Double (fromIntegral x2))) 
                                                     (LocalReference (Name x1)) 
                                                     meta0
                                  ]
getInsts retname num ex@(EAdd arg1@(EInt x1) arg2 ) = getInsts (UnName num) (num+1) arg2 ++
                                  [ retname := getBinOpInstruction ex (getNumT ex) 
                                                     (ConstantOperand (C.Float $ Fl.Double (fromIntegral x1))) 
                                                     (LocalReference (UnName num)) 
                                                     meta0
                                  ]
getInsts retname num ex@(EAdd arg1 arg2@(EInt x2) ) = getInsts (UnName num) (num+1) arg1 ++
                                  [ retname := getBinOpInstruction ex (getNumT ex) 
                                                     (ConstantOperand (C.Float $ Fl.Double (fromIntegral x2))) 
                                                     (LocalReference (UnName num)) 
                                                     meta0
                                  ]
----------- ESub
getInsts retname _ ex@(ESub arg1@(EVar (Ident x1)) arg2@(EVar (Ident x2))) =
                                  [ retname := getBinOpInstruction ex (getNumT ex) 
                                                     (LocalReference (Name x1)) 
                                                     (LocalReference (Name x2)) 
                                                     meta0
                                  ]
getInsts retname _ ex@(ESub arg1@(EVar (Ident x1)) arg2@(EInt x2)) =
                                  [ retname := getBinOpInstruction ex (getNumT ex) 
                                                     (LocalReference (Name x1)) 
                                                     (ConstantOperand (C.Float $ Fl.Double (fromIntegral x2))) 
                                                     meta0
                                  ]
getInsts retname _ ex@(ESub arg1@(EInt x2) arg2@(EVar (Ident x1)) ) =
                                  [ retname := getBinOpInstruction ex (getNumT ex) 
                                                     (ConstantOperand (C.Float $ Fl.Double (fromIntegral x2))) 
                                                     (LocalReference (Name x1)) 
                                                     meta0
                                  ]
getInsts retname num ex@(ESub arg1@(EInt x1) arg2 ) = getInsts (UnName num) (num+1) arg2 ++
                                  [ retname := getBinOpInstruction ex (getNumT ex) 
                                                     (ConstantOperand (C.Float $ Fl.Double (fromIntegral x1))) 
                                                     (LocalReference (UnName num)) 
                                                     meta0
                                  ]
getInsts retname num ex@(ESub arg1 arg2@(EInt x2) ) = getInsts (UnName num) (num+1) arg1 ++
                                  [ retname := getBinOpInstruction ex (getNumT ex) 
                                                     (ConstantOperand (C.Float $ Fl.Double (fromIntegral x2))) 
                                                     (LocalReference (UnName num)) 
                                                     meta0
                                  ]
----------- EMul
getInsts retname _ ex@(EMul arg1@(EVar (Ident x1)) arg2@(EVar (Ident x2))) =
                                  [ retname := getBinOpInstruction ex (getNumT ex) 
                                                     (LocalReference (Name x1)) 
                                                     (LocalReference (Name x2)) 
                                                     meta0
                                  ]
getInsts retname _ ex@(EMul arg1@(EVar (Ident x1)) arg2@(EInt x2)) =
                                  [ retname := getBinOpInstruction ex (getNumT ex) 
                                                     (LocalReference (Name x1)) 
                                                     (ConstantOperand (C.Float $ Fl.Double (fromIntegral x2))) 
                                                     meta0
                                  ]
getInsts retname _ ex@(EMul arg1@(EInt x2) arg2@(EVar (Ident x1)) ) =
                                  [ retname := getBinOpInstruction ex (getNumT ex) 
                                                     (ConstantOperand (C.Float $ Fl.Double (fromIntegral x2))) 
                                                     (LocalReference (Name x1)) 
                                                     meta0
                                  ]
getInsts retname num ex@(EMul arg1@(EInt x1) arg2 ) = getInsts (UnName num) (num+1) arg2 ++
                                  [ retname := getBinOpInstruction ex (getNumT ex) 
                                                     (ConstantOperand (C.Float $ Fl.Double (fromIntegral x1))) 
                                                     (LocalReference (UnName num)) 
                                                     meta0
                                  ]
getInsts retname num ex@(EMul arg1 arg2@(EInt x2) ) = getInsts (UnName num) (num+1) arg1 ++
                                  [ retname := getBinOpInstruction ex (getNumT ex) 
                                                     (ConstantOperand (C.Float $ Fl.Double (fromIntegral x2))) 
                                                     (LocalReference (UnName num)) 
                                                     meta0
                                  ]
----------- EDiv
getInsts retname _ ex@(EDiv arg1@(EVar (Ident x1)) arg2@(EVar (Ident x2))) =
                                  [ retname := getBinOpInstruction ex (getNumT ex) 
                                                     (LocalReference (Name x1)) 
                                                     (LocalReference (Name x2)) 
                                                     meta0
                                  ]
getInsts retname _ ex@(EDiv arg1@(EVar (Ident x1)) arg2@(EInt x2)) =
                                  [ retname := getBinOpInstruction ex (getNumT ex) 
                                                     (LocalReference (Name x1)) 
                                                     (ConstantOperand (C.Float $ Fl.Double (fromIntegral x2))) 
                                                     meta0
                                  ]
getInsts retname _ ex@(EDiv arg1@(EInt x2) arg2@(EVar (Ident x1)) ) =
                                  [ retname := getBinOpInstruction ex (getNumT ex) 
                                                     (ConstantOperand (C.Float $ Fl.Double (fromIntegral x2))) 
                                                     (LocalReference (Name x1)) 
                                                     meta0
                                  ]
getInsts retname num ex@(EDiv arg1@(EInt x1) arg2 ) = getInsts (UnName num) (num+1) arg2 ++
                                  [ retname := getBinOpInstruction ex (getNumT ex) 
                                                     (ConstantOperand (C.Float $ Fl.Double (fromIntegral x1))) 
                                                     (LocalReference (UnName num)) 
                                                     meta0
                                  ]
getInsts retname num ex@(EDiv arg1 arg2@(EInt x2) ) = getInsts (UnName num) (num+1) arg1 ++
                                  [ retname := getBinOpInstruction ex (getNumT ex) 
                                                     (ConstantOperand (C.Float $ Fl.Double (fromIntegral x2))) 
                                                     (LocalReference (UnName num)) 
                                                     meta0
                                  ]
main :: IO ()
main  = do 
    let Ok e = pExp $ myLexer "x + y"
        Ok dd= pExp $ myLexer str
        str = "((2 + (4 * z)) + 3)/7"
        dd' = getInsts (Name "dd") 0 dd
        ddTop = getModule [dd]
    print $ getInsts (Name "cc") 0 e
    putStrLn " ---------- "
    putStrLn str
    putStrLn " ---------- "
    mapM_ print $ dd'
    putStrLn " ---------- "
    dumpLLVMAsm ddTop

getModule :: [Exp] -> LLVM.General.AST.Module
getModule exps = defaultModule { moduleName = "Calc Module", moduleDefinitions = map (GlobalDefinition . getTopLvl) exps}
    where
        getTopLvl :: Exp -> Global 
        getTopLvl ex = let numT = getNumT ex in
                    functionDefaults {
                        G.returnType = numT,
                        G.name = Name "func_name",
                        G.parameters = (getParms ex numT, False),
                        G.basicBlocks = [ BasicBlock (Name "my_add") 
                                                  (getInsts (Name "retval") 0 ex)
                                                  (Do Ret {returnOperand = Just (LocalReference $ Name "retval"), metadata'=meta0})] }

dumpLLVMAsm :: LLVM.General.AST.Module -> IO ()
dumpLLVMAsm mymod = do
    r <- withContext (\ctx -> runErrorT $ withModuleFromAST ctx  mymod moduleLLVMAssembly)
    report r

report :: Show a => Either a String -> IO ()
report (Right s) = putStrLn s
report (Left x) = putStrLn $ "ERROR: " ++ show x

