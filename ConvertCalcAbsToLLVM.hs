{-# LANGUAGE ViewPatterns #-}
module ConvertCalcAbsToLLVM where

import qualified Abscalc as A
import Parcalc
import LLVM.General.AST
import LLVM.General.Module
import LLVM.General.Context
import qualified LLVM.General.AST.Global as G
import qualified LLVM.General.AST.Float as Fl
import qualified LLVM.General.AST.Constant as C
import ErrM

import Control.Monad.Trans.Error
import Data.List
import Data.Word
import Debug.Trace
import PromoteNum 


lowestRankNumT :: Type
lowestRankNumT = IntegerType {typeBits = 32 } -- FloatingPointType 64 IEEE   --- Integers

highestRankNumT :: Type
highestRankNumT = FloatingPointType 64 IEEE  --- Floats

getNumT :: Exp -> Type
getNumT (EInt _) = lowestRankNumT
getNumT (EFlo _) = highestRankNumT
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

getBinOpInstruction :: Exp -> NumT -> Operand -> Operand -> InstructionMetadata -> Instruction
getBinOpInstruction x@(EAdd _ _) IntT = trace ("trace1=" ++ show x ++ "\n") $ Add False False
getBinOpInstruction (EAdd _ _) _ = FAdd
getBinOpInstruction x@(ESub _ _) IntT = trace ("trace1=" ++ show x ++ "\n") $ Sub False False
getBinOpInstruction (ESub _ _) _ = FSub
getBinOpInstruction x@(EMul _ _) IntT = trace ("trace1=" ++ show x ++ "\n") $ Mul False False
getBinOpInstruction (EMul _ _) _ = FMul



getBinOpInstruction (EDiv _ _) _ = FDiv
getBinOpInstruction x@(numtype -> FloT) _  = trace ("trace4=" ++ show x ++ "\n")  FAdd 
getBinOpInstruction x FloT  = trace ("trace5=" ++ show x ++ "\n")  FAdd 
getBinOpInstruction x _     = trace ("trace6=" ++ show x ++ "\n") $ Add  False False

meta0 :: [a]
meta0 = []

binOpParams :: Exp -> Exp -> Type -> [Parameter]
binOpParams arg1 arg2 numT = getParms arg1 numT' ++ getParms arg2 numT' 
    where numT' = promoteNumType numT (promoteNumType (getNumT arg1) (getNumT arg2))

getParms :: Exp -> Type -> [Parameter]
getParms ex ty = nub $ getParms_ ex ty
    where
        getParms_ (EAdd arg1 arg2) numT = binOpParams arg1 arg2 numT
        getParms_ (ESub arg1 arg2) numT = binOpParams arg1 arg2 numT
        getParms_ (EMul arg1 arg2) numT = binOpParams arg1 arg2 numT
        getParms_ (EDiv arg1 arg2) _    = binOpParams arg1 arg2 highestRankNumT
        getParms_ (EVar _ x) numT = [Parameter numT (Name x) []]
        getParms_ (EInt _) _= []
        getParms_ (EFlo _) _= []


getInsts :: Name -> Word -> Exp -> [Named Instruction]
----------- EAdd
getInsts retname _ ex@(EAdd      (EVar _ x1)      (EVar _ x2)) =
                                  [ retname := getBinOpInstruction ex (numtype ex) 
                                                     (LocalReference (Name x1)) 
                                                     (LocalReference (Name x2)) 
                                                     meta0
                                  ]
getInsts retname _ ex@(EAdd      (EVar _ x1)      (EInt x2)) =
                                  [ retname := getBinOpInstruction ex IntT 
                                                     (LocalReference (Name x1)) 
                                                     (ConstantOperand (C.Int {C.integerBits = 32, C.integerValue = x2})) 
                                                     meta0
                                  ]
getInsts retname _ ex@(EAdd      (EInt x2)      (EVar _ x1) ) =
                                  [ retname := getBinOpInstruction ex IntT 
                                                     (ConstantOperand (C.Int {C.integerBits = 32, C.integerValue = x2})) 
                                                     (LocalReference (Name x1)) 
                                                     meta0
                                  ]
getInsts retname num ex@(EAdd      (EInt x1) arg2 ) = getInsts (UnName num) (num+1) arg2 ++
                                  [ retname := getBinOpInstruction ex IntT 
                                                     (ConstantOperand (C.Int {C.integerBits = 32, C.integerValue = x1})) 
                                                     (LocalReference (UnName num)) 
                                                     meta0
                                  ]
getInsts retname num ex@(EAdd arg1      (EInt x2) ) = getInsts (UnName num) (num+1) arg1 ++
                                  [ retname := getBinOpInstruction ex IntT 
                                                     (ConstantOperand (C.Int {C.integerBits = 32, C.integerValue = x2})) 
                                                     (LocalReference (UnName num)) 
                                                     meta0
                                  ]
getInsts retname _ ex@(EAdd      (EVar _ x1)      (EFlo x2)) =
                                  [ retname := getBinOpInstruction ex FloT 
                                                     (LocalReference (Name x1)) 
                                                     (ConstantOperand (C.Float $ Fl.Double x2)) 
                                                     meta0
                                  ]
getInsts retname _ ex@(EAdd      (EFlo x2)      (EVar _ x1) ) =
                                  [ retname := getBinOpInstruction ex FloT 
                                                     (ConstantOperand (C.Float $ Fl.Double x2)) 
                                                     (LocalReference (Name x1)) 
                                                     meta0
                                  ]
getInsts retname num ex@(EAdd      (EFlo x1) arg2 ) = getInsts (UnName num) (num+1) arg2 ++
                                  [ retname := getBinOpInstruction ex FloT 
                                                     (ConstantOperand (C.Float $ Fl.Double x1)) 
                                                     (LocalReference (UnName num)) 
                                                     meta0
                                  ]
getInsts retname num ex@(EAdd arg1      (EFlo x2) ) = getInsts (UnName num) (num+1) arg1 ++
                                  [ retname := getBinOpInstruction ex FloT 
                                                     (ConstantOperand (C.Float $ Fl.Double x2)) 
                                                     (LocalReference (UnName num)) 
                                                     meta0
                                  ]
getInsts (Name n) num ex@(EAdd arg1      arg2 ) = getInsts (Name (n ++ "0")) (num+1) arg1 ++ getInsts (Name (n ++ "1")) (num+2) arg2 ++
                                  [ Name n := getBinOpInstruction ex (numtype ex) 
                                                     (LocalReference (Name (n ++ "0"))) 
                                                     (LocalReference (Name (n ++ "1"))) 
                                                     meta0
                                  ]
getInsts (UnName n) num ex@(EAdd arg1      arg2 ) = getInsts (UnName (n + 1)) (num+1) arg1 ++ getInsts (UnName (n + 2)) (num+2) arg2 ++
                                  [ UnName n := getBinOpInstruction ex (numtype ex) 
                                                     (LocalReference (UnName (n + 1))) 
                                                     (LocalReference (UnName (n + 2))) 
                                                     meta0
                                  ]
-- getInsts retname _ ex@(EAdd      (EInt x1)      (EInt x2)) =
--                                   [ retname := getBinOpInstruction ex (numtype ex) 
--                                                      (ConstantOperand (C.Float $ Fl.Double (fromIntegral x1))) 
--                                                      (ConstantOperand (C.Float $ Fl.Double (fromIntegral x2))) 
--                                                      meta0
--                                   ]
----------- ESub
getInsts retname _ ex@(ESub      (EVar _ x1)      (EVar _ x2)) =
                                  [ retname := getBinOpInstruction ex (numtype ex) 
                                                     (LocalReference (Name x1)) 
                                                     (LocalReference (Name x2)) 
                                                     meta0
                                  ]
getInsts retname _ ex@(ESub      (EVar _ x1)      (EInt x2)) =
                                  [ retname := getBinOpInstruction ex (numtype ex) 
                                                     (LocalReference (Name x1)) 
                                                     (ConstantOperand (C.Int {C.integerBits = 32, C.integerValue = x2})) 
                                                     meta0
                                  ]
getInsts retname _ ex@(ESub      (EInt x2)      (EVar _ x1) ) =
                                  [ retname := getBinOpInstruction ex (numtype ex) 
                                                     (ConstantOperand (C.Int {C.integerBits = 32, C.integerValue = x2})) 
                                                     (LocalReference (Name x1)) 
                                                     meta0
                                  ]
getInsts retname num ex@(ESub      (EInt x1) arg2 ) = getInsts (UnName num) (num+1) arg2 ++
                                  [ retname := getBinOpInstruction ex (numtype ex) 
                                                     (ConstantOperand (C.Int {C.integerBits = 32, C.integerValue = x1})) 
                                                     (LocalReference (UnName num)) 
                                                     meta0
                                  ]
getInsts retname num ex@(ESub arg1      (EInt x2) ) = getInsts (UnName num) (num+1) arg1 ++
                                  [ retname := getBinOpInstruction ex (numtype ex) 
                                                     (ConstantOperand (C.Int {C.integerBits = 32, C.integerValue = x2})) 
                                                     (LocalReference (UnName num)) 
                                                     meta0
                                  ]
getInsts retname _ ex@(ESub      (EVar _ x1)      (EFlo x2)) =
                                  [ retname := getBinOpInstruction ex (numtype ex) 
                                                     (LocalReference (Name x1)) 
                                                     (ConstantOperand (C.Float $ Fl.Double x2)) 
                                                     meta0
                                  ]
getInsts retname _ ex@(ESub      (EFlo x2)      (EVar _ x1) ) =
                                  [ retname := getBinOpInstruction ex (numtype ex) 
                                                     (ConstantOperand (C.Float $ Fl.Double x2)) 
                                                     (LocalReference (Name x1)) 
                                                     meta0
                                  ]
getInsts retname num ex@(ESub      (EFlo x1) arg2 ) = getInsts (UnName num) (num+1) arg2 ++
                                  [ retname := getBinOpInstruction ex (numtype ex) 
                                                     (ConstantOperand (C.Float $ Fl.Double x1)) 
                                                     (LocalReference (UnName num)) 
                                                     meta0
                                  ]
getInsts retname num ex@(ESub arg1      (EFlo x2) ) = getInsts (UnName num) (num+1) arg1 ++
                                  [ retname := getBinOpInstruction ex (numtype ex) 
                                                     (ConstantOperand (C.Float $ Fl.Double x2)) 
                                                     (LocalReference (UnName num)) 
                                                     meta0
                                  ]
getInsts (Name n) num ex@(ESub arg1      arg2 ) = getInsts (Name (n ++ "0")) (num+1) arg1 ++ getInsts (Name (n ++ "1")) (num+2) arg2 ++
                                  [ Name n := getBinOpInstruction ex (numtype ex) 
                                                     (LocalReference (Name (n ++ "0"))) 
                                                     (LocalReference (Name (n ++ "1"))) 
                                                     meta0
                                  ]
getInsts (UnName n) num ex@(ESub arg1      arg2 ) = getInsts (UnName (n + 1)) (num+1) arg1 ++ getInsts (UnName (n + 2)) (num+2) arg2 ++
                                  [ UnName n := getBinOpInstruction ex (numtype ex) 
                                                     (LocalReference (UnName (n + 1))) 
                                                     (LocalReference (UnName (n + 2))) 
                                                     meta0
                                  ]
-- getInsts retname _ ex@(ESub      (EInt x1)      (EInt x2)) =
--                                   [ retname := getBinOpInstruction ex (numtype ex) 
--                                                      (ConstantOperand (C.Float $ Fl.Double (fromIntegral x1))) 
--                                                      (ConstantOperand (C.Float $ Fl.Double (fromIntegral x2))) 
--                                                      meta0
--                                   ]

----------- EMul
getInsts retname _ ex@(EMul      (EVar _ x1)      (EVar _ x2)) =
                                  [ retname := getBinOpInstruction ex (numtype ex) 
                                                     (LocalReference (Name x1)) 
                                                     (LocalReference (Name x2)) 
                                                     meta0
                                  ]
getInsts retname _ ex@(EMul      (EVar _ x1)      (EInt x2)) =
                                  [ retname := getBinOpInstruction ex (numtype ex) 
                                                     (LocalReference (Name x1)) 
                                                     (ConstantOperand (C.Int {C.integerBits = 32, C.integerValue = x2})) 
                                                     meta0
                                  ]
getInsts retname _ ex@(EMul      (EInt x2)      (EVar _ x1) ) =
                                  [ retname := getBinOpInstruction ex (numtype ex) 
                                                     (ConstantOperand (C.Int {C.integerBits = 32, C.integerValue = x2})) 
                                                     (LocalReference (Name x1)) 
                                                     meta0
                                  ]
getInsts retname num ex@(EMul      (EInt x1) arg2 ) = getInsts (UnName num) (num+1) arg2 ++
                                  [ retname := getBinOpInstruction ex (numtype ex) 
                                                     (ConstantOperand (C.Int {C.integerBits = 32, C.integerValue = x1})) 
                                                     (LocalReference (UnName num)) 
                                                     meta0
                                  ]
getInsts retname num ex@(EMul arg1      (EInt x2) ) = getInsts (UnName num) (num+1) arg1 ++
                                  [ retname := getBinOpInstruction ex (numtype ex) 
                                                     (ConstantOperand (C.Int {C.integerBits = 32, C.integerValue = x2})) 
                                                     (LocalReference (UnName num)) 
                                                     meta0
                                  ]
getInsts retname _ ex@(EMul      (EVar _ x1)      (EFlo x2)) =
                                  [ retname := getBinOpInstruction ex (numtype ex) 
                                                     (LocalReference (Name x1)) 
                                                     (ConstantOperand (C.Float $ Fl.Double x2)) 
                                                     meta0
                                  ]
getInsts retname _ ex@(EMul      (EFlo x2)      (EVar _ x1) ) =
                                  [ retname := getBinOpInstruction ex (numtype ex) 
                                                     (ConstantOperand (C.Float $ Fl.Double x2)) 
                                                     (LocalReference (Name x1)) 
                                                     meta0
                                  ]
getInsts retname num ex@(EMul      (EFlo x1) arg2 ) = getInsts (UnName num) (num+1) arg2 ++
                                  [ retname := getBinOpInstruction ex (numtype ex) 
                                                     (ConstantOperand (C.Float $ Fl.Double x1)) 
                                                     (LocalReference (UnName num)) 
                                                     meta0
                                  ]
getInsts retname num ex@(EMul arg1      (EFlo x2) ) = getInsts (UnName num) (num+1) arg1 ++
                                  [ retname := getBinOpInstruction ex (numtype ex) 
                                                     (ConstantOperand (C.Float $ Fl.Double x2)) 
                                                     (LocalReference (UnName num)) 
                                                     meta0
                                  ]
getInsts (Name n) num ex@(EMul arg1      arg2 ) = getInsts (Name (n ++ "0")) (num+1) arg1 ++ getInsts (Name (n ++ "1")) (num+2) arg2 ++
                                  [ Name n := getBinOpInstruction ex (numtype ex) 
                                                     (LocalReference (Name (n ++ "0"))) 
                                                     (LocalReference (Name (n ++ "1"))) 
                                                     meta0
                                  ]
getInsts (UnName n) num ex@(EMul arg1      arg2 ) = getInsts (UnName (n + 1)) (num+1) arg1 ++ getInsts (UnName (n + 2)) (num+2) arg2 ++
                                  [ UnName n := getBinOpInstruction ex (numtype ex) 
                                                     (LocalReference (UnName (n + 1))) 
                                                     (LocalReference (UnName (n + 2))) 
                                                     meta0
                                  ]
-- getInsts retname _ ex@(EMul      (EInt x1)      (EInt x2)) =
--                                   [ retname := getBinOpInstruction ex (numtype ex) 
--                                                      (ConstantOperand (C.Float $ Fl.Double (fromIntegral x1))) 
--                                                      (ConstantOperand (C.Float $ Fl.Double (fromIntegral x2))) 
--                                                      meta0
--                                   ]
----------- EDiv
getInsts retname _ ex@(EDiv      (EVar _ x1)      (EVar _ x2)) =
                                  [ retname := getBinOpInstruction ex (numtype ex) 
                                                     (LocalReference (Name x1)) 
                                                     (LocalReference (Name x2)) 
                                                     meta0
                                  ]
getInsts retname _ ex@(EDiv      (EVar _ x1)      (EInt x2)) =
                                  [ retname := getBinOpInstruction ex (numtype ex) 
                                                     (LocalReference (Name x1)) 
                                                     (ConstantOperand (C.Float $ Fl.Double (fromIntegral x2))) 
                                                     meta0
                                  ]
getInsts retname _ ex@(EDiv      (EInt x1)      (EVar _ x2) ) =
                                  [ retname := getBinOpInstruction ex (numtype ex) 
                                                     (ConstantOperand (C.Float $ Fl.Double (fromIntegral x1))) 
                                                     (LocalReference (Name x2)) 
                                                     meta0
                                  ]
{-
getInsts retname num ex@(EDiv      arg1      (EVar _ x2) ) = getInsts (UnName num) (num+1) arg1 ++
                                  [ retname := getBinOpInstruction ex (numtype ex) 
                                                     (LocalReference (UnName num)) 
                                                     (LocalReference (Name x2)) 
                                                     meta0
                                  ]
-}
getInsts retname num ex@(EDiv      (EInt x1) arg2 ) = getInsts (UnName num) (num+1) arg2 ++
                                  [ retname := getBinOpInstruction ex (numtype ex) 
                                                     (ConstantOperand (C.Float $ Fl.Double (fromIntegral x1))) 
                                                     (LocalReference (UnName num)) 
                                                     meta0
                                  ]
getInsts retname num ex@(EDiv arg1      (EInt x2) ) = getInsts (UnName num) (num+1) arg1 ++
                                  [ retname := getBinOpInstruction ex (numtype ex) 
                                                     (ConstantOperand (C.Float $ Fl.Double (fromIntegral x2))) 
                                                     (LocalReference (UnName num)) 
                                                     meta0
                                  ]
getInsts retname _ ex@(EDiv      (EVar _ x1)      (EFlo x2)) =
                                  [ retname := getBinOpInstruction ex (numtype ex) 
                                                     (LocalReference (Name x1)) 
                                                     (ConstantOperand (C.Float $ Fl.Double x2)) 
                                                     meta0
                                  ]
getInsts retname _ ex@(EDiv      (EFlo x2)      (EVar _ x1) ) =
                                  [ retname := getBinOpInstruction ex (numtype ex) 
                                                     (ConstantOperand (C.Float $ Fl.Double x2)) 
                                                     (LocalReference (Name x1)) 
                                                     meta0
                                  ]
getInsts retname num ex@(EDiv      (EFlo x1) arg2 ) = getInsts (UnName num) (num+1) arg2 ++
                                  [ retname := getBinOpInstruction ex (numtype ex) 
                                                     (ConstantOperand (C.Float $ Fl.Double x1)) 
                                                     (LocalReference (UnName num)) 
                                                     meta0
                                  ]
getInsts retname num ex@(EDiv arg1      (EFlo x2) ) = getInsts (UnName num) (num+1) arg1 ++
                                  [ retname := getBinOpInstruction ex (numtype ex) 
                                                     (ConstantOperand (C.Float $ Fl.Double x2)) 
                                                     (LocalReference (UnName num)) 
                                                     meta0
                                  ]
{-
getInsts retname _ ex@(EDiv      (EInt x1)      (EInt x2)) =
                                  [ retname := getBinOpInstruction ex (numtype ex) 
                                                     (ConstantOperand (C.Float $ Fl.Double (fromIntegral x1))) 
                                                     (ConstantOperand (C.Float $ Fl.Double (fromIntegral x2))) 
                                                     meta0
                                  ]
-}

------------------- Combos
getInsts (Name n) num ex@(EDiv arg1      arg2 ) = getInsts (Name (n ++ "0")) (num+1) arg1 ++ getInsts (Name (n ++ "1")) (num+2) arg2 ++
                                  [ Name n := getBinOpInstruction ex (numtype ex) 
                                                     (LocalReference (Name (n ++ "0"))) 
                                                     (LocalReference (Name (n ++ "1"))) 
                                                     meta0
                                  ]
getInsts (UnName n) num ex@(EDiv arg1      arg2 ) = getInsts (UnName (n + 1)) (num+1) arg1 ++ getInsts (UnName (n + 2)) (num+2) arg2 ++
                                  [ UnName n := getBinOpInstruction ex (numtype ex) 
                                                     (LocalReference (UnName (n + 1))) 
                                                     (LocalReference (UnName (n + 2))) 
                                                     meta0
                                  ]



getInsts retname _ ex@(EVar _ x) = 
                                  [ retname := getBinOpInstruction ex (numtype ex) 
                                                     (LocalReference (Name x)) 
                                                     (ConstantOperand (C.Float $ Fl.Double 0)) 
                                                     meta0
                                  ]

getInsts retname _ ex@(EInt x) = 
                                  [ retname := getBinOpInstruction ex (numtype ex) 
                                                     (ConstantOperand (C.Float $ Fl.Double (fromIntegral x))) 
                                                     (ConstantOperand (C.Float $ Fl.Double 0)) 
                                                     meta0
                                  ]
getInsts retname _ ex@(EFlo x) = 
                                  [ retname := getBinOpInstruction ex (numtype ex) 
                                                     (ConstantOperand (C.Float $ Fl.Double x)) 
                                                     (ConstantOperand (C.Float $ Fl.Double 0)) 
                                                     meta0
                                  ]
{-
getInsts retname _ ex = trace ("ex = " ++ show ex ++ "\n")
                                  [ retname := getBinOpInstruction ex (numtype ex) 
                                                     (ConstantOperand (C.Float $ Fl.Double 0)) 
                                                     (ConstantOperand (C.Float $ Fl.Double 0)) 
                                                     meta0
                                  ]
-}

main :: IO ()
main  = do 
    let Ok e = pExp $ myLexer "x + y"
        Ok dd= pExp $ myLexer str
        Ok ee= pExp $ myLexer eestr
        str = "((2 + (4 * z)) + 3)/7"
        eestr = "x + 2" -- "((a / 2) + 1/b)/c + 3*b"
        dd' = getInsts (Name "dd") 0 (promoteNum dd)
        ddTop = getModule [promoteNum dd]
    putStrLn $ "cc = " ++ "x + y"
    print $ getInsts (Name "cc") 0 (promoteNum e)
    putStrLn " ---------- "
    mapM_ print  dd'
    putStrLn " ---------- "
    putStrLn $ "dd = " ++ str
    dumpLLVMAsm ddTop
    putStrLn " ---------- "
    putStrLn $ "ee = " ++ eestr
    dumpLLVMAsm (getModule [promoteNum ee])
    x <- getContents
    let f (Ok y) = getModule [promoteNum y]
        f (Bad s) = defaultModule { moduleName = "Bad: " ++ s, moduleDefinitions = []}
    mapM_ (dumpLLVMAsm . f . pExp . myLexer) (lines x)

getModule :: [Exp] -> LLVM.General.AST.Module
getModule exps = defaultModule { moduleName = "Calc Module", moduleDefinitions = map (GlobalDefinition . getTopLvl) exps}
    where
        getTopLvl :: Exp -> Global 
        getTopLvl ex = let numT = getNumT ex in
                    functionDefaults {
                        G.returnType = numT,
                        G.name = Name "func_name",
                        G.parameters = (getParms ex numT, False),
                        G.basicBlocks = [ BasicBlock (Name "begin") 
                                                  (getInsts (Name "r") 0 ex)
                                                  (Do Ret {returnOperand = Just (LocalReference $ Name "r"), metadata'=meta0})] }

dumpLLVMAsm :: LLVM.General.AST.Module -> IO ()
dumpLLVMAsm mymod = do
    r <- withContext (\ctx -> runErrorT $ withModuleFromAST ctx  mymod moduleLLVMAssembly)
    report r

report :: Show a => Either a String -> IO ()
report (Right s) = putStrLn s
report (Left x) = putStrLn $ "ERROR: " ++ show x

