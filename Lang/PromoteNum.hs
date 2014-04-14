{-# LANGUAGE ViewPatterns #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE DeriveTraversable #-}
module Lang.PromoteNum where

import qualified Lang.AbsCalc as A
-- import Data.Traversable
import Debug.Trace

data NumT = IntT | FloT | Unknown deriving (Eq, Ord, Show)


data Exp =
   EAdd Exp Exp
 | ESub Exp Exp
 | EMul Exp Exp
 | EDiv Exp Exp
 | EInt Integer
 | EFlo Double
 | EVar NumT String
  deriving (Eq,Ord,Show)
-- deriving instance Traversable Exp

class HasNumT a where
    numtype :: a -> NumT
    
instance HasNumT A.Exp where
    numtype (A.EFlo _) = FloT 
    numtype (A.EInt _) = IntT
    numtype (A.EAdd (numtype -> IntT) (numtype -> IntT)) = IntT
    numtype (A.ESub (numtype -> IntT) (numtype -> IntT)) = IntT
    numtype (A.EMul (numtype -> IntT) (numtype -> IntT)) = IntT

    numtype (A.EAdd (numtype -> Unknown) (numtype -> IntT)) = Unknown
    numtype (A.ESub (numtype -> Unknown) (numtype -> IntT)) = Unknown
    numtype (A.EMul (numtype -> Unknown) (numtype -> IntT)) = Unknown

    numtype (A.EAdd (numtype -> IntT) (numtype -> Unknown)) = Unknown
    numtype (A.ESub (numtype -> IntT) (numtype -> Unknown)) = Unknown
    numtype (A.EMul (numtype -> IntT) (numtype -> Unknown)) = Unknown

    numtype (A.EAdd (numtype -> Unknown) (numtype -> Unknown)) = Unknown
    numtype (A.ESub (numtype -> Unknown) (numtype -> Unknown)) = Unknown
    numtype (A.EMul (numtype -> Unknown) (numtype -> Unknown)) = Unknown

    numtype (A.EAdd _ _) = FloT
    numtype (A.ESub _ _) = FloT
    numtype (A.EMul _ _) = FloT
    numtype (A.EDiv _ _) = FloT

    numtype (A.EVar _) = Unknown -- don't trigger promotion

instance HasNumT Exp where
    numtype (EFlo _) = FloT 
    numtype (EInt _) = IntT
    numtype (EAdd (numtype -> IntT) (numtype -> IntT)) = IntT
    numtype (ESub (numtype -> IntT) (numtype -> IntT)) = IntT
    numtype (EMul (numtype -> IntT) (numtype -> IntT)) = IntT

    numtype (EAdd (numtype -> Unknown) (numtype -> IntT)) = Unknown
    numtype (ESub (numtype -> Unknown) (numtype -> IntT)) = Unknown
    numtype (EMul (numtype -> Unknown) (numtype -> IntT)) = Unknown

    numtype (EAdd (numtype -> IntT) (numtype -> Unknown)) = Unknown
    numtype (ESub (numtype -> IntT) (numtype -> Unknown)) = Unknown
    numtype (EMul (numtype -> IntT) (numtype -> Unknown)) = Unknown

    numtype (EAdd (numtype -> Unknown) (numtype -> Unknown)) = Unknown
    numtype (ESub (numtype -> Unknown) (numtype -> Unknown)) = Unknown
    numtype (EMul (numtype -> Unknown) (numtype -> Unknown)) = Unknown

    numtype (EAdd _ _) = FloT
    numtype (ESub _ _) = FloT
    numtype (EMul _ _) = FloT
    numtype (EDiv _ _) = FloT

    numtype (EVar ty _) = ty-- don't trigger promotion


promoteNum :: A.Exp -> Exp
promoteNum expr = let r = if numtype expr == FloT then promote expr
                                          else unmote expr
                        in trace (show expr ++ " -promoteNum-> " ++ show r) r
                    where promote (A.EInt x) = EFlo (fromIntegral x)
                          promote (A.EAdd x y) = EAdd (promote x) (promote y)
                          promote (A.ESub x y) = ESub (promote x) (promote y)
                          promote (A.EMul x y) = EMul (promote x) (promote y)
                          promote (A.EDiv x y) = EDiv (promote x) (promote y)
                          promote (A.EVar (A.Ident x)) = EVar FloT x
                          promote (A.EFlo x) = EFlo x

                          unmote (A.EInt x) = EInt x 
                          unmote (A.EAdd x y) = EAdd (unmote x) (unmote y)
                          unmote (A.ESub x y) = ESub (unmote x) (unmote y)
                          unmote (A.EMul x y) = EMul (unmote x) (unmote y)
                          unmote (A.EDiv x y) = EDiv (unmote x) (unmote y)
                          unmote (A.EVar (A.Ident x)) = EVar IntT x
                          unmote (A.EFlo x) = EFlo x
                          
