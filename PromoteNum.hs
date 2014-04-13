{-# LANGUAGE ViewPatterns #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE DeriveTraversable #-}
module PromoteNum where

import Abscalc
-- import Data.Traversable

data NumT = IntT | FloT | Unknown deriving (Eq, Ord, Show)

-- deriving instance Traversable Exp


numtype :: Exp -> NumT
numtype (EInt _) = IntT
numtype (EAdd (numtype -> IntT) (numtype -> IntT)) = IntT
numtype (ESub (numtype -> IntT) (numtype -> IntT)) = IntT
numtype (EMul (numtype -> IntT) (numtype -> IntT)) = IntT
numtype (EAdd _ _) = FloT
numtype (ESub _ _) = FloT
numtype (EMul _ _) = FloT
numtype (EDiv _ _) = FloT
numtype (EVar _) = Unknown -- don't trigger promotion
numtype (EFlo _) = FloT 


promoteNum :: Exp -> Exp
promoteNum expr = if numtype expr == FloT then promote expr
                                          else expr
                    where promote (EInt x) = EFlo (fromIntegral x)
                          promote (EAdd x y) = EAdd (promote x) (promote y)
                          promote (ESub x y) = ESub (promote x) (promote y)
                          promote (EMul x y) = EMul (promote x) (promote y)
                          promote (EDiv x y) = EDiv (promote x) (promote y)
                          promote y = y
