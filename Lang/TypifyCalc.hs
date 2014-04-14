module Lang.TypifyCalc where

import Lang.AbsCalc
import Lang.ErrM
type Result = Err Exp

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

-- | A type check if you want it
check :: Exp -> Bool
check (EAs typ x)      = typ >= getTyp x
check (EPos _ _ typ x) = typ >= getTyp x
check _ = True
{-
transIdent :: Ident -> Result
transIdent x = case x of
  Ident str  -> failure x
-}

-- | getTyp 
--      Does no checking, assumes EAs is valid
--      Infers max type on untyped operators
--      Divisions are always max (TFlo)
getTyp :: Exp -> Typ
getTyp x = case x of
  EPos pos n typ exp -> typ 
  EAs typ exp  -> typ
  EAdd exp1 exp2  -> max (getTyp exp1) (getTyp exp2)
  ESub exp1 exp2  -> max (getTyp exp1) (getTyp exp2)
  EMul exp1 exp2  -> max (getTyp exp1) (getTyp exp2)
  EDiv exp1 exp2  -> TFlo
  EInt n   -> TI32
  EFlo d   -> TFlo
  EVar id  -> TUnk


-- | transExp 
--
--    No checking, conflicts are always resolved by taking max
--    No failures are possible...
--    After this is run all expressions are tagged with EAs
transExp :: Exp -> Result
transExp x = case x of
  EPos pos n typ exp  -> let {typ2 = getTyp exp; maxTyp = max typ typ2} in 
                        return $ EPos pos n maxTyp exp
  EAs typ exp  -> let {typ2 = getTyp exp; maxTyp = max typ typ2} in 
                        return $ EAs maxTyp exp
  EAdd exp1 exp2  -> let {maxTyp = max typ typ2; typ=getTyp exp1; typ2=getTyp exp2} in
                        return $ EAs maxTyp x
  ESub exp1 exp2  -> let {maxTyp = max typ typ2; typ=getTyp exp1; typ2=getTyp exp2} in
                        return $ EAs maxTyp x
  EMul exp1 exp2  -> let {maxTyp = max typ typ2; typ=getTyp exp1; typ2=getTyp exp2} in
                        return $ EAs maxTyp x
  EDiv exp1 exp2  -> return $ EAs TFlo x
  EInt n   -> return (EAs TI32 x)
  EFlo d   -> return (EAs TFlo x)
  EVar id  -> return (EAs TUnk x)


{-
transTyp :: Typ -> Result
transTyp x = case x of
  TI32  -> failure x
  TFlo  -> failure x
  TUnk  -> failure x
-}


{-
transPos :: Pos -> Result
transPos x = case x of
  PLin  -> failure x
  PCol  -> failure x
  PUnk  -> failure x
-}



