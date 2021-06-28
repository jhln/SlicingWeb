module Common.ForwardSlice where

import Common.Grammar
import Common.Eval


fwrdA :: ArithTrace -> State -> ArithExpr -> ArithExpr
fwrdA _ _ AHole     = AHole
fwrdA _ _ (Num n)   = Num n
fwrdA _ s (Var x)   = maybe AHole Num (readFromStore s x)
-- kann ArithExpr monadisch machen bzgl. AHole
fwrdA (TAdd t1 t2) s (Add a1 a2) =
    let v1 = fwrdA t1 s a1
     in case v1 of
        AHole  -> AHole
        Num n1 ->
             let v2 = fwrdA t2 s a2
              in case v2 of
                  AHole     -> AHole
                  Num n2    -> Num $ (+) n1 n2
                  _ -> error "fwrdA did not pattern match in Add"
        _ -> error "fwrdA did not pattern match in Add"
fwrdA (TSub t1 t2) s (Sub a1 a2) =
    let v1 = fwrdA t1 s a1
     in case v1 of
        AHole  -> AHole
        Num n1 ->
             let v2 = fwrdA t2 s a2
              in case v2 of
                  AHole     -> AHole
                  Num n2    -> Num $ (-) n1 n2
                  _ -> error "fwrdA did not pattern match in Sub"
        _ -> error "fwrdA did not pattern match in Sub"
fwrdA _ _ _ = error "fwrdA did not pattern match"


fwrdB :: BoolTrace -> State -> BoolExpr -> BoolExpr
fwrdB _ _ BHole     = BHole
fwrdB _ _ (Bool b)  = Bool b
fwrdB (TEq t1 t2) s (Eq a1 a2) =
    let v1 = fwrdA t1 s a1
     in case v1 of
        AHole  -> BHole
        Num n1 ->
             let v2 = fwrdA t2 s a2
              in case v2 of
                  AHole     -> BHole
                  Num n2    -> Bool $ (==) n1 n2
                  _ -> error "fwrdB did not pattern match in Eq"
        _ -> error "fwrdB did not pattern match in Eq"
fwrdB (TLEq t1 t2) s (LEq a1 a2) =
    let v1 = fwrdA t1 s a1
     in case v1 of
        AHole  -> BHole
        Num n1 ->
             let v2 = fwrdA t2 s a2
              in case v2 of
                  AHole     -> BHole
                  Num n2    -> Bool $ (<=) n1 n2
                  _ -> error "fwrdB did not pattern match in LEq"
        _ -> error "fwrdB did not pattern match in LEq"
fwrdB (TNot t) s (Not b)    =
    case fwrdB t s b of
        BHole    -> BHole
        (Bool b) -> Bool $ not b
        _       -> error "fwrdB did not pattern match in Not"
-- kann BoolExpr monadisch machen bzgl. AHole
fwrdB (TAnd t1 t2) s (And b1 b2) =
    let v1 = fwrdB t1 s b1
     in case v1 of
        BHole  -> BHole
        Bool b1 -> let v2 = fwrdB t2 s b2
                   in case v2 of
                        BHole       -> BHole
                        Bool b2     -> Bool $ b1 && b2                                        
                        _           -> error "fwrdB did not pattern match in Eq"
        _ -> error "fwrdB did not pattern match in And"
fwrdB _ _ _ = error "this is a fwrdB constellation, that did not match"


fwrdC :: CommandTrace -> State -> Command -> State
fwrdC TSkip s CHole         = s
fwrdC TSkip s Skip          = s
fwrdC (TAssign v t) s CHole = 
    setInStore s v Nothing
fwrdC (TAssign v t) s (Assign x a) =
--    setInStore s v $ fwrdA t s a
    case fwrdA t s a of
        Num n   -> setInStore s v $ Just n
        AHole   -> setInStore s v Nothing 
        x       -> error $ show x ++ " this is a fwrdC constellation, that did not match in Assing"
fwrdC (TSeq t1 t2) s CHole  = s2
    where
        s1 = fwrdC t1 s CHole
        s2 = fwrdC t2 s1 CHole
fwrdC (TSeq t1 t2) s (Seq c1 c2) = s2
    where
        s1 = fwrdC t1 s c1
        s2 = fwrdC t2 s1 c2
fwrdC (TIfTrue tB t1) s CHole            = fwrdC t1 s CHole
fwrdC (TIfTrue tB t1) s (If b c1 _) =
    case fwrdB tB s b of
        BHole       -> fwrdC t1 s CHole  
-- wieso nicht false?
        Bool True   -> fwrdC t1 s CHole
        _           -> error "this is a fwrdC constellation, that did not match in IfTrue"
fwrdC (TIfFalse tB t2) s CHole       = fwrdC t2 s CHole
fwrdC (TIfFalse tB t2) s (If b _ c2) =
    case fwrdB tB s b of
        BHole       -> fwrdC t2 s CHole
-- sich dass nicht auch true?
        Bool False  -> fwrdC t2 s c2
        _           -> error "this is a fwrdC constellation, that did not match in IfFalse"
fwrdC (TWhileFalse bT) s CHole = s
fwrdC (TWhileFalse bT) s (While b c) = s
-- komische seqenzierung
fwrdC (TWhileTrue tB tC tW) s CHole 
    = (\sC -> fwrdC tW sC CHole) (fwrdC tC s CHole)
fwrdC (TWhileTrue tB tC tW) s whileCom@(While b c) =
    case fwrdB tB s b of
        BHole       -> (\sC -> fwrdC tW sC CHole) (fwrdC tC s CHole)
        Bool True   -> (\sC -> fwrdC tW sC whileCom) (fwrdC tC s c)
        _           -> error "this is a fwrdC constellation, that did not match in WhileTrue"
fwrdC _ _ _ = error "this is a fwrdC constellation"