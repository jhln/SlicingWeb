module Common.Eval where

import Common.Grammar


readFromStore :: State -> Var -> Maybe ArithVal
readFromStore (Store rest var val) v
    | var == v  = val
    | otherwise = readFromStore rest v
readFromStore Empty _ = error "undeclared variable"


setInStore :: State -> Var -> Maybe ArithVal -> State
setInStore (Store rest var content) v val
    | var == v  = Store rest var val
    | otherwise = Store (setInStore rest v val) var content
setInStore Empty _ _ = error "undeclared variable"


evalA :: State -> ArithExpr -> (ArithTrace, Maybe ArithVal)
evalA s (Num i)         = (TNum i, Just i)
evalA s (Var v)         = (TRead v value, value)
    where
        value = readFromStore s v
evalA s (Add exp1 exp2) = (TAdd t1 t2, (+) <$> v1 <*> v2)
    where
        (t1,v1) = evalA s exp1
        (t2,v2) = evalA s exp2
evalA s (Sub exp1 exp2) = (TSub t1 t2, (-) <$> v1 <*> v2)
    where
        (t1,v1) = evalA s exp1
        (t2,v2) = evalA s exp2
evalA _ _               = undefined


-- vlt doch partial und hole-free grammar trennen
evalB :: State -> BoolExpr -> (BoolTrace, BoolVal)
evalB _ (Bool True)     = (TTruth, True)
evalB _ (Bool False)    = (TFalsum, False)
evalB s (Eq a1 a2)      = (TEq t1 t2, v1 == v2)
    where
        (t1, v1) = evalA s a1
        (t2, v2) = evalA s a2
evalB s (LEq a1 a2)      = (TLEq t1 t2, v1 <= v2)
    where
        (t1, v1) = evalA s a1
        (t2, v2) = evalA s a2
evalB s (Not b)         = (TNot t, not v)
    where
        (t,v) = evalB s b
evalB s (And exp1 exp2) = (TAnd t1 t2, v1 && v2)
    where
        (t1,v1) = evalB s exp1
        (t2,v2) = evalB s exp2
evalB _ _           = undefined


evalC :: State -> Command -> (CommandTrace , State)
evalC s Skip                =   (TSkip , s)
evalC s (Assign var aExp)   =   (TAssign var t ,
                                setInStore s var a)
    where
        (t,a) = evalA s aExp
evalC s (Seq c1 c2)         =   (TSeq t1 t2, s2)
    where
        (t1, s1) = evalC s c1
        (t2, s2) = evalC s1 c2
-- ist die auswertung lazy
evalC s (If bExp ifComExp elseComExp)
    | boolValue     = (TIfTrue boolTrace ifTrace, newStateIf)
    | not boolValue = (TIfFalse boolTrace elseTrace, newStateElse)
    where
        (boolTrace, boolValue) = evalB s bExp
        (ifTrace, newStateIf) = evalC s ifComExp
        (elseTrace, newStateElse) = evalC s elseComExp
-- eigenartige semantic
evalC s whileCom@(While b c)
    | boolV     =   ( TWhileTrue boolT whileT nextT
                        , nextS)
    | not boolV = (TWhileFalse boolT, s)
    where
        (boolT, boolV) = evalB s b
        (whileT, newS) = evalC s c
        (nextT, nextS) = evalC newS whileCom
evalC _ _       = undefined