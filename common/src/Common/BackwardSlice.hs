module Common.BackwardSlice where

import Common.Grammar
import Common.Eval


joinVarInStore :: (Var, Maybe ArithVal) -> State -> State
joinVarInStore t@(v1,val1) (Store r v2 val2)
    | v1 == v2  = Store r v1 (joinVal val1 val2)
    | otherwise = Store (joinVarInStore t r) v2 val2
    where
        joinVal v1 v2 = max v1 v2
joinVarInStore (v, val) Empty   = Store Empty v val

joinStores :: State -> State -> State
joinStores (Store r v val) s2   = joinStores r 
                                $ joinVarInStore 
                                    (v, val) 
                                    s2
joinStores Empty s2             = s2


bwrdA   :: ArithTrace
        -> State
        -> Maybe ArithVal
        -> (State , ArithExpr )
bwrdA _ _ Nothing                   =   ( Empty , AHole)
bwrdA (TNum nT) s (Just n)          =   ( Empty , Num nT)
bwrdA (TRead x maybeVal) s maybeValtoSlice  
    | maybeVal == maybeValtoSlice   =   ( Store Empty x maybeVal 
                                        , Var x)
    | otherwise                     =   error "slicing on the wrong value"
bwrdA (TAdd t1 t2) s (Just n)       =   ( joinStores s1 s2
                                        , Add a1 a2)
    where
        -- here the v1 is missing
        (s1,a1) = bwrdA t1 s $ recoverA t1
        (s2,a2) = bwrdA t2 s $ recoverA t2
bwrdA (TSub t1 t2) s (Just n)        =   ( joinStores s1 s2
                                        , Sub a1 a2)
    where
        (s1,a1) = bwrdA t1 s $ recoverA t1
        (s2,a2) = bwrdA t2 s $ recoverA t2


recoverA :: ArithTrace -> Maybe ArithVal
recoverA (TAdd t1 t2)       =   (+) 
                            <$> recoverA t1 
                            <*> recoverA t2
recoverA (TSub t1 t2)       =   (-) 
                            <$> recoverA t1 
                            <*> recoverA t2
recoverA (TRead _ maybeVal) =   maybeVal
recoverA (TNum i)           =   Just i


bwrdB   :: BoolTrace
        -> State
        -> Maybe BoolVal 
        -> (State , BoolExpr  )
bwrdB _ _ Nothing               =   ( Empty , BHole)
bwrdB TTruth s (Just true)      =   ( Empty 
                                    , Bool true)
bwrdB TFalsum s (Just false)    =   ( Empty 
                                    , Bool false)
bwrdB (TEq t1 t2) s _           =   ( joinStores s1 s2
                                    , Eq a1 a2)
    where
        (s1,a1) = bwrdA t1 s $ recoverA t1
        (s2,a2) = bwrdA t2 s $ recoverA t2
bwrdB (TLEq t1 t2) s _          =   ( joinStores s1 s2
                                    , LEq a1 a2)
    where
        (s1,a1) = bwrdA t1 s $ recoverA t1
        (s2,a2) = bwrdA t2 s $ recoverA t2
bwrdB (TNot b) s v              =   ( newS 
                                    , Not newExp)
    where
        (newS,newExp) = bwrdB b s v
bwrdB (TAnd t1 t2) s _          =   ( joinStores s1 s2
                                    , And a1 a2)
    where
        (s1,a1) = bwrdB t1 s $ recoverB t1
        (s2,a2) = bwrdB t2 s $ recoverB t2


recoverB :: BoolTrace -> Maybe BoolVal
recoverB TTruth         = Just True 
recoverB TFalsum        = Just False 
recoverB (TEq t1 t2)    =   (==) 
                        <$> recoverA t1 
                        <*> recoverA t2
recoverB (TLEq t1 t2)   =   (<=) 
                        <$> recoverA t1 
                        <*> recoverA t2
recoverB (TNot t)       = not 
                        <$> recoverB t
recoverB (TAnd t1 t2)   =   (&&) 
                        <$> recoverB t1 
                        <*> recoverB t2


bwrdC :: CommandTrace -> State -> (State, Command)
bwrdC _ Empty = ( Empty , CHole )
bwrdC TSkip s = ( s , CHole )
bwrdC (TAssign x t) s = 
    case readFromStore s x of
        Nothing     -> ( s , CHole )
        Just val    -> 
            let (newS,newExp) = bwrdA t s $ Just val
             in ( joinStores newS (setInStore s x Nothing)
                , Assign x newExp)
bwrdC (TSeq t1 t2) s =
    case bwrdC t2 s of
        ( s' , CHole ) -> 
            case bwrdC t1 s' of
                ( s'' , CHole )  -> ( s'' , CHole )
                ( s'' , c1 )     -> ( s'' , Seq c1 CHole )
        ( s' , c2 ) -> 
            let ( s'' , c1 ) = bwrdC t1 s'
             in ( s'' , Seq c1 c2)
bwrdC (TIfTrue tb t1) s =
    case bwrdC t1 s of
        ( s' , CHole ) -> ( s' , CHole )
        ( s' , c1) -> 
            let ( sb , b ) = bwrdB tb s ( Just True )
             in ( joinStores s' sb , If b c1 CHole )
bwrdC (TIfFalse tb t2) s =
    case bwrdC t2 s of
        ( s' , CHole ) -> ( s' , CHole )
        ( s' , c2) -> 
            let ( sb , b ) = bwrdB tb s (Just False )
             in ( joinStores s' sb , If b CHole c2 )
bwrdC (TWhileFalse tb) s = ( s , CHole )
bwrdC (TWhileTrue tb tc tw) s = 
    case bwrdC tw s of
        ( sw , CHole ) -> 
            case bwrdC tc sw of
                ( sc , CHole ) -> ( sc , CHole )
                ( sc , c ) ->
                    let ( sb , b ) = bwrdB tb s (Just True)
                     in ( joinStores sc sb 
                        , While b c)
        ( sw , cw ) ->
            let ( sc , c ) = bwrdC tc sw 
                ( sb , b ) = bwrdB tb s (Just True)
             in ( joinStores sc sb , While b c)