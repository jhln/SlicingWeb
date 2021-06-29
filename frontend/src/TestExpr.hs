module TestExpr where

import Grammar
import Eval
import ForwardSlice

-- z := z + 1
zAsn :: Command  
zAsn =  Assign "z" $ 
        Add (Var "z") $ Num 1

-- y := x + 1
yxAsn :: Command  
yxAsn = Assign "y" $ 
        Add (Var "x") $ Num 1

-- y := y + 1
yyAsn :: Command  
yyAsn = Assign "y" $ 
        Add (Var "y") $ Num 1

prob2 :: (CommandTrace, State)
prob2 = evalC inSAuto  yyAsn

prob3 :: (CommandTrace, State)
prob3 = evalC inSAuto (Assign "y" $ Num 1)

-- y = 1
yEq :: BoolExpr 
yEq = Eq (Var "y") $ Num 1

-- if (y = 1) then { y := x + 1 } 
--            else { y := y + 1 }
ifYCom :: Command  
ifYCom = If yEq yxAsn yyAsn

prob1 :: (CommandTrace, State)
prob1 = evalC inSAuto ifYCom

-- if (y = 1) then { y := x + 1 } 
--            else { y := y + 1 };
-- z := z + 1
ifYz :: Command  
ifYz = Seq ifYCom zAsn

prob0 :: (CommandTrace, State)
prob0 = evalC inSAuto ifYz

prob0answer :: (CommandTrace, State)
prob0answer = 
    ( (TSeq 
        (TIfFalse 
            (TEq 
                (TRead "y" (Just 0)) 
                (TNum 1)) 
            (TAssign "y"
                (TAdd 
                    (TRead "y" (Just 0)) 
                    (TNum 1)))) 
        (TAssign "z" 
            (TAdd 
                (TRead "z" (Just 2)) 
                (TNum 1))))
    , Store (Store (Store Empty 
                        "x" (Just 1)) 
                "y" (Just 1)) 
        "z" (Just 3))


fwrdTest1 :: ArithExpr
fwrdTest1 = fwrdA (TAdd (TRead "z" (Just 2)) (TNum 1)) inSAuto (Add (Var "z") $ Num 1)
fwrdTest2 :: ArithExpr
fwrdTest2 = fwrdA (TAdd (TRead "z" (Just 2)) (TNum 1)) outSHole  (Add (Var "z") $ Num 1)
fwrdTest3 :: State
fwrdTest3 = fwrdC (TAssign "z" (TAdd (TRead "z" (Just 2)) (TNum 1)))  outSHole yyAsn 
fwrdTest3answer :: State
fwrdTest3answer = Store (Store (Store Empty "x" Nothing) "y" (Just 0)) "z" (Just 1)
fwrdTest4 :: State
fwrdTest4 = fwrdC (TAssign "z" (TAdd (TRead "z" (Just 2)) (TNum 1)))  outSHole CHole 
fwrdTest4answer :: State
fwrdTest4answer = Store (Store (Store Empty "x" Nothing) "y" (Just 0)) "z" Nothing
fwrdTest5 :: State
fwrdTest5 = fwrdC (TAssign "y"(TAdd (TRead "y" (Just 0)) (TNum 1))) outSHole yyAsn
fwrdTest5answer :: State
fwrdTest5answer = Store (Store (Store Empty "x" Nothing) "y" (Just 1)) "z" Nothing
fwrdTest6 :: State
fwrdTest6 = fwrdC (TIfFalse (TEq (TRead "y" (Just 0)) (TNum 1)) (TAssign "y"(TAdd (TRead "y" (Just 0)) (TNum 1)))) outSHole ifYCom 
fwrdTest6answer :: State
fwrdTest6answer = Store (Store (Store Empty "x" Nothing) "y" (Just 1)) "z" Nothing
fwrdTest7 :: State
fwrdTest7 = fwrdC (TIfFalse (TEq (TRead "y" (Just 0)) (TNum 1)) (TAssign "y"(TAdd (TRead "y" (Just 0)) (TNum 1)))) inSAuto  ifYCom 
fwrdTest7answer :: State
fwrdTest7answer = Store (Store (Store Empty "x" (Just 1)) "y" (Just 1)) "z" (Just 2)
fwrdTest8 :: State
fwrdTest8 = fwrdC (fst prob0) outSHole ifYz
fwrdTest8answer :: State
fwrdTest8answer = Store (Store (Store Empty "x" Nothing) "y" (Just 1)) "z" Nothing


-- [x -> 1, y -> 0, z -> 2]
inS :: State 
inS = Store 
        Empty "z" $ Just 2

makeState   :: [(Var, Maybe ArithVal)]
            -> State 
makeState ((v,maybeVal):rest) = 
    Store (makeState rest) v maybeVal 
makeState [] = Empty

-- [x -> 1, y -> 0, z -> 2]
inSAuto :: State 
inSAuto = makeState 
    [ ("z", Just 2)
    , ("y", Just 0)
    , ("x", Just 1)]

-- [x -> Hole, y -> 1, z -> Hole]
outSHole :: State 
outSHole = makeState 
    [ ("z", Nothing )
    , ("y", Just 0)
    , ("x", Nothing)]

-- [x -> Hole, y -> Hole, z -> 2]
inSHole :: State 
inSHole = makeState 
    [ ("z", Nothing )
    , ("y", Nothing)
    , ("x", Just 2)]