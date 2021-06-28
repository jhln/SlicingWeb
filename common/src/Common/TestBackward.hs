module Common.TestBackward where

import Common.Grammar
import Common.Eval
import Common.BackwardSlice
import Common.TestExpr


arithTestTrace :: ArithTrace
arithTestTrace = (TAdd 
                    (TRead "z" (Just 2)) 
                    (TSub 
                        (TRead "y" (Just 2)) 
                        (TNum 1))) 


bwrdATest1 :: (State, ArithExpr)
bwrdATest1 = bwrdA 
                arithTestTrace 
                orgOutState
                $ Just 3


-- q→0, r→0, res→0, a→4, b→2
orgInState :: State
orgInState = makeState 
    [ ("q", Just 0)
    , ("r", Just 0)
    , ("res", Just 0)
    , ("a", Just 4)
    , ("b", Just 2)]

--r := a
--; while ( b <= r ) do
--    { q := q + 1
--    ; r := r - b}
--; if ( ! (r = 0) )
--    then
--        {res := 0}
--    else
--        {res := 1}

prog :: Command
prog = 
    Seq 
        rAsn 
        (Seq whlB ifR)


progTest :: (State, Command)
progTest = bwrdC (fst $ evalC orgInState prog) orgBwrdState 

progTestAnswer :: (State, Command)
progTestAnswer = (Store (Store (Store (Store (Store Empty "a" (Just 4)) "res" Nothing) "q" Nothing) "b" (Just 2)) "r" Nothing,Seq (Assign "r" (Var "a")) (Seq (While (LEq (Var "b") (Var "r")) (Seq CHole (Assign "r" (Sub (Var "r") (Var "b"))))) (If (Not (Eq (Var "r") (Num 0))) CHole (Assign "res" (Num 1)))))


-- q→2,r→0,res→1,a→4,b→2
orgOutState :: State
orgOutState = makeState 
    [ ("q", Just 2)
    , ("r", Just 0)
    , ("res", Just 1)
    , ("a", Just 4)
    , ("b", Just 2)]

-- q→,r→,res→1,a→,b→
orgBwrdState = makeState
    [ ("q", Nothing )
    , ("r", Nothing)
    , ("res", Just 1)
    , ("a", Nothing)
    , ("b", Nothing)]



-- r := a
rAsn :: Command
rAsn = Assign "r" $ Var "a"


-- while ( b <= r ) do
--    { q := q + 1
--    ; r := r - b}
whlB :: Command
whlB = While (LEq (Var "b") (Var "r"))
        $ Seq   (Assign "q" 
                    (Add 
                        (Var "q") 
                        $ Num 1)) 
                (Assign "r" 
                    (Sub 
                        (Var "r") 
                        $ Var "b")) 
                
-- if ( ! (r = 0) )
--    then
--        {res := 0}
--    else
--        {res := 1}
ifR :: Command
ifR = If (Not (Eq (Var "r") $ Num 0)) 
        (Assign "res" $ Num 0)
        (Assign "res" $ Num 1)




