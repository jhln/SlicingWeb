module Smt where

import Grammar
import Parser

data Protocol = Program 
                  Status
                  Init
                  Trans
                  Prop
type Status = [SMTVar]
type Init   = [SMTPair]
type Trans  = [SMTClause]
type Prop   = [SMTClause]

instance Show Protocol where
  show (Program s i t p) = --"no program"
    "(program \n" 
    ++ "  (state \n" 
    ++ "  " ++ (concat $ map (\(SMTVar v) -> "  ("++v++" Int)") s)
        ++ ")\n"
    
    ++ "  (init \n" 
        ++"    " ++ (concat $ map (\(SMTeq h b) -> 
                  "(= "++show h++" "++show b++")") i)
        ++ ")\n"
    ++ "  (transition \n" 
        ++ ( concatAndInterleave "\n"  $ map (\(Clause pre post) -> 
                              "    (=>" ++ 
                              (concat $ map (\pair ->
                                case pair of
                                  (SMTeq h b)   -> 
                                    "(= "++show h++" "++show b++")"
                                  (SMTleq h b)  -> "(= "++show h++" "++show b++")"
                                  (SMTg h b)    -> "(= "++show h++" "++show b++")"
                                  (SMTuneq h b) -> "(= "++show h++" "++show b++")") 
                          pre) ++ ")\n" ++
                              "       ("++ 
                              (concat$ map (\pair ->
                                case pair of
                                  (SMTeq h b)   -> 
                                    "(= "++show h++" "++show b++")"
                                  (SMTleq h b)  -> "(= "++show h++" "++show b++")"
                                  (SMTg h b)    -> "(= "++show h++" "++show b++")"
                                  (SMTuneq h b) -> "(= "++show h++" "++show b++")") 
                          post) ) t)
        ++ ")\n"
    ++ "  (property \n"
    ++ "    " ++ show p ++ "))"
concatAndInterleave :: String -> [String] -> String
concatAndInterleave s (h:[]) = h
concatAndInterleave s (h:r) = h++s++concatAndInterleave s r
concatAndInterleave _ [] = []
    {-
    ++ "( property \n" 
        ++ map show i
        ++ ") \n"
    -}    
    

strToProtocolTest :: Protocol
strToProtocolTest = strToProtocol smtProg 


strToProtocol :: String -> Protocol
strToProtocol codeStr = 
  let commands  = extractCommands codeStr
      context   = extractContext commands
      clauses   = translComToProp 
                    context 
                    $ enumCommands 0 commands
   in Program 
        context 
        (map (\v -> (SMTeq v $ Num 0)) context)
        clauses
        []


type SMTProg  = [SMTClause]
data SMTClause  = Clause SMTPre SMTPost
instance Show SMTClause where
  show (Clause pre post) = show pre ++ " => " ++ show post ++"\n"
type SMTPre   = [SMTPair]
type SMTPost  = [SMTPair]
data SMTPair  = SMTeq SMTHead SMTBody
              | SMTleq SMTHead SMTBody
              | SMTg SMTHead SMTBody
              | SMTuneq SMTHead SMTBody
type SMTHead  = SMTVar
type SMTBody  = ArithExpr
data SMTVar   = SMTVar Var
instance Show SMTVar where
  show (SMTVar v) = v
--data SMTNum   = ArithExpr


instance Show SMTPair where
  show (SMTeq (SMTVar v) b) = v ++" = "++ show b
  show (SMTleq (SMTVar v) b) = v ++" <= "++ show b
  show (SMTg (SMTVar v) b) = v ++" > "++ show b
  show (SMTuneq (SMTVar v) b) = v ++" /= "++ show b

strToPropTest :: SMTProg
strToPropTest = strToProp smtProg


strToProp :: String -> SMTProg
strToProp codeStr = 
  let commands  = extractCommands codeStr
      context   = extractContext commands
   in translComToProp context $ enumCommands 0 commands


extractContextTest :: [SMTVar]
extractContextTest        = extractContext testProg1
extractContextTestAnswer  = [SMTVar "b",SMTVar "r",SMTVar "q",SMTVar "a"]

extractContext :: Command -> [SMTVar]
extractContext = map (\(Var v) -> SMTVar v) . varsInCommand

translComToProp :: [SMTVar] -> [(Command, Int)] -> SMTProg
translComToProp context ((Assign v a,i):r)
  = let smtPrePc    = SMTeq (SMTVar "pc") $ Num i
        smtPost   = (SMTeq (SMTVar "pc'") $ Num $ i+1) :
                    (SMTeq (SMTVar v) a) :
                    [ SMTeq 
                      (SMTVar $ vName++"'")
                      (Var vName)
                    | (SMTVar vName) <- context, vName /= v] 
     in [(Clause [smtPrePc] smtPost)]++translComToProp context r
translComToProp context ((While b c,i):r)
  = let smtPrePc  = SMTeq (SMTVar "pc") $ Num i
        smtPost = [SMTeq (SMTVar $ vName++"'") (Var vName)
                  | (SMTVar vName) <-context]
        (posCond, negCond)      = conditionFrom b
        (pcForPosCond, pcForNegCond)  = (succ i, succ $ lastPc i c)
     in [ (Clause 
            [smtPrePc , posCond]
            ((SMTeq (SMTVar "pc'") $ Num $ pcForPosCond) 
            : smtPost))
        , (Clause 
            [smtPrePc , negCond]
            ((SMTeq (SMTVar "pc'") $ Num $ pcForNegCond) 
            : smtPost))]
        ++ translComToProp context r
translComToProp _ [] = []
{-translComToProp context (If b c1 c2, i)
  = let smtPrePc  = SMTeq (SMTVar "pc") $ Num i
        smtPost = [SMTeq (SMTVar $ vName++"'") (Var vName)
                  | (SMTVar vName) <-context]
        (posCond, negCond)      = conditionFrom b
        (pcPosCond, pcNegCond)  = (succ i, succ $ lastPc i c)
     in [ [smtPrePc , posCond]
          , (SMTeq (SMTVar "pc'") $ Num $ pcPosCond) 
            : smtPost
        , [smtPrePc , negCond]
          , (SMTeq (SMTVar "pc'") $ Num $ pcNegCond) 
            : smtPost]-}


conditionFrom :: BoolExpr -> (SMTPair, SMTPair)
conditionFrom BHole = error "no BHole interpretable"
conditionFrom (Bool b) = error "no ture/false interpretable"
conditionFrom (Eq (Var v) a) =  ( SMTeq (SMTVar v) a
                                , SMTuneq (SMTVar v) a) 
conditionFrom (LEq (Var v) a) = ( SMTleq (SMTVar v) a
                                , SMTg (SMTVar v) a) 
conditionFrom _ = error "not translateable bool-expr"

lastPc :: Int -> Command -> Int
lastPc i c = snd $ last $ enumCommands i c


{-
translComToProp context [] =
  let smtPre  = [SMTeq (SMTVar "pc") $ Num $ -1]
      smtPost = [SMTeq (SMTVar "pc'") $ Num $ -1]
   in [(smtPre,smtPost)]
-}

testProgEnum = enumCommands 0 $ extractCommands smtProg
testProgEnumAnswer = [(Assign "i" (Num 0),0),(Assign "r" (Num 0),1),(While (LEq (Var "i") (Var "n")) (Seq (Assign "r" (Add (Var "r") (Var "i"))) (Assign "i" (Add (Var "i") (Num 1)))),2),(Assign "r" (Add (Var "r") (Var "i")),3),(Assign "i" (Add (Var "i") (Num 1)),4)]

testProg1 :: Command
testProg1 = 
  (Seq 
    (Assign "r" (Var "a")) 
    (While (LEq (Var "b") (Var "r")) 
      (Seq 
        (Assign "q" (Add (Var "q") (Num 1))) 
        (Assign "r" (Sub (Var "r") (Var "b"))))))

smtProg ::String
smtProg = "i:=0;r:=0;while(i<=n)do{r:=(r+i);i:=(i+1)}"



enumCommands :: Int -> Command -> [(Command, Int)]
enumCommands i (Seq s1 s2) = 
  let before = enumCommands i s1 
   in before ++ enumCommands ((+) 1 $ snd $ last before) s2
enumCommands i w@(While b c) = 
  (w, i) : (enumCommands (i+1) c)
enumCommands i w@(If b c1 c2) = 
  (w, i) : (enumCommands (i+1) $ Seq c1 c2)
enumCommands i c = [(c,i)]




varsInCommand :: Command -> [ArithExpr]
varsInCommand CHole   = []
varsInCommand Skip    = []
varsInCommand (Assign v a)  = completice [Var v]
                            $ varsInArith a
varsInCommand (Seq c1 c2) = 
   completice (varsInCommand c1) $ varsInCommand c2
varsInCommand (While b c) = 
  completice (varsInBool b) $ varsInCommand c


varsInArith :: ArithExpr -> [ArithExpr]
varsInArith v@(Var _) = [v]
varsInArith (Add a1 a2) = completice
                        ( varsInArith a1)
                        $ varsInArith a2
varsInArith (Sub a1 a2) = completice
                        ( varsInArith a1)
                        $ varsInArith a2
varsInArith _ = []

varsInBool :: BoolExpr -> [ArithExpr]
varsInBool (And b1 b2) = completice
                        ( varsInBool b1)
                        $ varsInBool b2
varsInBool (Eq a1 a2) = completice
                        ( varsInArith a1)
                        $ varsInArith a2
varsInBool (LEq a1 a2) = completice
                        ( varsInArith a1)
                        $ varsInArith a2
varsInBool _ = []

enlarge :: Eq a => a -> [a] -> [a]
enlarge e s@(a:as)
  | e == a    = s
  | otherwise = a : enlarge e as
enlarge e []  = [e]

completice :: Eq a => [a] -> [a] -> [a]
completice (e:es) as  = completice es 
                      $ enlarge e as
completice [] as = as



