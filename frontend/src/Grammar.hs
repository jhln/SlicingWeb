module Grammar where

type Var        = String   
--                deriving (Eq, Ord, Show)
data ArithExpr  = AHole
                | Num Int 
                | Var Var 
                | Add ArithExpr ArithExpr
                | Sub ArithExpr ArithExpr
                deriving (Ord, Eq)
instance Show ArithExpr where
    show (Add a1 a2) = show a1 ++ " + " ++ show a2
    show (Sub a1 a2) = show a1 ++ " - " ++ show a2
    show (Num i) = show i
    show (Var v) = v
    show (AHole) = "AHole"
    
-- order instance fraglich ob nicht besser per hand?
-- wie stelle ich Var var typen her?

data BoolExpr   = BHole
--                | Truth 
--                | Falsum 
                | Bool Bool
                | Eq ArithExpr ArithExpr
                | LEq ArithExpr ArithExpr
                | Not BoolExpr 
                | And BoolExpr BoolExpr
                deriving (Eq, Ord, Show)
data Command    = CHole
                | Skip
                | Assign Var ArithExpr
                | Seq Command Command
                | While BoolExpr Command
                | If BoolExpr Command Command
                deriving (Eq, Ord, Show)
type Value      = Either ArithVal BoolVal
type ArithVal   = Int 
--                deriving (Eq, Ord, Show)
type BoolVal    = Bool
data State      = Empty
                | Store State Var (Maybe ArithVal)
--                | SHole State Var 
                deriving (Eq, Ord, Show)

data Trace      = AT ArithTrace 
                | BT BoolTrace 
                | CT CommandTrace
data ArithTrace = TNum Int
                | TRead Var (Maybe ArithVal)
                | TAdd ArithTrace ArithTrace
                | TSub ArithTrace ArithTrace
                deriving Show
data BoolTrace  = TTruth
                | TFalsum
                | TEq ArithTrace ArithTrace
                | TLEq ArithTrace ArithTrace
                | TNot BoolTrace
                | TAnd BoolTrace BoolTrace
                deriving Show
data CommandTrace  
                = TSkip
                | TAssign Var ArithTrace
                | TSeq CommandTrace CommandTrace
                | TWhileTrue    BoolTrace CommandTrace CommandTrace
                | TWhileFalse   BoolTrace
                | TIfTrue   BoolTrace CommandTrace
                | TIfFalse  BoolTrace CommandTrace
                deriving Show
 

{-
instance Ord ArithExpr where
    AHole <= _              = True
    Num _ <= AHole          = False 
    Num x <= Num y          = x <= Y
    Num _ <= _              = True
    Var _ <= AHole          = False 
    Var x <= Var y          = x <= y
    Var _ <= _              = True
    Add _ _ <= AHole        = False
    Add x1 y1 <= Add x2 y2  = x1 <= x2 && y1 <= y2
    Add _ _ <= _            = False
-}

--                | SHole State Var 
{-
data AHole      = AHole
data BHole      = BHole
data CHole      = CHole
data SHole      = SHole State Var AHole

type PArithExpr = Maybe ArithExpr
type PBoolExpr  = Either BoolExpr  BHole
type PCommand   = Either Command CHole
type PState     = Either State SHole
-}