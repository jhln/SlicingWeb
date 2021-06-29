{-# LANGUAGE LambdaCase #-}
module Parser where

import Text.Parsec
import Grammar
import Data.Functor.Identity

import Control.Monad (void)



-- | fixing ParsecT with String-Input, the ParseStore data-type and the Indentity-Monad as execution environment 
data ParserStore = ParserStore
type Parser a = ParsecT String ParserStore Identity a

-- * Implementation of the Parsec Parser

-- | top-level function for using the top-level parser for parsing mini-code as string-input
runWith :: Parser a -> String -> Either ParseError a
runWith p s = runIdentity $ runParserT p ParserStore "<interactive>" $clearWhiteSpace s

-- | reduce the input string by removing the white space
clearWhiteSpace :: String -> String
clearWhiteSpace = replace "\n" ""
                . replace " " ""

-- | helper function for the 'clearWhiteSpace' application
replace :: String -> String -> String -> String
replace _ replacement [] = []
replace pattern replacement str
    | pattern == (take (length pattern) str)
        = replacement ++ (replace pattern replacement 
                            (drop (length pattern) str))
    | otherwise = head str : (replace pattern replacement $ tail str)


data Relator = EQQ | LEQ deriving (Show, Eq)
data Operator = Plus | Minus deriving (Show, Eq)
-- data Digit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine deriving (Show, Eq, Enum)
-- data Letter = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z deriving (Show, Eq)

{-
isSmallLetter :: Char -> Bool
isSmallLetter c = elem c "abcdefghijklmnopqrstuvwxyz"

isOperator:: Char -> Bool
isOperator c = elem c "+-"

-- | parse (>= 1) ASCII digits, return parsed characters
num :: Parser Integer
num = do
    n <- many1 digit
    return (read n)

-- | consuming whitespace
whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

-- | consume trailing whitespace
lexeme :: Parser a -> Parser a
lexeme p = do
           x <- p
           whitespace
           return x

-- | parse single character, return parsed character
symbol :: Char -> Parser Char
symbol c = lexeme $ char c

-- | parse "(", parse ")", return enclosed characters
betweenParens :: Parser a -> Parser a
betweenParens p = between (symbol '(') (symbol ')') p

-- | parse "{", parse "}", return enclosed characters
betweenParensCurly :: Parser a -> Parser a
betweenParensCurly p = between (symbol '{') (symbol '}') p

operatorParse :: Parser Operator
operatorParse = do
      x <- (oneOf "+-")
      return $ chooseOp x
           where chooseOp '+' = Plus
                 chooseOp '-' = Minus-}


-- var := char ident_rest
var' :: Parser ArithExpr 
var' = {- (:) <$> char' <*> identRest' -}
    do
        c <- char'
        rs <- identRest'
        return $ Var $ (c:rs)

-- char := a | ... | z
char' ::Parser Char
char' = oneOf ['a'..'z']

-- ident_rest := ε | ident_char ident_rest
identRest' :: Parser String
identRest' = many identChar'

-- ident_char := char | digit | _
identChar' :: Parser Char
identChar' = choice
    [char '_', char', digit']

-- digit := 0 | ... | 9
digit' ::Parser Char
digit' = oneOf ['0'..'9']


num' :: Parser ArithExpr 
num' = do
    n <- many1 digit
    return $ Num $ read n

aHole' :: Parser ArithExpr 
aHole' = do
        string "Hole"
        return AHole 

prob1 :: Either ParseError ArithExpr
prob1 = runWith arithExprNested' "(f-5)+(3-(f+1))"
prob2 :: Either ParseError ArithExpr
prob2 = runWith arithExprNested' "((f-5)+(3-(f+1)))-3"

arithExprNested' :: Parser ArithExpr 
arithExprNested'  = choice 
            $ map try 
                [ aHole'
                , num'
                , var' 
                , do{   
                    string "(";
                    a1 <- arithExprNested';
                    op <- oneOf "+-";
                    a2 <- arithExprNested';
                    string ")";
                    let opConstruct '+' = Add
                        opConstruct '-' = Sub 
                        in
                            return 
                            $ opConstruct op a1 a2
                    } 
                ]

bHole' :: Parser BoolExpr 
bHole' = do
        string "Hole"
        return BHole

bool' :: Parser BoolExpr 
bool' = do
        b <-  choice 
            $ map string ["true","false"]
        let bVal "true" = True 
            bVal "false" = False
        return $ Bool $ bVal b

boolProb1 :: Either ParseError BoolExpr
boolProb1 = runWith boolExprNested'  "(true&&(false&&(a<=b)))"

boolExprNested' :: Parser BoolExpr 
boolExprNested'  = choice 
            $ map try 
                [ bHole'
                , bool'
--                , var' 
                , do{
                    string "(!";
                    b <- boolExprNested';
                    string ")";
                    return $ Not b
                    }
                , do{
                    string "(";
                    a1 <- arithExprNested';
                    op <- choice 
                        $ map string ["<=","="];
                    a2 <- arithExprNested';
                    string ")";
                    let rel "<=" = LEq
                        rel "=" = Eq
                        in
                            return 
                            $ rel op a1 a2
                    }
                , do{
                    string "(";
                    a1 <- boolExprNested';
                    op <- string "&&";
                    a2 <- boolExprNested';
                    string ")";
                    return $ And a1 a2
                    }]

commandsProb1 :: Either ParseError Command
commandsProb1 = runWith commandsExpr' "skip;skit := (a+1);skip"
commandsProb2 :: Either ParseError Command
commandsProb2 = runWith commandsExpr' "skip; while(a=3)do{b:=(b-1)};if true then {a:=1}else{a:=2};skip"
commandsProb21 = runWith commandsExpr' "skip; while(a=3)do{b:=(b-1)};if true then {a:=1}else{a:=2};skip"
commandsProb31 = runWith commandsExpr'
    "while ( b <= r ) do { q := (q + 1); r := (r - b)}; if  (! (r = 0))  then {res := 0} else {res := 1}"
commandsProb3 = runWith commandsExpr'
    "r := a; while ( b <= r ) do { q := (q + 1); r := (r - b)}; if ( ! (r = 0) ) then {res := 0} else {res := 1}"
commandsProb4 = runWith commandsExpr'
    "i:=0;r:= 0;while(i<=n)do{r:=(r+i);i:=(i+1)}"

commandsExpr' :: Parser Command  
commandsExpr' = choice 
            $ map try 
                [ return Seq 
                    <*> commandExpr' 
                    <*  string ";" 
                    <*> commandsExpr'
                , commandExpr']

commandProb1 :: Either ParseError Command
commandProb1 = runWith commandExpr' "if true then {a:=1}else{a:=2}"

commandExpr' :: Parser Command  
commandExpr'  = choice 
            $ map try 
                [ return CHole <* string "Hole"
                , return Skip <* string "skip"
                , assign'
                , return While 
                    <*  string "while"
                    <*> boolExprNested'
                    <*  string "do{"
                    <*> commandsExpr'
                    <*  string "}"
                , return If 
                    <*  string "if"
                    <*> boolExprNested'
                    <*  string "then{"
                    <*> commandsExpr'
                    <*  string "}else{"
                    <*> commandsExpr'
                    <*  string "}"
                ]

assign' :: Parser Command 
assign' = do
    (Var v) <- var'
    string ":="
    a <- arithExprNested'
    return $ Assign v a






-- | helper-function for the often occuring ideom of parsing parts in parenthesis
inParen' :: ParsecT String u Identity a -> ParsecT String u Identity a
inParen' = between (string "(") (string ")")

-- | helper-function for the often occuring ideom of parsing parts in braces
inBrace' :: ParsecT String u Identity a -> ParsecT String u Identity a
inBrace' = between (string "{") (string "}")
