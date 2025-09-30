module MiniLang where
import Data.Char (isDigit, isAlpha, isAlphaNum, isSpace)
import Control.Monad.Identity (Identity)
import Data.Time.Format.ISO8601 (yearFormat)

---------------------------------------------------------------
-- 1. Creating tokens
---------------------------------------------------------------

data Token
    = TInt Int
    | TBool Bool
    | TVar String
    | TIf
    | TThen
    | TElse
    | TPlus
    | TMinus
    deriving (Show,Eq)

---------------------------------------------------------------
-- 2. Making a lexer
---------------------------------------------------------------

-- break string into tokens

lexTokens :: String -> [Token]
lexTokens [] = []
lexTokens (c:cs)
    | isSpace c = lexTokens cs -- (h:t) if c is a space SKIP it
    | isDigit c =
        let (digits, rest) = span isDigit (c:cs)
        in TInt (read digits) : lexTokens rest
    | isAlpha c =
        let (letters, rest) = span isAlphaNum (c:cs)
        in case letters of
            "if"    -> TIf          : lexTokens rest
            "else"  -> TElse        : lexTokens rest
            "True"  -> TBool True   : lexTokens rest
            "False" -> TBool False  : lexTokens rest
            _       -> TVar letters : lexTokens rest

    | c == '+' = TPlus : lexTokens cs
    | c == '-' = TMinus : lexTokens cs

    | otherwise = error ("Unexpected char:" ++ [c])

---------------------------------------------------------------
-- 3. Creating our own Ast tree
---------------------------------------------------------------
data Expr
    = EInt Int
    | EBool Bool
    | EVar String
    | EIf Expr Expr Expr
    | EPlus Expr Expr
    | EMinus Expr Expr
  deriving (Show, Eq)


---------------------------------------------------------------
-- 4. Parser
---------------------------------------------------------------
parse :: [Token] -> Expr
parse [TInt n]      = EInt n
parse [TBool b]     = EBool b
parse [TVar x]     = EVar x

-- if expressions -> if then else
parse [TIf, cond, thenE, elseE] =         --using literal list pattern
    EIf (parse [cond]) (parse [thenE]) (parse [elseE])

-- plus
parse [tok1, TPlus, tok2] =
    EPlus (parse [tok1]) (parse [tok2])

-- minus
parse [tok1, TMinus, tok2]=
    EMinus (parse [tok1]) (parse [tok2])

parse toks = error ("Cannot parse:" ++ show toks)     -- show tokens


---------------------------------------------------------------
-- 5. Values defined for int ans bool
---------------------------------------------------------------

data Value
    = VInt Int
    | VBool Bool
    deriving (Show,Eq)


---------------------------------------------------------------
-- EVALUATIR
---------------------------------------------------------------
--Bool
eval :: Expr -> Value
eval (EInt n)       = VInt n
eval (EBool b)      = VBool b
eval (EVar x)       = error ("Unbound variable: " ++ x)
eval (EIf cond t e) = 
    case eval cond of                              -- Covered in class today also read slides completely

        VBool True   -> eval t
        VBool False  -> eval e
        _            -> error "Condition must be a Boolean Value"

--Arithmetic
eval (EPlus e1 e2) =
    case (eval e1, eval e2) of
        (VInt n1, VInt n2) -> VInt (myAdd n1 n2)
        _                  -> error "Type error in addition"

eval (EMinus e1 e2) =
    case (eval e1, eval e2) of
        (VInt n1, VInt n2)    -> VInt (mySub n1 n2)
        _                     -> error "Type error in subtraction"

---------------------------------------------------------------
-- Arithmetic function definitions
---------------------------------------------------------------

myAdd :: Int -> Int -> Int
myAdd x y = x + y

mySub :: Int -> Int -> Int
mySub x y = x - y

---------------------------------------------------------------
-- Helper functions defined
---------------------------------------------------------------

-- run function for parsing and building AST:
run :: String -> Expr
run = parse . lexTokens

-- parse + eval function:
runEval :: String -> Value
runEval = eval . run

