module MiniLang where
import Data.Char (isDigit, isAlpha, isAlphaNum, isSpace)
import Control.Monad.Identity (Identity)
import Data.Time.Format.ISO8601 (yearFormat)
import System.Console.Terminfo (restoreDefaultColors)

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
    | TTimes
    | TDiv
    | TEq
    | TNotEq
    | TLess
    | TGreater
    | TLet
    | TAssign
    | TLeftP
    | TRightP
    | TSemicolon

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
            "let"   -> TLet         : lexTokens rest
            "True"  -> TBool True   : lexTokens rest
            "False" -> TBool False  : lexTokens rest
            _       -> TVar letters : lexTokens rest

    | c == '+' = TPlus : lexTokens cs
    | c == '-' = TMinus : lexTokens cs
    | c == '*' = TTimes : lexTokens cs
    | c == '/' = TDiv   : lexTokens cs
    -- check for two-character operators first
    | c == '=' && not (null cs) && head cs == '=' = TEq : lexTokens (tail cs)
    | c == '!' && not (null cs) && head cs == '=' = TNotEq : lexTokens (tail cs)
    | c == '=' = TAssign : lexTokens cs
    | c == '<' = TLess : lexTokens cs
    | c == '>' = TGreater : lexTokens cs
    | c == '(' = TLeftP : lexTokens cs
    | c == ')' = TRightP : lexTokens cs
    | c == ';' = TSemicolon : lexTokens cs

    | otherwise = error ("Unexpected char:" ++ [c])

---------------------------------------------------------------
-- 3. Creating our own Ast tree
---------------------------------------------------------------
data Expr = EInt Int
    | EBool Bool
    | EVar String
    -- left and right p??
    | EIf Expr Expr Expr
    | EPlus Expr Expr
    | EMinus Expr Expr
    | ETimes Expr Expr
    | EDiv Expr Expr
    | EEq Expr Expr
    | ENeq Expr Expr
    | ELt Expr Expr
    | EGt Expr Expr
    | ELet Expr Expr
    | ESemicolonSequence Expr Expr

  deriving (Show, Eq)
-- all these give bool ans
-- like Equal? True, or NotEqual? False
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

-- equal to
parse [tok1, TEq, tok2] = 
    EEq  (parse [tok1]) (parse [tok2])

-- not equal to
parse [tok1, TNotEq, tok2] = 
    ENeq (parse [tok1]) (parse [tok2])

-- less than
parse [tok1, TLess, tok2] = 
    ELt  (parse [tok1]) (parse [tok2])

-- greater than
parse [tok1, TGreater, tok2] = 
    EGt  (parse [tok1]) (parse [tok2])

-- pattern match on [TLet, TVar x, TAssign, expr1]
parse [TLet, TVar x, TAssign, val ] =
    ELet (parse [TVar x]) (parse [val])
parse toks =
  case splitOnSemicolon toks of
    [stmt] -> fst (parseExpr 0 stmt)
    (stmt:rest) -> ESemicolonSequence (fst (parseExpr 0 stmt)) (parse (concat rest))
    [] -> error "Empty input"

-- Split a list of tokens into sublists, divided by semicolons
splitOnSemicolon :: [Token] -> [[Token]]
splitOnSemicolon [] = []
splitOnSemicolon toks =
  let (stmt, rest) = break (== TSemicolon) toks
  in case rest of
       [] -> [stmt]
       (_ : rest') -> stmt : splitOnSemicolon rest'



-- these were fixed length parses
-- now for any length token list:
parseExprVar :: [Token] -> Expr
parseExprVar [] = error "cant have an empty input"
parseExprVar [TInt n] = EInt n
parseExprVar (TInt n : TPlus : rest)= -- TPLUS
    EPlus (EInt n) (parseExprVar rest)
parseExprVar (TInt n : TMinus : rest)= -- TMinus
    EPlus (EInt n) (parseExprVar rest)
parseExprVar (TInt n : TTimes : rest)= -- TTimes
    EPlus (EInt n) (parseExprVar rest)
parseExprVar (TInt n : TDiv : rest)= -- TDiv
    EPlus (EInt n) (parseExprVar rest)

-- how can we pattern match more generally??
-- what if you have longer than 2 or 3 lists
    -- cases make sense to implement 

-- now defining the PRATT PARSER OPERATOR TABLE THINGY
parseExpr :: Int -> [Token] -> (Expr, [Token])
parseExpr minBP tokens =
    let (lhs,rest) = parseAtom tokens
    in loop lhs rest
    where
        loop lhs toks =
            case toks of 
                (op : toks')
                    | lbp op >= minBP -> 
                        let (rhs, toks'') = parseExpr (rbp op) toks' 
                        in loop (makeExpr op lhs rhs) toks'' 
                _ -> (lhs, toks)

makeExpr :: Token -> Expr -> Expr -> Expr
makeExpr TPlus   = EPlus
makeExpr TMinus  = EMinus
makeExpr TTimes  = ETimes
makeExpr TDiv    = EDiv
makeExpr TEq     = EEq
makeExpr TNotEq  = ENeq
makeExpr TLess   = ELt
makeExpr TGreater= EGt
makeExpr _       = \_ _ -> error "unknown operator in makeExpr"

lbp TPlus = 10
lbp TMinus = 10
lbp TTimes = 20
lbp TDiv = 20
-- lbp TLeftP = 30
-- lbp TRightP = 30
lbp _ = 0

rbp = lbp

parseAtom (TInt n : rest) = (EInt n, rest)
parseAtom (TBool b : rest) = (EBool b, rest)
parseAtom (TVar x : rest) = (EVar x, rest)
parseAtom (TLeftP : rest)=
    let (expr, rest') = parseExpr 0 rest
    in case rest' of
         (TRightP : rest'') -> (expr, rest'')
         _ -> error "missing closing parenthesis"
parseAtom [] = error "unexpected end of input"
parseAtom _  = error "invalid atom"

---------------------------------------------------------------
-- 5. Values defined for int and bool
---------------------------------------------------------------

data Value
    = VInt Int
    | VBool Bool
    deriving (Show,Eq)


---------------------------------------------------------------
-- EVALUATOR
---------------------------------------------------------------
--Bool
eval :: Expr -> Value
eval (EInt n)       = VInt n
eval (EBool b)      = VBool b
eval (EVar x)       = error ("Unbound variable: " ++ x)
eval (EIf cond t e) = 
    case eval cond of                              

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

eval (ETimes e1 e2) =
    case (eval e1, eval e2) of
        (VInt n1, VInt n2)    -> VInt (myTimes n1 n2)
        _                     -> error "Type error in multiplication"

eval (EDiv e1 e2) =
    case (eval e1, eval e2) of
        (VInt n1, VInt n2)    -> VInt (myDiv n1 n2)
        _                     -> error "Type error in Division"


eval (EEq e1 e2) =
    case (eval e1, eval e2) of
        (VInt n1, VInt n2)    -> VBool (n1 == n2)
        (VBool b1, VBool b2)  -> VBool (b1 == b2)
        _                     -> error "Type error in =="

eval (ENeq e1 e2) =
    case (eval e1, eval e2) of
        (VInt n1, VInt n2)    -> VBool (n1 /= n2)
        (VBool b1, VBool b2)  -> VBool (b1 /= b2)
        _                     -> error "Type error in !="

eval (ELt e1 e2) =
    case (eval e1, eval e2) of
        (VInt n1, VInt n2)    -> VBool (n1 < n2)
        _                     -> error "Type error in <"

eval (EGt e1 e2) =
    case (eval e1, eval e2) of
        (VInt n1, VInt n2)    -> VBool (n1 > n2)
        _                     -> error "Type error in >"


eval (ELet name valExpr) =
    let val = eval valExpr
        env = [(name, val)]
    in evalWithEnv env


-- factor to also have an env
---------------------------------------------------------------
-- Arithmetic function definitions
---------------------------------------------------------------

myAdd :: Int -> Int -> Int
myAdd x y = x + y

mySub :: Int -> Int -> Int
mySub x y = x - y

myTimes :: Int -> Int -> Int
myTimes x y = x * y

myDiv :: Int -> Int -> Int
myDiv x y = x `div` y



type Env =[(String, Value)]

-- Evaluate an expression with a given environment (used for let bindings)
evalWithEnv :: Expr -> Env -> Value
evalWithEnv (EInt n) _ = VInt n
evalWithEnv (EBool b) _ = VBool b
-- ************ figure this out
evalWithEnv (EVar x) env =
    case lookup x env of
        Just v  -> v -- uses stored value of variable
        Nothing -> error ("Unbound variable: " ++ x)

evalWithEnv (ESemicolonSequence e1 e2) env =
    let _ = evalWithEnv e1 env
        val = evalWithEnv e2 env
    in val

-- Arithmetic
evalWithEnv (EPlus e1 e2) env =
    case (evalWithEnv e1 env, evalWithEnv e2 env) of
        (VInt n1, VInt n2) -> VInt (n1 + n2)
        _ -> error "Type error in +"

evalWithEnv (EMinus e1 e2) env =
    case (evalWithEnv e1 env, evalWithEnv e2 env) of
        (VInt n1, VInt n2) -> VInt (n1 - n2)
        _ -> error "Type error in -"

evalWithEnv (ETimes e1 e2) env =
    case (evalWithEnv e1 env, evalWithEnv e2 env) of
        (VInt n1, VInt n2) -> VInt (n1 * n2)
        _ -> error "Type error in *"

evalWithEnv (EDiv e1 e2) env =
    case (evalWithEnv e1 env, evalWithEnv e2 env) of
        (VInt n1, VInt n2) -> VInt (n1 `div` n2)
        _ -> error "Type error in /"


evalWithEnv (ELet (EVar name) valExpr) env =
    let val = evalWithEnv valExpr env
        newEnv = (name, val) : env
    in evalWithEnv body newEnv

evalWithEnv (ESemicolonSequence e1 e2) env =
    let newEnv = updateEnv e1 env
    in evalWithEnv e2 newEnv -- recursively call

updateEnv :: Expr -> Env -> Env
updateEnv (ELet (EVar name) valExpr) env =
    let val = evalWithEnv valExpr env
    in (name, val) : env
updateEnv _ env = env

---------------------------------------------------------------
-- Helper functions
---------------------------------------------------------------

-- run function for parsing and building AST:
run :: String -> Expr
run = parse . lexTokens -- so it js shows u the tokens it created

-- parse + eval function:
runEval :: String -> Value
runEval = eval . run   -- you evaluate the tokens u parsed thru




-- arithmetic + -  * 'div keep it integer division for now so we can see the whoel pipeline

-- conditionsls ==, != , < > etc
    -- syntax errors, when tokens don;t have smth to match w
    -- up\ can change the syntax of whatever you want too, like /= instead of !=

-- assignment x= 42 etc

-- helpful to sraer all asggignment stamenents with let, so the token recognizes let and kjnoiws its an assignment statemnet
-- pattern match the firt tokem, if its a let token, you know its assignment


-- opwraator precedence
-- 7 * 10 stickier than - 1


-- pratt parsers are a cnice way to coe up with a solution for this IN ONE LINEAR PASS

-- also thing called recursive descent another wa of doing it




--basically a lookup table for operator precedence, like rank all o fhem

-- to do by next deadline

--------------------------------------------------

-- lists and tuples


-- how you tihink yout, how do yopu separate statememnts, maybe by using a semicolon, add a semicolon token, eg:
-- x = 42 ; 
-- y = 43;
-- so when a token comes accross it it just sees a ; token and parses accrows it in one line


-- kinda like a calculator with memory--  rebind names??
-- refactor passes


-- recursive descent is the easier parser technique
-- helper functions for parsing the hwole list of tokens


-- pratt parsers are a cnice way to coe up with a solution for this IN ONE LINEAR PASS

-- also thing called recursive descent another wa of doing it




--basically a lookup table for operator precedence, like rank all o fhem


-- later:
-- functions. how does someone define a function