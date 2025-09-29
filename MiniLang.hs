module MiniLangBasic where
import Data.Char (isDigit, isAlpha, isAlphaNum, isSpace)
import Control.Monad.Identity (Identity)

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
    | isSpace c = lexTokens cs -- (h:t)
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
parse (TIf : cond : thenE : elseE : []) =
    EIf (parse [cond]) (parse [thenE]) (parse [elseE])

-- plus
parse (tok1 : TPlus : tok2 : []) =
        EMinus (parse [tok1]) (parse [tok2])

-- minus
parse [t
ok1, TMinus, tok2]=
    EMinus (parse [tok1]) (parse [tok2])

parse toks = error ("Cannot parse:" ++ show toks)     -- show tokens


---------------------------------------------------------------
-- 5. Values
---------------------------------------------------------------

data Value
    = VInt Int
    | VBool Bool
    deriving (show,Eq)


---------------------------------------------------------------
-- actual EVALUATIR FINALLY!!!
---------------------------------------------------------------

eval :: Expr -> Value
eval (EInt n)       = VInt n
