module MiniLangBasic where
import Data.Char (isDigit, isAlpha, isAlphaNum, isSpace)

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
    | TOp String
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

    | c == '+' = TOp "+" : lexTokens cs
    | c == '-' = TOp "-" : lexTokens cs

    | otherwise = error ("Unexpected char:" ++ [c])

