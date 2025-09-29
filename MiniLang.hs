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
