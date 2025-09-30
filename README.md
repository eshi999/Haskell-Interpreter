|| HASKELL INTERPRETER ||

A small functional interpreter written in Haskell using first principles.
It includes simple lex (lexer) functions and a parser.


Flowchart for a working interpreter:
[Lexical analysis] --> [Syntactic analysis] --> **** (expr) --> [Evaluation ] --> [Output ]

Checkpoint-1. September 30th
We process code in three main steps:

Lexing → Turn raw input (String) into tokens.

Parsing → Convert tokens into an Abstract Syntax Tree (AST).

Evaluation → Walk the AST to compute a result.

Datatypes and functions created and used in the code:

1. Tokens (data Token)

TInt Int → integers

TBool Bool → booleans (True, False)

TVar String → variables

TIf, TElse → keywords

TPlus, TMinus → operators

2. Lexer (lexTokens)

Skips spaces.

Groups digits into TInt.

Recognizes booleans, if/else, variables.

Maps + → TPlus, - → TMinus.

3. AST (data Expr)

EInt, EBool, EVar → literals / variables.

EIf Expr Expr Expr → if-then-else.

EPlus Expr Expr, EMinus Expr Expr → binary arithmetic.

4. Parser (parse)

Turns token lists into AST.

Handles single values, if-expressions, +, -.

5. Values (data Value)

VInt Int

VBool Bool

6. Evaluator (eval)

Evaluates AST → Value.

If-expressions choose a branch based on condition.

Arithmetic uses custom functions:

myAdd → addition

mySub → subtraction

7. Run Helpers

run :: String -> Expr → string → AST.

runEval :: String -> Value → string → evaluated result.
