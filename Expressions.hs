module Expressions where

import Tokens
import Data.List (intercalate)

-- Declaration types
data Declaration
    = FunDecl Token [Token] [Declaration]  -- Function with name, parameters, and body
    | VarDecl Token (Maybe Expr)           -- Variable with optional expression
    | Statement Stmt                       -- Wrapped statement

instance Show Declaration where
    show decl = case decl of
        FunDecl name params body -> 
            "F DEC -> " ++ showToken name ++ "(" ++ 
            intercalate "," (map showToken params) ++ "){" ++ 
            concatMap ((++ " ") . show) body ++ "}"
        VarDecl token (Just expr) -> 
            "V DEC -> " ++ showToken token ++ "=" ++ show expr ++ ";"
        VarDecl token Nothing -> 
            "V DEC ->" ++ showToken token ++ ";"
        Statement stmt -> show stmt

-- Statement constructs
data Stmt
    = Expression Expr                    -- Expression statement
    | For (Maybe Declaration) (Maybe Expr) (Maybe Expr) Stmt  -- For loop
    | If Expr Stmt (Maybe Stmt)          -- If statement
    | Print Expr                         -- Print command
    | Return (Maybe Expr)                -- Return statement
    | While Expr Stmt                    -- While loop
    | Block [Declaration]                -- Block of declarations

instance Show Stmt where
    show stmt = case stmt of
        Expression expr -> "" ++ show expr ++ ";"
        For init cond update body -> 
            "for (" ++ maybe "" show init ++ "; " ++ 
            maybe "" show cond ++ "; " ++ 
            maybe "" show update ++ ") " ++ show body
        If cond thenBranch elseBranch -> 
            "if (" ++ show cond ++ ") " ++ show thenBranch ++ 
            maybe "" ((" else " ++) . show) elseBranch
        Print expr -> "print " ++ show expr ++ ";"
        Return expr -> "return" ++ maybe "" ((" " ++) . show) expr ++ ";"
        While cond body -> "while (" ++ show cond ++ ") " ++ show body
        Block decls -> "{" ++ concatMap ((" " ++) . show) decls ++ "}"

-- Expression constructs
data Expr
    = Assign Token Expr                  -- Assignment expression
    | Logical Expr Token Expr            -- Logical operation
    | Binary Expr Token Expr             -- Binary operation
    | Call Expr Token [Expr]             -- Function call
    | Grouping Expr                      -- Grouped expression
    | Literal Literal                    -- Literal value
    | Unary Token Expr                   -- Unary operation
    | Variable Token                     -- Variable reference

instance Show Expr where
    show expr = case expr of
        Assign target value -> "" ++ showToken target ++ " = " ++ show value
        Logical left op right -> "" ++ show left ++ " " ++ showToken op ++ " " ++ show right
        Binary left op right -> "" ++ show left ++ " " ++ showToken op ++ " " ++ show right
        Call callee _ args -> "" ++ show callee ++ "(" ++ intercalate ", " (map show args) ++ ")"
        Grouping expr -> "(" ++ show expr ++ ")"
        Literal lit -> "" ++ showLiteral lit
        Unary op expr -> "" ++ showToken op ++ show expr
        Variable name -> "" ++ showToken name

-- Parse tree definition
data ParseTree = ParseTree [Declaration]

instance Show ParseTree where
    show (ParseTree decls) = 
        "Parse Tree:\n" ++ concatMap ((++ "\n") . ("  " ++) . show) decls

-- Helper functions for display
showLiteral :: Literal -> String
showLiteral lit = case lit of
    STR s -> "\"" ++ s ++ "\""
    NUM n -> show n
    TRUE_LIT -> "TRUE_LIT"
    FALSE_LIT -> "FALSE_LIT"
    NIL_LIT -> "NIL_LIT"

showToken :: Token -> String
showToken (TOKEN _ lexeme _ _) = lexeme