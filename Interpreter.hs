module Interpreter where

import qualified Data.Map as Map
import Scanner
import Parser
import Expressions
import Tokens
import Data.Char (isDigit)

-- Values in the language
data Value
    = Float Float
    | Bool Bool
    | Nil
    | String String
    deriving Eq

instance Show Value where
    show (Float f) = if f == fromIntegral (floor f) then show (floor f) else show f
    show (Bool True) = "true"
    show (Bool False) = "false"
    show Nil = "nil"
    show (String s) = s

-- Environment for variables
type Env = [Map.Map String Value]

-- State for interpretern
data InterpreterState = InterpreterState
    { env :: Env           -- Variabelmiljö
    , outputs :: [String]  -- Lista över utskrifter
    }

-- Initial state
initialState :: InterpreterState
initialState = InterpreterState { env = [Map.empty], outputs = [] }

-- Main interpreting function
interpret :: ParseTree -> Either String [String]
interpret (ParseTree decls) = do
    let initial = initialState
    finalState <- evalDeclarations decls initial
    return (reverse $ outputs finalState)

-- Run a list of declarations
evalDeclarations :: [Declaration] -> InterpreterState -> Either String InterpreterState
evalDeclarations [] state = Right state
evalDeclarations (decl:decls) state = do
    newState <- evalDeclaration decl state
    evalDeclarations decls newState

-- Run a single declaration
evalDeclaration :: Declaration -> InterpreterState -> Either String InterpreterState
evalDeclaration (VarDecl name maybeExpr) state = do
    (value, newState) <- case maybeExpr of
        Just expr -> evalExpr expr state
        Nothing -> Right (Nil, state)
    let currentEnv = head (env newState)
        newEnv = Map.insert (showToken name) value currentEnv
    Right newState { env = newEnv : tail (env newState) }
evalDeclaration (FunDecl _ _ _) state = Right state
evalDeclaration (Statement stmt) state = evalStmt stmt state

-- Evaluate a statement
evalStmt :: Stmt -> InterpreterState -> Either String InterpreterState
evalStmt (Expression expr) state = do
    (_, newState) <- evalExpr expr state
    Right newState
evalStmt (Print expr) state = do
    (value, newState) <- evalExpr expr state
    Right newState { outputs = show value : outputs newState }
evalStmt (If cond thenBranch maybeElse) state = do
    (condVal, state1) <- evalExpr cond state
    if toBool condVal
        then evalStmt thenBranch state1
        else case maybeElse of
            Just elseBranch -> evalStmt elseBranch state1
            Nothing -> Right state1
evalStmt (While cond body) state = evalWhile cond body state
evalStmt (For init cond update body) state = do
    state' <- case init of
        Just decl -> evalDeclaration decl state
        Nothing -> Right state
    let whileStmt = While (maybe (Literal TRUE_LIT) id cond) 
                          (Block [Statement body, Statement (Expression (maybe (Literal NIL_LIT) id update))])
    evalStmt whileStmt state'
evalStmt (Block decls) state = do
    let newEnv = Map.empty : env state  -- Nytt scope
    newState <- evalDeclarations decls (state { env = newEnv })
    Right newState { env = tail (env newState) }  -- Återställ scope
evalStmt (Return _) _ = Left "Return statement not supported outside functions"

-- While-loop
evalWhile :: Expr -> Stmt -> InterpreterState -> Either String InterpreterState
evalWhile cond body state = do
    (condVal, state1) <- evalExpr cond state
    if toBool condVal
        then do
            newState <- evalStmt body state1
            evalWhile cond body newState
        else Right state1

-- Evaluate an expression
evalExpr :: Expr -> InterpreterState -> Either String (Value, InterpreterState)
evalExpr (Literal lit) state = Right (literalToValue lit, state)
evalExpr (Variable name) state = do
    value <- lookupVar (showToken name) (env state)
    Right (value, state)
evalExpr (Assign target valueExpr) state = do
    (value, newState) <- evalExpr valueExpr state
    let name = showToken target
    case updateVar name value (env newState) of
        Just newEnv -> Right (value, newState { env = newEnv })
        Nothing -> Left $ "Undefined variable: " ++ name
evalExpr (Binary left op right) state = do
    (l, state1) <- evalExpr left state
    (r, state2) <- evalExpr right state1
    value <- binaryOp (showToken op) l r
    Right (value, state2)
evalExpr (Logical left op right) state = do
    (l, state1) <- evalExpr left state
    case showToken op of
        "or" -> if toBool l then Right (l, state1) else evalExpr right state1
        "and" -> if toBool l then evalExpr right state1 else Right (l, state1)
        _ -> Left $ "Unknown logical operator: " ++ showToken op
evalExpr (Unary op expr) state = do
    (val, newState) <- evalExpr expr state
    value <- unaryOp (showToken op) val
    Right (value, newState)
evalExpr (Grouping expr) state = evalExpr expr state
evalExpr (Call _ _ _) _ = Left "Function calls not supported in this version"

-- Helper functions
literalToValue :: Literal -> Value
literalToValue (NUM n) = Float n
literalToValue (STR s) = String s
literalToValue TRUE_LIT = Bool True
literalToValue FALSE_LIT = Bool False
literalToValue NIL_LIT = Nil

toBool :: Value -> Bool
toBool (Bool b) = b
toBool Nil = False
toBool _ = True

toFloat :: Value -> Either String Float
toFloat (Float f) = Right f
toFloat _ = Left "Non-numerical value in numerical expression"

lookupVar :: String -> Env -> Either String Value
lookupVar name [] = Left $ "Identifier " ++ name ++ " not found"
lookupVar name (scope:rest) =
    case Map.lookup name scope of
        Just value -> Right value
        Nothing -> lookupVar name rest

updateVar :: String -> Value -> Env -> Maybe Env
updateVar name value [] = Nothing
updateVar name value (scope:rest) =
    if Map.member name scope
        then Just (Map.insert name value scope : rest)
        else do
            newRest <- updateVar name value rest
            Just (scope : newRest)

binaryOp :: String -> Value -> Value -> Either String Value
binaryOp "+" (Float l) (Float r) = Right (Float (l + r))
binaryOp "+" (String l) (String r) = Right (String (l ++ r))
binaryOp "+" _ _ = Left "Operands must be two numbers or two strings for +"
binaryOp "-" (Float l) (Float r) = Right (Float (l - r))
binaryOp "*" (Float l) (Float r) = Right (Float (l * r))
binaryOp "/" (Float l) (Float r) = 
    if r == 0 then Left "Division by zero" else Right (Float (l / r))
binaryOp "<" (Float l) (Float r) = Right (Bool (l < r))
binaryOp "<=" (Float l) (Float r) = Right (Bool (l <= r))
binaryOp ">" (Float l) (Float r) = Right (Bool (l > r))
binaryOp ">=" (Float l) (Float r) = Right (Bool (l >= r))
binaryOp "==" l r = Right (Bool (l == r))
binaryOp "!=" l r = Right (Bool (l /= r))
binaryOp "or" (Bool b1) (Bool b2) = Right (Bool (b1 || b2))
binaryOp "and" (Bool b1) (Bool b2) = Right (Bool (b1 && b2))
binaryOp _ _ _ = Left "Invalid binary operation"

unaryOp :: String -> Value -> Either String Value
unaryOp "-" (Float f) = Right (Float (-f))
unaryOp "!" val = Right (Bool (not $ toBool val))
unaryOp op _ = Left $ "Invalid unary operation: " ++ op

-- Run interpreter from file
runInterpreterFromFile :: FilePath -> IO ()
runInterpreterFromFile filename = do
    content <- readFile filename
    let tokens = scanTokens content
        parseTree = parse tokens
    case interpret parseTree of
        Left err -> putStrLn $ "Error: " ++ err
        Right outs -> mapM_ putStrLn outs