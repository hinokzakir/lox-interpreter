module Parser (parse) where
import Scanner (scanTokens)
import Tokens
import Expressions

-- Parse a file by reading and scanning it
runParserFromFile :: FilePath -> IO ()
runParserFromFile filepath = do
    contents <- readFile filepath
    let tokenList = scanTokens contents
    let result = parse tokenList
    print result

-- Main parsing function
parse :: [Token] -> ParseTree
parse tokens = 
    let (tree, remaining) = parseProgram tokens
    in case remaining of
        [TOKEN EOF _ _ _] -> tree
        [] -> tree
        _ -> error $ "Syntax error: unparsed tokens remaining: " ++ show remaining

-- Parse the entire program
parseProgram :: [Token] -> (ParseTree, [Token])
parseProgram tokens = 
    let (decls, rest) = parseDeclList tokens
    in (ParseTree decls, rest)

-- Parse a list of declarations
parseDeclList :: [Token] -> ([Declaration], [Token])
parseDeclList [] = ([], [])
parseDeclList (TOKEN EOF _ _ _:rest) = ([], rest)
parseDeclList tokens@(TOKEN RIGHT_BRACE _ _ _:_) = ([], tokens) -- Stop at RIGHT_BRACE, don’t consume it
parseDeclList tokens = 
    let (decl, rest1) = parseSingleDecl tokens
        (decls, rest2) = parseDeclList rest1
    in (decl:decls, rest2)

-- Parse a single declaration
parseSingleDecl :: [Token] -> (Declaration, [Token])
parseSingleDecl (TOKEN VAR _ _ _:rest) = parseVariableDecl rest
parseSingleDecl (TOKEN FUN _ _ _:rest) = parseFunctionDecl rest
parseSingleDecl tokens = 
    let (stmt, rest) = parseStmt tokens
    in (Statement stmt, rest)

-- Parse variable declaration
parseVariableDecl :: [Token] -> (Declaration, [Token])
parseVariableDecl tokens = 
    let (idTok, rest1) = expectToken IDENTIFIER "Expected variable name" tokens
    in case rest1 of
        TOKEN EQUAL _ _ _:rest2 -> 
            let (expr, rest3) = parseExpr rest2
                (semi, rest4) = expectToken SEMICOLON "Expected ';' after variable initialization" rest3
            in (VarDecl idTok (Just expr), rest4)
        TOKEN SEMICOLON _ _ _:rest2 -> (VarDecl idTok Nothing, rest2)
        _ -> error "Syntax error in variable declaration"

-- Parse function declaration
parseFunctionDecl :: [Token] -> (Declaration, [Token])
parseFunctionDecl tokens = 
    let (name, rest1) = expectToken IDENTIFIER "Expected function name" tokens
        (_, rest2) = expectToken LEFT_PAREN "Expected '(' after function name" rest1
        (params, rest3) = parseParamList rest2
        (_, rest4) = expectToken RIGHT_PAREN "Expected ')' after parameters" rest3
        (_, rest5) = expectToken LEFT_BRACE "Expected '{' before function body" rest4
        (body, rest6) = parseDeclList rest5
        (_, rest7) = expectToken RIGHT_BRACE "Expected '}' after function body" rest6
    in (FunDecl name params body, rest7)

-- Parse parameter list
parseParamList :: [Token] -> ([Token], [Token])
parseParamList (TOKEN RIGHT_PAREN _ _ _:rest) = ([], rest)
parseParamList tokens = 
    let (param, rest1) = expectToken IDENTIFIER "Expected parameter name" tokens
    in case rest1 of
        TOKEN COMMA _ _ _:rest2 -> 
            let (restParams, rest3) = parseParamList rest2
            in (param:restParams, rest3)
        TOKEN RIGHT_PAREN _ _ _:rest2 -> ([param], rest2)
        _ -> error "Syntax error in parameter list"

-- Parse statement
parseStmt :: [Token] -> (Stmt, [Token])
parseStmt (TOKEN IF _ _ _:rest) = parseIfStmt rest
parseStmt (TOKEN WHILE _ _ _:rest) = parseWhileStmt rest
parseStmt (TOKEN FOR _ _ _:rest) = parseForStmt rest
parseStmt (TOKEN PRINT _ _ _:rest) = parsePrintStmt rest
parseStmt (TOKEN RETURN _ _ _:rest) = parseReturnStmt rest
parseStmt (TOKEN LEFT_BRACE _ _ _:rest) = 
    let (stmt, rest') = parseBlockStmt rest -- Skip LEFT_BRACE here
    in (stmt, rest')
parseStmt tokens = parseExprStmt tokens

-- Parse if statement
parseIfStmt :: [Token] -> (Stmt, [Token])
parseIfStmt tokens = 
    let (_, rest1) = expectToken LEFT_PAREN "Expected '(' after 'if'" tokens
        (cond, rest2) = parseExpr rest1
        (_, rest3) = expectToken RIGHT_PAREN "Expected ')' after condition" rest2
        (thenStmt, rest4) = parseStmt rest3
        (elseStmt, rest5) = case rest4 of
            TOKEN ELSE _ _ _:rest' -> 
                let (stmt, rest'') = parseStmt rest'
                in (Just stmt, rest'')
            _ -> (Nothing, rest4)
    in (If cond thenStmt elseStmt, rest5)

-- Parse while statement
parseWhileStmt :: [Token] -> (Stmt, [Token])
parseWhileStmt tokens = 
    let (_, rest1) = expectToken LEFT_PAREN "Expected '(' after 'while'" tokens
        (cond, rest2) = parseExpr rest1
        (_, rest3) = expectToken RIGHT_PAREN "Expected ')' after condition" rest2
        (body, rest4) = parseStmt rest3
    in (While cond body, rest4)

-- Parse for statement
parseForStmt :: [Token] -> (Stmt, [Token])
parseForStmt tokens = 
    let (_, rest1) = expectToken LEFT_PAREN "Expected '(' after 'for'" tokens
        (init, rest2) = case rest1 of
            TOKEN SEMICOLON _ _ _:rest' -> (Nothing, rest')
            TOKEN VAR _ _ _:rest' -> 
                let (decl, rest'') = parseVariableDecl rest'
                in (Just decl, rest'')
            _ -> 
                let (stmt, rest') = parseExprStmt rest1
                in (Just (Statement stmt), rest')
        (cond, rest3) = case rest2 of
            TOKEN SEMICOLON _ _ _:rest' -> (Nothing, rest')
            _ -> 
                let (expr, rest') = parseExpr rest2
                    (_, rest'') = expectToken SEMICOLON "Expected ';' after condition" rest'
                in (Just expr, rest'')
        (update, rest4) = case rest3 of
            TOKEN RIGHT_PAREN _ _ _:rest' -> (Nothing, rest')
            _ -> 
                let (expr, rest') = parseExpr rest3
                    (_, rest'') = expectToken RIGHT_PAREN "Expected ')' after update" rest'
                in (Just expr, rest'')
        (body, rest5) = parseStmt rest4
    in (For init cond update body, rest5)

-- Parse print statement
parsePrintStmt :: [Token] -> (Stmt, [Token])
parsePrintStmt tokens = 
    let (expr, rest1) = parseExpr tokens
        (_, rest2) = expectToken SEMICOLON "Expected ';' after print" rest1
    in (Print expr, rest2)

-- Parse return statement
parseReturnStmt :: [Token] -> (Stmt, [Token])
parseReturnStmt tokens = 
    let (val, rest1) = case tokens of
            TOKEN SEMICOLON _ _ _:rest' -> (Nothing, rest')
            _ -> 
                let (expr, rest') = parseExpr tokens
                in (Just expr, rest')
        (_, rest2) = expectToken SEMICOLON "Expected ';' after return" rest1
    in (Return val, rest2)

-- Parse block statement
parseBlockStmt :: [Token] -> (Stmt, [Token])
parseBlockStmt tokens = 
    let (decls, rest1) = parseDeclList tokens
        (_, rest2) = expectToken RIGHT_BRACE "Expected '}' after block" rest1
    in (Block decls, rest2)

-- Parse expression statement
parseExprStmt :: [Token] -> (Stmt, [Token])
parseExprStmt tokens = 
    let (expr, rest1) = parseExpr tokens
        (_, rest2) = expectToken SEMICOLON "Expected ';' after expression" rest1
    in (Expression expr, rest2)

-- Expression parsing
parseExpr :: [Token] -> (Expr, [Token])
parseExpr tokens = 
    let (left, rest1) = parseAssignment tokens
    in (left, rest1)

parseAssignment :: [Token] -> (Expr, [Token])
parseAssignment tokens = 
    let (left, rest1) = parseOrExpr tokens
    in case rest1 of
        TOKEN EQUAL _ _ _:rest2 -> 
            case left of
                Variable var -> 
                    let (val, rest3) = parseAssignment rest2
                    in (Assign var val, rest3)
                _ -> error "Invalid assignment target"
        _ -> (left, rest1)

parseOrExpr :: [Token] -> (Expr, [Token])
parseOrExpr tokens = 
    let (left, rest1) = parseAndExpr tokens
    in case rest1 of
        (tok@(TOKEN OR _ _ _):rest2) -> 
            let (right, rest3) = parseAndExpr rest2
            in (Logical left tok right, rest3)
        _ -> (left, rest1)

parseAndExpr :: [Token] -> (Expr, [Token])
parseAndExpr tokens = 
    let (left, rest1) = parseEquality tokens
    in case rest1 of
        (tok@(TOKEN AND _ _ _):rest2) -> 
            let (right, rest3) = parseEquality rest2
            in (Logical left tok right, rest3)
        _ -> (left, rest1)

parseEquality :: [Token] -> (Expr, [Token])
parseEquality tokens = parseBinary parseComparison [EQUAL_EQUAL, BANG_EQUAL] tokens

parseComparison :: [Token] -> (Expr, [Token])
parseComparison tokens = parseBinary parseTerm [GREATER, GREATER_EQUAL, LESS, LESS_EQUAL] tokens

parseTerm :: [Token] -> (Expr, [Token])
parseTerm tokens = parseBinary parseFactor [PLUS, MINUS] tokens

parseFactor :: [Token] -> (Expr, [Token])
parseFactor tokens = parseBinary parseUnary [STAR, SLASH] tokens

parseUnary :: [Token] -> (Expr, [Token])
parseUnary (TOKEN MINUS _ _ ln:rest) = 
    let (expr, rest') = parseUnary rest
    in (Unary (TOKEN MINUS "-" NONE ln) expr, rest')
parseUnary (TOKEN BANG _ _ ln:rest) = 
    let (expr, rest') = parseUnary rest
    in (Unary (TOKEN BANG "!" NONE ln) expr, rest')
parseUnary tokens = parseCallExpr tokens

parseCallExpr :: [Token] -> (Expr, [Token])
parseCallExpr tokens = 
    let (primary, rest1) = parsePrimary tokens
    in parseCallTail primary rest1

parseCallTail :: Expr -> [Token] -> (Expr, [Token])
parseCallTail expr (TOKEN LEFT_PAREN _ _ ln:rest) = 
    let (args, rest1) = parseArgList rest
        (_, rest2) = expectToken RIGHT_PAREN "Expected ')' after arguments" rest1
        nextExpr = Call expr (TOKEN LEFT_PAREN "(" NONE ln) args
    in parseCallTail nextExpr rest2
parseCallTail expr rest = (expr, rest)

parseArgList :: [Token] -> ([Expr], [Token])
parseArgList (TOKEN RIGHT_PAREN _ _ _:rest) = ([], rest)
parseArgList tokens = 
    let (arg, rest1) = parseExpr tokens
    in case rest1 of
        TOKEN COMMA _ _ _:rest2 -> 
            let (restArgs, rest3) = parseArgList rest2
            in (arg:restArgs, rest3)
        _ -> ([arg], rest1)

parsePrimary :: [Token] -> (Expr, [Token])
parsePrimary (TOKEN TRUE _ _ _:rest) = (Literal TRUE_LIT, rest)
parsePrimary (TOKEN FALSE _ _ _:rest) = (Literal FALSE_LIT, rest)
parsePrimary (TOKEN NIL _ _ _:rest) = (Literal NIL_LIT, rest)
parsePrimary (TOKEN NUMBER _ n _:rest) = (Literal n, rest)
parsePrimary (TOKEN STRING _ s _:rest) = (Literal s, rest)
parsePrimary (tok@(TOKEN IDENTIFIER _ _ _):rest) = (Variable tok, rest)
parsePrimary (TOKEN LEFT_PAREN _ _ _:rest) = 
    let (expr, rest1) = parseExpr rest
        (_, rest2) = expectToken RIGHT_PAREN "Expected ')' after grouped expression" rest1
    in (Grouping expr, rest2)
parsePrimary tokens@(TOKEN t _ _ _:_) = 
    error $ "Unexpected token in primary expression: " ++ show t
parsePrimary tokens = 
    error $ "Unexpected token in expression: " ++ show (take 1 tokens)

-- Helper function for binary operators
parseBinary :: ([Token] -> (Expr, [Token])) -> [TokenType] -> [Token] -> (Expr, [Token])
parseBinary next ops tokens = 
    let (left, rest1) = next tokens
    in case rest1 of
        (tok@(TOKEN t _ _ _):rest2) | t `elem` ops -> 
            let (right, rest3) = next rest2
                newExpr = Binary left tok right
            in parseBinary' newExpr next ops rest3
        _ -> (left, rest1)

-- Helper function for continuing binary operations
parseBinary' :: Expr -> ([Token] -> (Expr, [Token])) -> [TokenType] -> [Token] -> (Expr, [Token])
parseBinary' left next ops tokens = 
    case tokens of
        (tok@(TOKEN t _ _ _):rest) | t `elem` ops -> 
            let (right, rest') = next rest
                newExpr = Binary left tok right
            in parseBinary' newExpr next ops rest'
        _ -> (left, tokens)

-- Helper function to expect and consume a specific token
expectToken :: TokenType -> String -> [Token] -> (Token, [Token])
expectToken expected msg (tok@(TOKEN t _ _ _):rest) 
    | t == expected = (tok, rest)
    | otherwise = error $ msg ++ ", got " ++ show t
expectToken _ msg [] = error $ msg ++ ", got end of input"