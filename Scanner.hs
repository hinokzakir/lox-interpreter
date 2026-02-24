module Scanner (scanTokens) where

import Data.Char 
import Data.Maybe

import System.IO

import qualified Data.Map as Map
import Tokens

-- Keyword mapping
keywords :: Map.Map String TokenType
keywords = Map.fromList
    [ ("and", AND), ("class", CLASS), ("else", ELSE), ("false", FALSE)
    , ("for", FOR), ("fun", FUN), ("if", IF), ("nil", NIL), ("or", OR)
    , ("print", PRINT), ("return", RETURN), ("super", SUPER)
    , ("this", THIS), ("true", TRUE), ("var", VAR), ("while", WHILE)
    ]

-- Scanner state
data ScannerState = ScannerState
    { source  :: String  -- Input string
    , tokens  :: [Token] -- List of tokens
    , start   :: Int     -- Start of the lexeme
    , current :: Int     -- Current position in the input
    , line    :: Int     -- Current line number
    }

-- Main scan function
scanTokens :: String -> [Token]
scanTokens input = tokens (scan (ScannerState input [] 0 0 1))

-- Function to run the scanner with a .lox file to read.  Usage: runScannerFromFile "filename.lox"
runScannerFromFile :: FilePath -> IO ()
runScannerFromFile filename = do
    content <- readFile filename
    let tokens = scanTokens content
    mapM_ print tokens

scan :: ScannerState -> ScannerState
scan state
    | isAtEnd state = state { tokens = tokens state ++ [TOKEN EOF "" NONE (line state)] }
    | otherwise =
        let newState = scanToken state { start = current state }
        in scan newState

-- Check if we are at the end of the input
isAtEnd :: ScannerState -> Bool
isAtEnd state = current state >= length (source state)

-- Scan a single token
scanToken :: ScannerState -> ScannerState
scanToken state =
    let (char, newState) = advance state
    in case char of
        '(' -> addToken LEFT_PAREN newState
        ')' -> addToken RIGHT_PAREN newState
        '{' -> addToken LEFT_BRACE newState
        '}' -> addToken RIGHT_BRACE newState
        ',' -> addToken COMMA newState
        '.' -> addToken DOT newState
        '-' -> addToken MINUS newState
        '+' -> addToken PLUS newState
        ';' -> addToken SEMICOLON newState
        '*' -> addToken STAR newState
        '/' -> handleSlash newState
        '!' -> if match '=' newState
          then addToken BANG_EQUAL (snd (advance newState))
          else addToken BANG newState
        '=' -> if match '=' newState
          then addToken EQUAL_EQUAL (snd (advance newState))
          else addToken EQUAL newState
        '<' -> if match '=' newState
          then addToken LESS_EQUAL (snd (advance newState))
          else addToken LESS newState
        '>' -> if match '=' newState
          then addToken GREATER_EQUAL (snd (advance newState))
          else addToken GREATER newState

        '"' -> handleString newState
        ' '  -> newState
        '\r' -> newState
        '\t' -> newState
        '\n' -> newState { line = line newState + 1 }
        _    | isDigit char -> handleNumber newState
             | isIdentifierChar char -> handleIdentifier newState
             | otherwise    -> scannerError state char  -- Handle unexpected characters

scannerError :: ScannerState -> Char -> a
scannerError state char =
    error $ "Scanner error line " ++ show (line state) ++ " - '" ++ [char] ++ "' is not a valid TokenType"
-- Advance the current position
advance :: ScannerState -> (Char, ScannerState)
advance state = (source state !! current state, state { current = current state + 1 })

-- Peek at the next character without consuming it
peek :: ScannerState -> Char
peek state
    | isAtEnd state = '\0'
    | otherwise = source state !! current state

peekNext :: ScannerState -> Char
peekNext state
    | current state + 1 >= length (source state) = '\0'
    | otherwise = source state !! (current state + 1)

-- Match expected character and advance if true
match :: Char -> ScannerState -> Bool
match expected state
    | isAtEnd state = False
    | source state !! current state /= expected = False
    | otherwise = True

-- Add token to the token list
addToken :: TokenType -> ScannerState -> ScannerState
addToken tokenType state = state { tokens = tokens state ++ [TOKEN tokenType (getLexeme state) NONE (line state)] }

addTokenWithLiteral :: TokenType -> Literal -> ScannerState -> ScannerState
addTokenWithLiteral tokenType lit state = state { tokens = tokens state ++ [TOKEN tokenType (getLexeme state) lit (line state)] }

getLexeme :: ScannerState -> String
getLexeme state = take (current state - start state) (drop (start state) (source state))

-- Handle comments and slash
handleSlash :: ScannerState -> ScannerState
handleSlash state
    | match '/' state = skipComment state
    | otherwise = addToken SLASH state

skipComment :: ScannerState -> ScannerState
skipComment state
    | peek state == '\n' || isAtEnd state = state
    | otherwise = skipComment (state { current = current state + 1 })

-- Handle string literals
handleString :: ScannerState -> ScannerState
handleString state =
    let newState = consumeUntil '"' state
    in if isAtEnd newState
        then error "Unterminated string."
        else addTokenWithLiteral STRING (STR (tail (getLexeme newState))) (newState { current = current newState + 1 })

-- Consume characters until a specific character is found
consumeUntil :: Char -> ScannerState -> ScannerState
consumeUntil ch state
    | isAtEnd state || peek state == ch = state
    | otherwise = consumeUntil ch (state { current = current state + 1, line = if peek state == '\n' then line state + 1 else line state })

-- Handle number literals
handleNumber :: ScannerState -> ScannerState
handleNumber state =
    let newState = consumeWhile isDigit state
        newState' = if peek newState == '.' && isDigit (peekNext newState)
                      then consumeWhile isDigit (newState { current = current newState + 1 })
                      else newState
    in addTokenWithLiteral NUMBER (NUM (read (getLexeme newState') :: Float)) newState'

consumeWhile :: (Char -> Bool) -> ScannerState -> ScannerState
consumeWhile cond state
    | isAtEnd state || not (cond (peek state)) = state
    | otherwise = consumeWhile cond (state { current = current state + 1 })

-- Handle identifiers and keywords
handleIdentifier :: ScannerState -> ScannerState
handleIdentifier state =
    let newState = consumeWhile isIdentifierCharNum state
        text = getLexeme newState
        tokenType = Map.findWithDefault IDENTIFIER text keywords
    in addToken tokenType newState

-- Function to identify Identifier char such as Alpha or '_'
isIdentifierChar :: Char -> Bool
isIdentifierChar c = isAlpha c || c == '_'


-- Function to identify Identifier char such as AlphaNumeric or '_'
isIdentifierCharNum :: Char -> Bool
isIdentifierCharNum c = isIdentifierChar c || isDigit c