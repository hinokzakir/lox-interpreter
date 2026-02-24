module Main where

import Interpreter (runInterpreterFromFile)
import System.Environment (getArgs)

-- Main function
main :: IO ()
main = do
    arguments <- getArgs
    case arguments of
        [file] -> runLoxFile file
        _ -> putStrLn "Usage: lox filename.lox"

-- Helper to run the interpreter on a file
runLoxFile :: FilePath -> IO ()
runLoxFile path = runInterpreterFromFile path