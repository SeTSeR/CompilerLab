module Main where


import Lib

import Data.Either
import Control.Monad.Except
import System.Environment
import System.Exit

parseArgs :: ExceptT String IO Config
parseArgs = do
    args <- lift getArgs
    case args of
        infile:outfile:_ -> return $ config infile outfile
        _ -> throwError "You must specify at least two arguments: source file and destination file"

main :: IO ()
main = do
    config <- runExceptT parseArgs
    case config of
        Left err -> putStrLn $ "Args parsing errors:\n" ++ err
        Right config -> do
            result <- runExceptT $ compile config
            either (\err -> putStrLn $ "Compilation errors:\n" ++ err) (\_ -> return ()) result