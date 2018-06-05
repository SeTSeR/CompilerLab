module Lib
    ( compile
    , config
    , Config
    ) where

import Frontend
import Backend

import Control.Monad.Except
import Data.Either
import Data.Functor.Identity
import Text.Read

data Config = Config
            { inputfile :: String
            , outfile :: String
            }

config :: String -> String -> Config
config infile outfile = Config
                        { inputfile = infile
                        , outfile = outfile
                        }

compile :: Config -> ExceptT String IO ()
compile config = do
    tocompile <- liftIO $ readFile $ inputfile config
    let str:functions = lines tocompile
    borders <- toExceptT $ borders str
    functions <- toExceptT $ foldr collect (return []) functions
    let derivs = map derivative functions
    liftIO $ writeFile (outfile config) $ genCode borders functions derivs

toExceptT :: (Show a) => Except a b -> ExceptT String IO b
toExceptT = mapExceptT (return . runIdentity) . withExcept show

collect :: String -> Except [ParseError] [AST] -> Except [ParseError] [AST]
collect str result = case runExcept result of
    Left errs -> mapExcept (fmap (\x -> [x])) $ withExcept (\err -> err:errs) $ parse str
    Right asts -> case runExcept $ parse str of
        Left err -> throwError [err]
        Right ast -> return $ ast:asts