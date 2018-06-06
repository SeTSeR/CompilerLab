module Frontend(
    AST,
    ParseError,
    borders,
    parse,
    derivative,
) where

import Control.Monad.Except
import Data.Either
import Data.Functor.Identity
import Text.Read(readEither)
import System.IO.Unsafe

data AST = Variable | Number Double | UnaryOperator String AST | BinaryOperator String AST AST
    deriving (Show, Eq)

data ParseError = BorderError | UnknownTokenError String | MissingParametersError String | OddTokensError | EmptyInputError

instance Show ParseError where
    show BorderError = "Incorrect borders"
    show (UnknownTokenError token) = "Unknown operator: " ++ token
    show (MissingParametersError token) = "Missing " ++ token ++ " operator parameters"
    show EmptyInputError = "Input is empty"
    show OddTokensError = "Odd tokens in input"


borders :: String -> Except ParseError [Double]
borders str = case words str of
    a:b:_ -> let maybeBorders = map readEither [a, b]
             in if all isRight maybeBorders
                then return $ rights maybeBorders
                else throwError BorderError
    _ -> throwError BorderError

parseToken :: Except ParseError [AST] -> String -> Except ParseError [AST]
parseToken stack token = do
    let unaryOperators = ["sin", "cos", "tan", "ctg", "ln"]
        binaryOperators = ["+", "-", "*", "/", "^"]
    stack <- stack
    case (readEither token) :: Either String Double of
        Right value -> return $ (Number value):stack
        _ -> if token == "x"
            then return $ Variable:stack
            else if token `elem` unaryOperators
                then case stack of
                    arg:xs -> return $ (UnaryOperator token arg):xs
                    _ -> throwError $ MissingParametersError token
                else if token `elem` binaryOperators
                    then case stack of
                        right:left:xs -> return $ (BinaryOperator token left right):xs
                        _ -> throwError $ MissingParametersError token
                    else throwError $ UnknownTokenError token

parse :: String -> Except ParseError AST
parse str = do
    stack <- foldl parseToken ((return []) :: Except ParseError [AST]) $ words str
    case stack of
        [x] -> return x
        x:xs -> throwError OddTokensError
        _ -> throwError EmptyInputError

derivative :: AST -> AST
derivative = id