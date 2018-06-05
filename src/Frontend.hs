module Frontend(
    AST,
    ParseError,
    borders,
    parse,
    derivative,
) where

import Control.Monad.Except
import Data.Either
import Text.Read

data AST = Variable | Number Double | UnaryOperator String AST | BinaryOperator String AST AST
    deriving (Show, Eq)

data ParseError = BorderError | EmptyInput

instance Show ParseError where
    show BorderError = "Incorrect borders"
    show EmptyInput = "Input is empty"


borders :: String -> Except ParseError [Double]
borders str = case words str of
    a:b:_ -> let maybeBorders = map readEither [a, b]
             in if all isRight maybeBorders
                then return $ rights maybeBorders
                else throwError BorderError
    _ -> throwError BorderError

parse :: String -> Except ParseError AST
parse = undefined

derivative :: AST -> AST
derivative = undefined