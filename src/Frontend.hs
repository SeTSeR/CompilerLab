module Frontend(
    AST(..),
    ParseError(..),
    borders,
    parse,
    derivative,
    optimize
) where

import Control.Monad.Except
import Data.Either
import Data.Functor.Identity
import Text.Read(readEither)
import System.IO.Unsafe

data AST = Variable | Number Double | UnaryOperator String AST | BinaryOperator String AST AST
    deriving Eq

instance Show AST where
    show Variable = "x"
    show (Number n) = show n
    show (UnaryOperator token arg) = "(" ++ token ++ " " ++ (show arg) ++ ")"
    show (BinaryOperator token left right) = "(" ++ (show left) ++ " " ++ token ++ " " ++ (show right) ++ ")"

data ParseError = BorderError | UnknownTokenError String | MissingParametersError String | OddTokensError | EmptyInputError
    deriving Eq

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
            else if token == "e"
                then return $ (Number $ exp 1):stack
            else if token == "pi"
                then return $ (Number pi):stack
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
derivative Variable = Number 1
derivative (Number _) = Number 0
derivative (UnaryOperator token arg) = case token of
    "sin" -> BinaryOperator "*" (UnaryOperator "cos" arg) (derivative arg)
    "cos" -> BinaryOperator "*" (BinaryOperator "*" (Number $ -1) (UnaryOperator "sin" arg)) (derivative arg)
    "tan" -> BinaryOperator "/" (derivative arg) (BinaryOperator "*" (UnaryOperator "cos" arg) (UnaryOperator "cos" arg))
    "ctg" -> BinaryOperator "/" (BinaryOperator "*" (Number $ -1) (derivative arg)) (BinaryOperator "*" (UnaryOperator "sin" arg) (UnaryOperator "sin" arg))
    "ln" -> BinaryOperator "/" (derivative arg) arg
derivative (BinaryOperator token left right) = case token of
    "+" -> BinaryOperator "+" (derivative left) (derivative right)
    "-" -> BinaryOperator "-" (derivative left) (derivative right)
    "*" -> BinaryOperator "+" (BinaryOperator "*" (derivative left) right) (BinaryOperator "*" left (derivative right))
    "/" -> BinaryOperator "/" (BinaryOperator "-" (BinaryOperator "*" (derivative left) right) (BinaryOperator "*" left (derivative right))) (BinaryOperator "*" right right)
    "^" -> BinaryOperator "*" (BinaryOperator "^" left right) (derivative $ BinaryOperator "*" right (UnaryOperator "ln" left))

optimize :: AST -> AST
optimize = foldConstants . arithmeticOptimizations . foldConstants

foldConstants :: AST -> AST
foldConstants (UnaryOperator token arg) = case foldConstants arg of
    Number x -> case token of
        "sin" -> Number $ sin x
        "cos" -> Number $ cos x
        "tan" -> Number $ tan x
        "ctg" -> Number $ 1.0 / tan x
        "ln"  -> Number $ log x
    tree -> tree
foldConstants (BinaryOperator token left right) = case (foldConstants left, foldConstants right) of
    (Number a, Number b) -> case token of
        "+" -> Number $ a + b
        "-" -> Number $ a - b
        "*" -> Number $ a * b
        "/" -> Number $ a / b
        "^" -> Number $ a ** b
    (left, right) -> BinaryOperator token left right
foldConstants tree = tree

arithmeticOptimizations :: AST -> AST
arithmeticOptimizations (BinaryOperator token left right) = let updatedTree = BinaryOperator token (arithmeticOptimizations left) (arithmeticOptimizations right)
                                                            in case token of
                                                                "+" -> additionOptimizations updatedTree
                                                                "-" -> substractionOptimizations updatedTree
                                                                "*" -> multiplicationOptimizations updatedTree
                                                                "/" -> divisionOptimizations updatedTree
                                                                "^" -> powerOptimizations updatedTree
arithmeticOptimizations (UnaryOperator token arg) = UnaryOperator token (arithmeticOptimizations arg)
arithmeticOptimizations tree = tree

additionOptimizations :: AST -> AST
additionOptimizations (BinaryOperator "+" left (Number 0)) = left
additionOptimizations (BinaryOperator "+" (Number 0) right) = right
additionOptimizations tree = tree

substractionOptimizations :: AST -> AST
substractionOptimizations (BinaryOperator "-" left (Number 0)) = left
substractionOptimizations (BinaryOperator "-" left right) | left == right = Number 0
substractionOptimizations tree = tree

multiplicationOptimizations :: AST -> AST
multiplicationOptimizations (BinaryOperator "*" _ (Number 0)) = Number 0
multiplicationOptimizations (BinaryOperator "*" (Number 0) _) = Number 0
multiplicationOptimizations (BinaryOperator "*" left (Number 1)) = left
multiplicationOptimizations (BinaryOperator "*" (Number 1) right) = right
multiplicationOptimizations tree = tree

divisionOptimizations :: AST -> AST
divisionOptimizations (BinaryOperator "/" (Number 0) _) = Number 0
divisionOptimizations (BinaryOperator "/" left right) | left == right = Number 1
divisionOptimizations tree = tree

powerOptimizations :: AST -> AST
powerOptimizations (BinaryOperator "^" (Number 0) _) = Number 0
powerOptimizations (BinaryOperator "^" (Number 1) _) = Number 1
powerOptimizations (BinaryOperator "^" _ (Number 0)) = Number 1
powerOptimizations tree = tree
