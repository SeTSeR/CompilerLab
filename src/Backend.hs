module Backend(
    genCode
) where

import Frontend(AST(..))

import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map

genCode :: [Double] -> [AST] -> [AST] -> String
genCode [a, b] functions derivatives = let table = createTable a b (functions ++ derivatives)
                                  in (header (length functions) (length derivatives)) ++ "\n"
                                   ++ (rodata table) ++ "\n"
                                   ++ (text table functions derivatives)

createTable :: Double -> Double -> [AST] -> Map.HashMap Double String
createTable a b trees = let table = Map.fromList [(a, "a"), (b, "b")]
                            identifiersSet = findIdentifiers trees
                            identifiersList = Set.toList identifiersSet
                            identifiersCount = length identifiersList
                            identifierNames = map (((++) "const") . show) [1..identifiersCount]
                            identifiersMap = Map.fromList $ zip identifiersList identifierNames
                        in Map.union table identifiersMap

findIdentifiers :: [AST] -> Set.HashSet Double
findIdentifiers = let searchTree Variable = Set.empty
                      searchTree (Number x) = Set.singleton x
                      searchTree (UnaryOperator _ arg) = searchTree arg
                      searchTree (BinaryOperator _ left right) = Set.union (searchTree left) (searchTree right)
                  in foldr (\tree table -> Set.union table (searchTree tree)) (Set.empty)

header :: Int -> Int -> String
header funccount derivcount = unlines $ ["global a", "global b"] ++ map (((++) "global f") . show) [1..funccount]
                                                                 ++ map (((++) "global df") . show) [1..derivcount]

rodata :: Map.HashMap Double String -> String
rodata table = let elements = zip (Map.keys table) (Map.elems table)
                   lines = ["section .rodata"] ++ (map (\(key, value) -> "    " ++ value ++ " resd " ++ (show key)) elements)
               in unlines lines

text :: Map.HashMap Double String -> [AST] -> [AST] -> String
text table functions derivatives = ""