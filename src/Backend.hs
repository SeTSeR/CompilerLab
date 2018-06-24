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
findIdentifiers = let findIdentifiers Variable = Set.empty
                      findIdentifiers (Number x) = Set.singleton x
                      findIdentifiers (UnaryOperator _ arg) = findIdentifiers arg
                      findIdentifiers (BinaryOperator _ left right) = Set.union (findIdentifiers left) (findIdentifiers right)
                  in foldr (\tree table -> Set.union table (findIdentifiers tree)) (Set.empty)

header :: Int -> Int -> String
header funccount derivcount = unlines $ ["global a", "global b"] ++ map (((++) "global f") . show) [1..funccount]
                                                                 ++ map (((++) "global df") . show) [1..derivcount]

rodata :: Map.HashMap Double String -> String
rodata table = let elements = zip (Map.keys table) (Map.elems table)
                   lines = ["section .rodata"] ++ (map (\(key, value) -> "    " ++ value ++ " dq " ++ (show key)) elements)
               in unlines lines

text :: Map.HashMap Double String -> [AST] -> [AST] -> String
text table functions derivatives = let fcount = length functions
                                       dcount = length derivatives
                                       funcs = zip [1..fcount] functions
                                       derivs = zip [1..dcount] derivatives
                                       lines = ["section .text"] ++ (concatMap (subroutine table "f") funcs) ++ (concatMap (subroutine table "df") derivs)
                                   in unlines lines

subroutine :: Map.HashMap Double String -> String -> (Int, AST) -> [String]
subroutine table prefix (number, function) = [prefix ++ (show number) ++ ":"] ++ prolog ++ (node table function) ++ epilog ++ [""]

prolog :: [String]
prolog = map ((++) "    ") [ "sub rsp, 8"
                           , "movsd qword[rsp], xmm0"
                           ]

epilog :: [String]
epilog = map ((++) "    ") [ "movsd xmm0, qword[rsp]"
                           , "add rsp, 16"
                           ]

node :: Map.HashMap Double String -> AST -> [String]
node table tree = map ((++) "    ") [ "push qword[rsp]"
                                    ]