module Backend(
    genCode
) where

import Frontend(AST(..))

import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map

genCode :: [Double] -> [AST] -> [AST] -> String
genCode [a, b] functions derivatives = let table = createTable (functions ++ derivatives)
                                  in (header (length functions) (length derivatives)) ++ "\n"
                                   ++ (rodata table a b) ++ "\n"
                                   ++ (text table functions derivatives)

createTable :: [AST] -> Map.HashMap Double String
createTable trees = let identifiersSet = findIdentifiers trees
                        identifiersList = Set.toList identifiersSet
                        identifiersCount = length identifiersList
                        identifierNames = map (((++) "const") . show) [1..identifiersCount]
                        in Map.fromList $ zip identifiersList identifierNames

findIdentifiers :: [AST] -> Set.HashSet Double
findIdentifiers = let findIdentifiers Variable = Set.empty
                      findIdentifiers (Number x) = Set.singleton x
                      findIdentifiers (UnaryOperator _ arg) = findIdentifiers arg
                      findIdentifiers (BinaryOperator _ left right) = Set.union (findIdentifiers left) (findIdentifiers right)
                  in foldr (\tree table -> Set.union table (findIdentifiers tree)) (Set.empty)

header :: Int -> Int -> String
header funccount derivcount = unlines $ [ "[BITS 64]", "default rel"
                                        , "global a", "global b"] ++ map (((++) "global f") . show) [1..funccount]
                                                                  ++ map (((++) "global df") . show) [1..derivcount]

rodata :: Map.HashMap Double String -> Double -> Double -> String
rodata table a b = let elements = zip (Map.keys table) (Map.elems table)
                       lines  = [ "section .rodata"
                                , "    a dq " ++ (show a)
                                , "    b dq " ++ (show b)
                                ] ++ (map (\(key, value) -> "    " ++ value ++ " dq " ++ (show key)) elements)
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
prolog = map ((++) "    ")  [ "push rbp"
                            , "mov rbp, rsp"
                            , "sub rsp, 8"
                            , "movsd qword[rsp], xmm0"
                            ]

epilog :: [String]
epilog = map ((++) "    ") [ "movsd xmm0, qword[rsp]"
                           , "add rsp, 16"
                           , "pop rbp"
                           , "ret"
                           ]

node :: Map.HashMap Double String -> AST -> [String]
node table tree = case tree of
        Variable -> [ "    push qword[rbp - 8]" ]
        Number x -> [ "    mov rax, qword[" ++ (table Map.! x) ++ "]"
                    , "    push rax" ]
        UnaryOperator token arg -> let begin = [ "fld qword[rsp]"  ]
                                       end   = [ "fstp qword[rsp]" ]
                                       body  = case token of
                                        "sin" -> [ "fsin"     ]
                                        "cos" -> [ "fcos"     ]
                                        "tan" -> [ "fptan"
                                                 , "fstp st0"
                                                 ]
                                        "ctg" -> [ "fptan"
                                                 , "fdivp"
                                                 ]
                                        "ln"  -> [ "fld1"
                                                 , "fxch"
                                                 , "fyl2x"
                                                 ]
                                   in (++) (node table arg) $ map ((++) "    ") (begin ++ body ++ end)
        BinaryOperator token left right -> let  begin = [ "fld qword[rsp + 8]"
                                                        , "fld qword[rsp]"
                                                        ]
                                                end   = [ "add rsp, 8"
                                                        , "fstp qword[rsp]"
                                                        ]
                                                body  = case token of
                                                    "+" -> [ "faddp" ]
                                                    "-" -> [ "fsubp" ]
                                                    "*" -> [ "fmulp" ]
                                                    "/" -> [ "fdivp" ]
                                                    "^" -> [ "fxch"
                                                           , "fyl2x"
                                                           , "fld1"
                                                           , "fld st1"
                                                           , "fprem"
                                                           , "f2xm1"
                                                           , "faddp"
                                                           , "fscale"
                                                           , "fstp st1"
                                                           ]
                                           in (++) ((node table left) ++ (node table right)) $ map ((++) "    ") (begin ++ body ++ end)