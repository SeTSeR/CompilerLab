module Backend(
    genCode
) where

import Frontend(AST)

genCode :: [Double] -> [AST] -> [AST] -> String
genCode [a, b] funcs derivs = "Hello, world!\n"