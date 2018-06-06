import Test.HUnit

import Control.Monad.Except

import Frontend

test1 = TestCase $ assertEqual "for parsing variable: " (Right Variable) (runExcept $ parse "x")

test2 = TestCase $ assertEqual "for parsing complex expression: " (Right (BinaryOperator "+" (Number 2.0) (BinaryOperator "*" (UnaryOperator "sin" Variable) (Number 3.0)))) (runExcept $ parse "2 x sin 3 * +")

test3 = TestCase $ assertEqual "for parsing erroneous expression: " (Left (MissingParametersError "+")) (runExcept $ parse "2 +")

main :: IO ()
main = do
    let parserTests = TestList [TestLabel "Test parse simple" test1, TestLabel "Test parse complex" test2, TestLabel "Test parse error" test3]
    runTestTT parserTests
    return ()
