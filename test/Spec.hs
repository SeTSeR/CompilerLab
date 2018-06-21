import Test.HUnit

import Control.Monad.Except

import Frontend

test1 = TestCase $ assertEqual "for parsing variable: " (Right Variable) (runExcept $ parse "x")

test2 = TestCase $ assertEqual "for parsing complex expression: " (Right (BinaryOperator "+" (Number 2.0) (BinaryOperator "*" (UnaryOperator "sin" Variable) (Number 3.0)))) (runExcept $ parse "2 x sin 3 * +")

test3 = TestCase $ assertEqual "for parsing erroneous expression: " (Left (MissingParametersError "+")) (runExcept $ parse "2 +")

test4 = TestCase $ assertEqual "simple derivative: " (runExcept $ derivative <$> parse "x 2 +") (runExcept $ parse "1 0 +")

test5 = TestCase $ assertEqual "more complex derivative: " (runExcept $ derivative <$> parse "x sin 3 *") (runExcept $ parse "-1 x cos x cos * / * 3 x sin 3 * +")

test6 = TestCase $ assertEqual "third derivative: " (runExcept $ derivative <$> parse "x x ln *") (runExcept $ parse "1 x ln * x 1 x / * +")

main :: IO ()
main = do
    let parserTests = TestList [TestLabel "Test parse simple" test1, TestLabel "Test parse complex" test2, TestLabel "Test parse error" test3,
                                TestLabel "Test differentiate simple" test4, TestLabel "Test another derivative" test5, TestLabel "Test third rerivative" test6]
    runTestTT parserTests
    return ()
