import Test.HUnit

import Control.Monad.Except

import Frontend

test1 = TestCase $ assertEqual "for parsing variable: " (Right Variable) (runExcept $ parse "x")

test2 = TestCase $ assertEqual "for parsing complex expression: " (Right (BinaryOperator "+" (Number 2.0) (BinaryOperator "*" (UnaryOperator "sin" Variable) (Number 3.0)))) (runExcept $ parse "2 x sin 3 * +")

test3 = TestCase $ assertEqual "for parsing erroneous expression: " (Left (MissingParametersError "+")) (runExcept $ parse "2 +")

test4 = TestCase $ assertEqual "simple derivative: " (runExcept $ parse "1 0 +") (runExcept $ derivative <$> parse "x 2 +")

test5 = TestCase $ assertEqual "more complex derivative: " (runExcept $ parse "x cos 1 * 3 * x sin 0 * +") (runExcept $ derivative <$> parse "x sin 3 *")

test6 = TestCase $ assertEqual "third derivative: " (runExcept $ parse "1 x ln * x 1 x / 1 * * +") (runExcept $ derivative <$> parse "x x ln *")

test7 = TestCase $ assertEqual "test case with divide: " (runExcept $ parse "-1 x sin * e x ^ * x cos e x ^ * 1 * - e x ^ e x ^ * /") (runExcept $ derivative <$> parse "x cos e x ^ /")

main :: IO ()
main = do
    let parserTests = TestList [TestLabel "Test parse simple" test1, TestLabel "Test parse complex" test2, TestLabel "Test parse error" test3,
                                TestLabel "Test differentiate simple" test4, TestLabel "Test another derivative" test5, TestLabel "Test third rerivative" test6,
                                TestLabel "Test with exponent and division" test7]
    runTestTT parserTests
    return ()
