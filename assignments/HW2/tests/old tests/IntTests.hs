module IntTests where 

import Hw02 (fib, gcd)
import Data.List
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase, (@=?))
import Test.Tasty.QuickCheck as QC

-- Test for fib
fibTests = testGroup "tests for fib function" 
    [
        QC.testProperty "forall n, fib n + fib (n+1) = fib (n+2)" $ 
            \n -> n >= 0 QC.==> fib n + fib (n+1) == fib (n+2)
    ]


-- Tests for gcd 

gcdDivisorTest = QC.testProperty "forall m n larger than 0, gcd of m n divids both n and m" $
    \m n -> m > 0 && n > 0 QC.==> (m `mod` Hw02.gcd m n == 0) && (n `mod` Hw02.gcd m n == 0)

factors n = [x | x <- [1..n], x `mod` n == 0]
commonFactor :: Integer -> Integer -> [Integer]
commonFactor n m = factors n `intersect` factors m

gcdGreatestTest = QC.testProperty "forall m n larger than 0, gcd of m n is " $ 
    \m n -> 
        m > 0 && n > 0 QC.==> all (\fac -> fac <= Hw02.gcd m n) (commonFactor n m)

gcdTests = testGroup "tests for gcd function"
    [
        gcdDivisorTest,
        gcdGreatestTest
    ]

---- Test for Integers ----
intTests = testGroup "tests for Integer function" 
    [
        fibTests,
        gcdTests
    ]
