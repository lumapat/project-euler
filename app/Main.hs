module Main (main) where

import Lib

isMultipleOf :: Integer -> Integer -> Bool
isMultipleOf a b = (a `mod` b)  == 0

isFactorOf = flip isMultipleOf

-- Initial naive implementation
naivePrimes :: Integer -> [Integer]
naivePrimes 1 = [2]
naivePrimes n = (primes' n (lastPrime + 1)) : lastPrime : remPrimes
    where
        (lastPrime : remPrimes) = naivePrimes (n - 1)
        primes' n' i | and (fmap (not . (isMultipleOf i)) (naivePrimes (n' - 1))) = i
                     | otherwise                                             = primes' n' (i+1)

primes :: [Integer]
primes = primes' 2 [] []
    where
        primes' :: Integer -> [Integer] -> [Integer] -> [Integer]
        primes' n [] total = n : primes' (n+1) (n:total) (n:total)
        primes' n (p:ps) total | n `mod` p > 0 = primes' n ps total
                               | otherwise     = primes' (n+1) total total

fibonacci :: [Integer]
fibonacci = 1 : 2 : zipWith (+) fibonacci (drop 1 fibonacci)

solutionTo :: Int -> Integer
solutionTo 1 = sum $ filter multipleOfThreeOrFive [3..999]
    where
        multipleOfThreeOrFive n = any (== 0) [n `mod` 3, n `mod` 5]

solutionTo 2 = sum $ filter (\n -> (n `mod` 2) == 0) $ takeWhile (< 4000000) fibonacci

-- solutionTo 3 = 600851475143
--     where

solutionTo 5 = head $ filter isSmallestMultiple [2520..]
    where
        isSmallestMultiple :: Integer -> Bool
        isSmallestMultiple n = and $ (isMultipleOf n) <$> [1..20]

solutionTo 6 = sumSquare - squareSum
    where
        sumSquare = sum (zipWith (*) [1..100] [1..100])
        sum' = sum [1..100]
        squareSum = sum' * sum'

solutionTo 7 = primes !! 10000

solutionTo _ = 0


main :: IO ()
main = putStrLn $ show $ solutionTo 7
