module Main (main) where

import Lib

isMultipleOf :: Integer -> Integer -> Bool
isMultipleOf a b = (a `mod` b)  == 0

isFactorOf = flip isMultipleOf

-- Initial naive implementation
primes :: Integer -> [Integer]
primes 1 = [2]
primes n = (primes' n (lastPrime + 1)) : lastPrime : remPrimes
    where
        (lastPrime : remPrimes) = primes (n - 1)
        primes' n' i | and (fmap (not . (isMultipleOf i)) (primes (n' - 1))) = i
                     | otherwise                                             = primes' n' (i+1)

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

solutionTo _ = 0


main :: IO ()
main = putStrLn $ show $ solutionTo 6
