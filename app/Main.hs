module Main (main) where

import Lib

fibonacci :: [Integer]
fibonacci = 1 : 2 : zipWith (+) fibonacci (drop 1 fibonacci)

solutionTo :: Int -> Integer
solutionTo 1 = sum $ filter multipleOfThreeOrFive [3..999]
    where
        multipleOfThreeOrFive n = any (== 0) [n `mod` 3, n `mod` 5]

solutionTo 2 = sum $ filter (\n -> (n `mod` 2) == 0) $ takeWhile (< 4000000) fibonacci

solutionTo _ = 0


main :: IO ()
main = putStrLn $ show $ solutionTo 2
