module Main (main) where

import Lib

solutionTo :: Int -> Int
solutionTo 1 = sum $ filter multipleOfThreeOrFive [3..999]
    where
        multipleOfThreeOrFive n = any (== 0) [n `mod` 3, n `mod` 5]


main :: IO ()
main = putStrLn $ show $ solutionTo 1
