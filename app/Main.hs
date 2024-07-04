module Main (main) where

import Data.Char (digitToInt)
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

windows :: Int -> [a] -> [[a]]
windows n vs = windows' (take n vs) (drop n vs)
    where
        windows' ws []         = []
        windows' (w:ws) (x:xs) = (w:ws) : windows' (ws ++ [x]) xs

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

solutionTo 8 = foldl maxProduct 0 (windows 13 n)
    where
        n = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
        maxProduct m vs = max m (toInteger $ product $ digitToInt <$> vs)


solutionTo _ = 0


main :: IO ()
main = putStrLn $ show $ solutionTo 8
