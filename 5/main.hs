import Prelude
import Data.List
import Data.Numbers.Primes

main = do
  putStrLn $ show (minimizeMultiples [1..20])
  putStrLn $ show (smallestMultiple [1..20])

smallestMultiple :: [Int] -> Int
smallestMultiple factors =
  mul minimized
  where minimized = minimizeMultiples factors

minimizeMultiples :: [Int] -> [Int]
minimizeMultiples factors = minimizeMultiples' factors []

{-
 - This function will return true if the product of the integers in the second argument
 - is evenly divided by the first number.
 - If they are, this means that all the prime factors of the first argument are present in the
 - union of all the prime factors of the numbers in the second argument.
 -}
hasAllFactorsIn :: Int -> [Int] -> Bool
hasAllFactorsIn n xs = (mul xs) `mod` n == 0

{-
 - We start at the smallest number, and iterate upwards (assuming a sorted list)
 - We maintain a list of "chosen" factors - that is - the factors that we believe must
 - be present in the minimal product.
 - If all of the next number's prime factors are already present in the collection of prime factors
 - in the list of "chosen" factors, there is no point in also choosing that factor
 - Otherwise, we choose the factor, but we also have to remove all lesser factors that are made
 - redundant by this new factor.
 -}
minimizeMultiples' :: [Int] -> [Int] -> [Int]
minimizeMultiples' [] acc = acc
minimizeMultiples' (x:xs) acc
  | x `hasAllFactorsIn` acc = minimizeMultiples' xs acc
  | otherwise = minimizeMultiples' xs (x:stripFactorMultiples acc x)

{-
 - This function takes a list of numbers ns, and another number x, and returns ns without any number
 - which is the product of any combination of x's prime factors.
 -}
stripFactorMultiples :: [Int] -> Int -> [Int]
stripFactorMultiples numbers number =
  numbers \\ strip
  where strip = [mul x | x <- (subsequences $ primeFactors number)]

mul xs = foldr (*) 1 xs
