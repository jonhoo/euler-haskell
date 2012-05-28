import Prelude
import Data.List

main = do
  putStrLn $ "Found " ++ (show $ length ms) ++ " palindromes"
  putStrLn $ "Some: " ++ (show $ take 8 ms)
  putStrLn $ "Max: " ++ (show mx)

  where td = [100..999]
        ms = multiplyP td
        mx = foldr max 0 ms

multiplyP :: [Int] -> [Int]
multiplyP td = multiplyP' td td

multiplyP' :: [Int] -> [Int] -> [Int]
multiplyP' [] _ = []
multiplyP' (x:xs) tr = union (filter palindrome r) (multiplyP' xs tr)
  where r = map (x *) tr

palindrome :: Int -> Bool
palindrome n = (rev n) == n

rev :: Int -> Int
rev n = rev' n 0

rev' :: Int -> Int -> Int
rev' n r
  | n == 0 = r
  | otherwise =
    let lastDigit = n `mod` 10
    in rev' (n `div` 10) (r * 10 + lastDigit)
