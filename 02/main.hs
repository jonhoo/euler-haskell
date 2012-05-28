import Prelude

main = do
  putStrLn $ "Found " ++ (show $ length fibs) ++ " fibs"
  putStrLn $ "Last fib: " ++ (show $ head fibs)

  putStrLn $ "Found " ++ (show $ length efibs) ++ " even fibs"

  putStrLn $ "Sum: " ++ (show s)

  where fibs  = fiblte 4000000
        efibs = filter even fibs
        s     = foldr (+) 0 efibs

fiblte :: Int -> [Int]
fiblte limit = fiblte' 1 [] limit

fiblte' :: Int -> [Int] -> Int -> [Int]
fiblte' _ [] limit = fiblte' 2 [fib 1] limit
fiblte' i s@(x:xs) limit
  | next > limit = s
  | otherwise    = fiblte' (i+1) (next : s) limit
  where next = fib i

fib :: Int -> Int
fib i
  | i < 2 = 1
  | otherwise = fib (i-2) + fib (i-1)
