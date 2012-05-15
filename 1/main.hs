import Prelude
import Data.List

main = do
  showMultiples 1000

showMultiples :: Int -> IO ()
showMultiples limit = do
  putStr "Multiples of 5: "
  putStrLn $ show m5

  putStr "Multiples of 3: "
  putStrLn $ show m3

  putStr "Common multiples: "
  putStrLn $ show m

  putStrLn $ "Sum: " ++ (show s)
  where m5 = multiplesBelow 5 limit
        m3 = multiplesBelow 3 limit
        m  = union m5 m3
        s  = foldr (+) 0 m

multiplesBelow :: Int -> Int -> [Int]
multiplesBelow = multiplesBelow' []

multiplesBelow' :: [Int] -> Int -> Int -> [Int]
multiplesBelow' [] multiple limit = multiplesBelow' [multiple] multiple limit
multiplesBelow' s@(x:xs) multiple limit
  | next >= limit = s
  | otherwise     = multiplesBelow' (next:s) multiple limit
  where next = x + multiple
