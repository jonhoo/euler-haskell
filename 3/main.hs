import Prelude

main = do
  putStrLn $ "Minifying 13195: " ++ (show m1)

  putStrLn $ "Minifying 600851475143: " ++ (show m2)

  where m1 = minify 13195
        m2 = minify 600851475143

minify :: Int -> Int
minify n = minify' n 2

minify' :: Int -> Int -> Int
minify' n m
  | m >= n = n
  | otherwise =
    if n `mod` m == 0
      then minify' (n `div` m) m
      else minify' n (m+1)
