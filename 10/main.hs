-- This is simple
-- We already have an infinite list of primes from problem #7
-- so we simply use takeWhile to get all the primes less than
-- 2 million and then do the sum.
main = do
  putStrLn $ show $ foldr (+) 0 (takeWhile (<2000000) primes)

-- Using the solution from problem #7
minus (x:xs) (y:ys) = case (compare x y) of
           LT -> x : minus  xs  (y:ys)
           EQ ->     minus  xs     ys
           GT ->     minus (x:xs)  ys
minus  xs     _     = xs

primes = 2 : primes'
  where
    primes' = sieve [3,5..] 9 primes'
    sieve (x:xs) q ps@ ~(p:t)
      | x < q     = x : sieve xs q ps
      | otherwise =     sieve (xs `minus` [q, q+2*p..]) (head t^2) t
