import Prelude
import Data.List

main = do
  putStrLn "Calculating..."
  putStrLn $ show $ firstMany
  putStrLn $ show $ primeFactors firstMany
  where manyFactors = dropWhile (\x -> numberOfDivisors x < 500) triangleNumbers
        firstMany = head manyFactors

-- An infinite list of triangle numbers
triangleNumbers = map triangleNumber [1..]

triangleNumber 0 = 0
triangleNumber n = n + triangleNumber (n-1)

-- Counts the number of divisors for a number using the σ0 divisor function
-- Essentially, the number of divisors for a number is the sum of one more than the exponents
-- of each prime factor
-- See http://en.wikipedia.org/wiki/Divisor_function
numberOfDivisors n = foldr (*) 1 $ map (\(x, n) -> n+1) (primeFactors n)

primeFactors n = primeFactors' n primes []

-- Finding the prime factors is simple when we have all the primes from problem #7
primeFactors' n pp@(p:ps) ff
  -- No point in dividing further if the next divisor is larger than the number
  | p > n          = ff
  --
  -- Does the current prime evenly divide n?
  | n `mod` p == 0 =
    let next = primeFactors' (n `div` p) pp in
    case ff of
      -- If we've already divided by it, just increment the counter
      -- Otherwise, start a counter
      ((p', fn):fs) | p' == p   -> next ((p, fn+1):fs)
                    | otherwise -> next ((p, 1):ff)
      [] ->                        next [(p, 1)]
  --
  -- The current prime doesn't divide n evenly
  | otherwise =
    let next = primeFactors' n ps in
    case ff of
      -- Have we successfully divided by the current prime before?
      -- If so, simply carry on to the next prime
      -- Otherwise, we need to make sure we store the fact that we used it 0 times
      ((p', _):_) | p' == p   -> next ff
                  | otherwise -> next ((p, 0):ff)
      [] ->                      next [(p, 0)]

-- Below here is copy-paste from #7
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
