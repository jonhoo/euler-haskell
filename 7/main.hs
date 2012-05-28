main = do
  putStrLn $ show $ primes !! 10000

{-
 - An implementation of list difference on ordered lists
 - This is needed so that we can seize evaluation of the difference
 - Even if the list we're subtracting is infinite
 -}
minus (x:xs) (y:ys) = case (compare x y) of
           LT -> x : minus  xs  (y:ys)
           EQ ->     minus  xs     ys
           GT ->     minus (x:xs)  ys
minus  xs     _     = xs

{-
 - Eratosthenes sieve with minor optimizations
 - Basically identical to the one from http://www.haskell.org/haskellwiki/Prime_numbers#Postponed
 - with minor style changes
 -
 - I've chosen to take the code almost verbatim before it is fairly straightforward
 - Essentially, the sieve in general works by listing all the numbers and removing all multiples
 - of every prime encountered up to any given point from the remaining list of numbers.
 -
 - The basic implementation requires a maximum number for the sieve, but this version generates
 - an infinite list that we can easily extract values from.
 -
 - It has a couple of optimizations over the naïve Eratosthenes sieve, namely:
 -  * The list is initially only odd numbers, since even numbers cannot be primes
 -  * After removing all multiples of a prime n, we start the next removal at n² since all
 -    non-primes between n and n² will already have been wiped
 -
 - Understanding this implementation is a great exercise, since there are a number of quite
 - complex interactions going on. The algorithm is essentially inductive based on the following steps:
 -  1. Find the first prime number (easy, it is 2)
 -  2. Assuming you have a list of all the prime numbers up until n, find the next prime number
 -     This inductive step can be achieved using the lazy evaluation of haskell, and is caused
 -     by the recursive definition of primes'.
 -}
primes = 2 : primes'
  where
    primes' = sieve [3,5..] 9 primes'
    sieve (x:xs) q ps@ ~(p:t)
      | x < q     = x : sieve xs q ps
      | otherwise =     sieve (xs `minus` [q, q+2*p..]) (head t^2) t
