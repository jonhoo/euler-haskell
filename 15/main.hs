import Prelude

main = do
  putStrLn "Calculating"
  -- This is extremely simple.
  -- Out path from the top left corner to the bottom right must contain an equal number of moves
  -- down and to the right, 20 of each to be precise.
  -- It does not matter in what order they come, so we're looking for the possible distributions
  -- of 20 elements within a group of 40.
  -- Luckily, there is a formula for this if you paid attention in math, called the combination formula.
  -- http://en.wikipedia.org/wiki/Combination
  -- There is even an optimized Haskell version that does not use factorials directly: http://stackoverflow.com/a/6806997/472927
  putStrLn $ show $ choose 40 20;

choose n 0 = 1
choose 0 k = 0
choose n k = choose (n-1) (k-1) * n `div` k
