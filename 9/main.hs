import Prelude
import Numeric

{-
 - So, we have two equations:
 -   a² + b² = c²
 -   a + b + c = 1000
 - Solving for c gives:
 -   a² + b² = (1000 - a - b)²
 -   a² + b² = 1000000 - 2000a - 2000b + a² + b² + 2ab
 -   2000a + 2000b - 2ab = 1000000
 -   1000a + 1000b - ab = 500000
 -   a + b + 0.001ab = 500
 - So, we simply use list comprehension to generate the potential solutions
 - We limit the possible values of a and be to be in the integer range 1..499 since both have
 - to be positive, and their sum has to be less than 500.
 - This could be limited further, but there is no real need.
 -
 - Note that this gives us two solutions where a and b are reversed
 -}
main = do
  putStrLn $ show pots
  mapM_ putStrLn $ map display (map (showFFloat Nothing) products)
  where c a b = sqrt $ a*a + b*b
        -- All values a,b,c that satisfy c² = a² + b² and a + b + c = 1000
        pots = [(a, b, (c a b)) | a <- [1..499], b <- [1..499], a + b + (c a b) == 1000]
        prod3 (a,b,c) = a*b*c
        products = map prod3 pots
        display a = a ""
