import Prelude
import Data.List

main = do
  putStrLn $ show $ sum muls
  where nums = [1..100]
        muls = map (mulsumminself nums) nums

mulsumminself numbers number = number * ((sum numbers) - number)

{-
 - This requires some explanation...
 - Okay, so we're calculating the difference between
 - s1 = a² + b² + ... + y² + z²
 - and
 - s2 = (a + b + ... + y + z)²
 - Expanding the former gives:
 - a² + ab + ... + az + ba + b² + bc + ... bz + ... + ya + yb + ... + y² + yz + za + ... + zy + z²
 - Which, after reorganized, gives:
 - a² + b² + ... + y² + z² + (ab + ... + az + ba + ... + bz + ... + ya + ... + yz + za + ... + zy)
 - The first part of which is == s1
 - So, the difference is that last expression
 - The last expression can be simplified to:
 - a(b + ... + z) + b(a + c + ... + z) + ... + y(a + ... + x + z) + z(a + ... + y)
 - This looks tedious to calculate, but let's reorganize it a bit:
 - a(a + ... + z - a) + b(a + ... + z - b) + ... + y(a + ... + z - y) + z(a + ... + z - z)
 - It is now fairly obvious that to obtain the difference s2 - s1 by for each number n doing:
 - n(s - n) where s is the sum of the numbers a-z
 - This corresponds to the poorly named mulsumminself (MULtiply with SUM MINus SELF)
 - So, we map this function over all the terms (1.100), and then simply add them together to
 - get the solution
 -}
