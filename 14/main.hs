import Prelude
import Data.List

main = do
  putStrLn "Calculating"
  putStr $ show $ fst lng
  putStr " with length of "
  putStr $ show $ snd lng
  where all = [(n, collatzLength n) | n <- [1..999999]]
        -- Special max function for extracting from tuples
        mx  = \a@(_, l1) b@(_, l2) -> if l1 > l2 then a else b
        -- We use foldl1' becuase we want strict evaluation with no starting value
        -- We also fold from the left because then we don't want any laziness here for performance.
        lng = foldl1' mx all

{-
 - Here, we should utilize some memoization to make the computation go a lot faster
 - Essentially, we remember all computed chains, so if we later encounter a request for a chain
 - we've already computed, we don't have to compute it again
 -
 - The memoization syntax is inspired by the example here: http://www.haskell.org/haskellwiki/Memoization#Memoization_with_recursion,
 - but we've replaced the simple list with a binary tree to make it faster
 - The binary tree is one optimized for integer keys using the 0s and 1s of the binary representation
 - of the integer as left and right trees. See http://stackoverflow.com/a/11060954/472927
 -}

data Tree a = Tree (Tree a) a (Tree a)
instance Functor Tree where
  fmap f (Tree l n r) = Tree (fmap f l) (f n) (fmap f r)

index t n = index' t (n+1)
index' (Tree l m r) 1 = m
index' (Tree l m r) n = case n `divMod` 2 of
  (n', 0) -> index' l n'
  (n', 1) -> index' r n'

-- Fill a tree with natural numbers
nats = fmap (\n -> n-1) nats'
nats' = Tree l 1 r
  where
    l = fmap (\n -> n*2)     nats'
    r = fmap (\n -> n*2 + 1) nats'
    nats' = Tree l 1 r


-- The real business logic starts here
chainTree :: Tree Integer
chainTree = fmap collatzLength' nats

collatzLength :: Integer -> Integer
collatzLength = index chainTree

collatzLength' 1 = 1
collatzLength' n = 1 + collatzLength next
  where next = if even n then n `div` 2 else 3 * n + 1
