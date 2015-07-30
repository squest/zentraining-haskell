module One where

import Data.List

abs' x = if x >= 0 then x else (- x)

isPrime n
  | n < 2 = False
  | n == 2 = True
  | even n = False
  | otherwise = iter 3
  where gjelas = n*n
        iter i
          | isqr > n = True
          | 0 == rem n i = False
          | otherwise = iter $ i + 2
          where isqr = i*i

fakIter i = iter 1 1
  where iter n res
          | n > i = res
          | otherwise = iter (n+1) (res*n)

take' 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

sum' [] = 0
sum (x:xs) = x + (sum' xs)

map' f [] = []
map' f (x:xs) = (f x) : map' f xs
