module One where

import Data.List

x = 123

sqr x = x*x
cube x = x*x*x

something = [324,34,345,456,56,7]

cons x xs = x:xs

mul3 = (3*)
mul3a = (\x -> x * 3)

mul3b x = 3*x

abs' x = if x >= 0 then x else (- x)

abs'' x
 | x >= 0 = x
 | otherwise = (- x)


fak i = if i == 0 then 1 else i * (fak $ i - 1)

fak1 i
  | i == 0 = 1
  | True = i * (fak1 $ pred i)

fak2 0 = 1
fak2 i = i * (fak2 $ pred i)

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
