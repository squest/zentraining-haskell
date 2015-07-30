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

take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

sum' [] = 0
sum' (x:xs) = x + (sum' xs)

map' f [] = []
map' f (x:xs) = (f x) : map' f xs

null' [] = True
null' _ = False

drop' 0 (xs) = (xs)
drop' _ [] = []
drop' n (x:xs) = drop' (n-1) xs

fst' (a, b) = a

snd' (a, b) = b


filter' f [] = []
filter' f (x:xs)
  | (f x) = x : filter' f xs
  | otherwise = filter' f xs

delete' n [] = []
delete' n (x:xs)
  | (n == x) = xs
  | otherwise = x : delete' n xs

deleteAll' n [] = []
deleteAll' n (x:xs)
    | (n == x) = deleteAll' n xs
    | otherwise = x : deleteAll' n xs

foldl'' _ a [] = a
foldl'' f a (x:xs) = foldl'' f (f a x) xs


foldl1'' _ (x:[]) = x
foldl1'' f (x:y:xs) = foldl1'' f ((f x y):xs)
