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

--sum' [] = 0
--sum' (x:xs) = x + (sum' xs)

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

zip' [] _ = []
zip' _ [] = []
zip' (a:as) (x:xs) = (a,x) : zip' as xs

zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (a:as) (x:xs) = (f a x) : zipWith' f as xs

nth [] _ = []
nth (xs) n = [xs !! n]

sort' [] = []
sort' (x:[]) = [x]
sort' (x:xs) = (sort' (filter (<= x) xs)) ++
                [x] ++
                (sort' (filter (> x) xs))

scanl' f a [] = [a]
scanl' f a (x:xs) = a : scanl' f (f a x) xs

scanl1' _ (x:[]) = [x]
scanl1' f (x:y:xs) = x : scanl1' f ((f x y):xs)

elem' a [] = False
elem' a (x:xs)
      | (x == a) = True
      | otherwise = elem' a xs

notElem' a [] = True
notElem' a (x:xs)
      | (x == a) = False
      | otherwise = notElem' a xs

head' (x:xs) = x

tail' (x:xs) = xs

length' [] = 0
length' (x:xs) = 1 + (length' xs)

reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

last' (x:xs) = (head' (reverse' xs))

init'' (xs) = (reverse' (tail' (reverse' xs)))

init' (x:[]) = []
init' (x:xs) = x : init' xs

min'' (xs) = (head' (sort' xs))
max'' (xs) = (head' (reverse (sort' xs)))

max' a b = if a > b then a else b
min' a b = if a > b then b else a

maximum' [x] = x
maximum' (x:xs) = max' x (maximum' xs)

minimum' [x] = x
minimum' (x:xs) = min' x (minimum' xs)

concat' [] = []
concat' (x:xs) = x ++ (concat' xs)

intersperse' _ [] = []
intersperse' _ (x:[]) = x : []
intersperse' a (x:xs) = x : a : intersperse' a xs

intercalate' _ [] = []
intercalate' _ (x:[]) = x ++ []
intercalate' a (x:xs) = x ++ a ++ intercalate' a xs

and' [] = True
and' (x:y:xs) = if x && y then True else False

or' [] = False
or' (x:y:xs) = if x && y then True else False

zip3' [] _ _ = []
zip3' _ [] _ = []
zip3' _ _ [] = []
zip3' (a:as) (x:xs) (y:ys) = (a,x,y) : zip3' as xs ys

sum' (x:[]) = x
sum' (x:y:xs) = sum' ((x + y):xs)

product' (x:[]) = x
product' (x:y:xs) = product' ((x * y):xs)
