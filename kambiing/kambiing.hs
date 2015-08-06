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
and' (x:xs)
  | x = and' xs
  | otherwise = False

or' [] = False
or' (x:xs)
  | x =True
  | otherwise = or' xs

zip3' [] _ _ = []
zip3' _ [] _ = []
zip3' _ _ [] = []
zip3' (a:as) (x:xs) (y:ys) = (a,x,y) : zip3' as xs ys

zipWith3' _ [] _ _ = []
zipWith3' _ _ [] _ = []
zipWith3' _ _ _ [] = []
zipWith3' f (a:as) (x:xs) (y:ys) = (f a x y) : zipWith3' f as xs ys

sum' (x:[]) = x
sum' (x:y:xs) = sum' ((x + y):xs)

product' (x:[]) = x
product' (x:y:xs) = product' ((x * y):xs)

unwords' (x:[]) = x
unwords' (x:xs) = x ++ " " ++ unwords' xs

takeWhile' f [] = []
takeWhile' f (x:xs)
  | (f x) = x : takeWhile' f xs
  | otherwise = []

concatMap' _ [] = []
concatMap' f (x:xs) = (f x) ++ concatMap' f xs

all' f (x) = and' (map' f x)

any' f (x) = or' (map' f x)

insert' x (xs) = x:xs

nub' [] = []
nub' (x:xs) = x : (nub' (deleteAll' x xs))

union' [] [] = []
union' xs ys = nub' (xs ++ ys)

tails' [] = [[]]
tails' (x:xs) = (x:xs) : tails' xs

inits' [] = [[]]
inits' xs = (map' reverse' (reverse' (tails' (reverse' xs))))

intersect' _ [] = []
intersect' [] _ = []
intersect' (x:xs) ys
        | elem' x ys = x : intersect' xs ys
        | otherwise = intersect' xs ys

group' [] = []
group' xs = takeWhile' (== head' xs) xs : group' (dropWhile (== head' xs) xs)

splitAt' _ [] = []
splitAt' n xs = [take n xs] ++ [drop n xs]

partition' _ [] = []
partition' f xs = [filter' f xs] ++ [filter' (\n -> not (f n)) xs]

replicate' 0 [] = []
replicate' n xs = take' n ([xs] ++ replicate' (pred n) xs)


iterate' f x = x : iterate' f (f x)

repeat' x = x : repeat' x

cycle' x = x ++ cycle' x

--(fn [x] (nth (iterate #(conj % (+ (last (butlast %)) (last %) )) [1 1] ) (- x 2) ))
--translate dari clojure, masih salah
fibopartial xs = xs ++ [((last (init xs)) + last xs)]
fibo 2 = [1,1]
fibo x = nth (iterate' fibopartial [1,1]) (x - 2)

fak x = foldl1'' (*) [1..x]

fakList x = scanl1' (*) [1..x]


sieveN n = sieve [2..n]

sieve [] = []
sieve (x:xs) = x : sieve (filter (\n -> (0 /= rem n x)) xs)
