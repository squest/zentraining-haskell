module Haskell where

import Data.List

--null
null' [] = True
null' _ = False

--take
take' 0 _ = []
take' _ [] = []
take' a (x:xs) = x : (take' (pred a) xs)

--snd
snd' (a, b) = b

--drop
drop' 0 xs = xs
drop' _ [] = []
drop' a (x:xs) = drop' (pred a) xs

--fst
fst' (a, b) = a

--map
map' f [] = []
map' f (x:xs) = f x : map' f xs

--filter
filter' f [] = []
filter' f (x:xs) = if (f x == False) then (filter' f xs) else x : (filter' f xs)

--remove
remove' f [] = []
remove' f (x:xs) = if (f x == True) then (remove' f xs) else x: (remove' f xs)

--delete
delete' _ [] = []
delete' a (x:xs) = if x == a then xs else x : delete' a xs

--nub
nub' [] = []
nub' (x:xs) = x : nub'  (filter' (x /= ) xs)

--foldl
foldl2 f a [] = a
foldl2 f a (x:xs) = foldl2 f (f x a) xs

--foldl1
foldl12 f (x:y:xs) = if xs == [] then f x y else foldl12 f ((f x y):xs)

--zip
zip2 [] _ = []
zip2 _ [] = []
zip2 (x:xs) (y:ys) = (x,y) : zip2 xs ys

--zipWith
zipWith2 f [] _ = []
zipWith2 f _ [] = []
zipWith2 f (x:xs) (y:ys) = f x y : zipWith2 f xs ys

--nth
nth (x:xs) a = if a == 0 then x else nth xs (pred a)

--sort
sort' [] = []
sort' xs = minimum' xs : (sort' (delete (minimum' xs) xs))

--scanl
scanl2 _ a [] = [a]
scanl2 f a (x:xs) = a : scanl2 f (f a x) xs

--scanl1
scanl12 _ [] = []
scanl12 _ (x:[]) = [x]
scanl12 f (x:y:xs) = x : scanl12 f ((f x y):xs)

--inits
inits' [] = [[]]
inits' xs = inits' (init' xs) ++ [xs]

--tails
tails' [] = [[]]
tails' xs = [xs] ++ tails' (tail' xs)

--elem
elem2 _ [] = False
elem2 a (x:xs) = if x == a then True else elem2 a xs

--notElem
notElem2 _ [] = True
notElem2 a (x:xs) = if xs == a then False else notElem2 a xs

--head
head2 (x:xs) = x

--length
length2 [] = 0
length2 (x:xs) = 1 + length2 xs

--reverse
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

--last
last' (x:[]) = x
last' (x:xs) = last' xs

--tail
tail' (x:xs) = xs

--init
init' (x:[]) = []
init' (x:xs) = x : init' xs

--max
max' a b = if a >= b then a else b

--min
min' a b = if a <= b then a else b

--minimum
minimum' (x:[]) = x
minimum' (x:y:xs) = minimum' ((min' x y):xs)

--maximum
maximum' (x:[]) = x
maximum' (x:y:xs) = maximum' ((max' x y):xs)

--concat
concat2 [[]] = []
concat2 (x:[]) = x
concat2 (x:xs) = x ++ concat2 xs

--union
union' [] xs = xs
union' xs [] = xs
union' (x:xs) ys = if elem2 x ys then x:union' xs (filter' (x /= ) (nub' ys)) else x:union' xs ys

--intersect
intersect2 [] _ = []
intersect2 _ [] = []
intersect2 (x:xs) ys = if elem x ys then x : intersect2 xs ys else intersect2 xs ys

--intersperse
intersperse' _ [] = []
intersperse' _ (x:[]) = [x]
intersperse' a (x:xs) = x : a : intersperse' a xs

--intercalate
intercalate' _ [] = []
intercalate' _ (x:[]) = x
intercalate' as (x:xs) = x ++ as ++ intercalate' as xs

--and
and' xs = if elem2 False xs then False else True

--or
or' xs = if elem2 True xs then True else False

--group
group' [] = []
group' (x:xs) = takeWhile' (x == ) (x:xs) : group' (dropWhile' (x == ) (x:xs))

--zip3
zip32 [] _ _ = []
zip32 _ [] _ = []
zip32 _ _ [] = []
zip32 (a:as) (b:bs) (c:cs) = (a,b,c) : (zip32 as bs cs)

--sum
sum' [] = 0
sum' (x:xs) = x + sum' xs

--product
product' [] = 1
product' (x:xs) = x * product' xs

--splitAt
splitAt' _ [] = ([],[])
splitAt' a xs = (take' a xs, drop' a xs)

--words
words' "" = []
words' (x:xs) = if x == ' ' then words' xs else [takeWhile' (' ' /= ) (x:xs)] ++ words' (delete' ' ' (dropWhile' (' ' /= ) (x:xs)))

--lines
lines' "" = []
lines' (x:xs) = if x == '\n' then "":lines' xs else [takeWhile' ('\n' /= ) (x:xs)] ++ lines' (delete' '\n' (dropWhile' ('\n' /= ) (x:xs)))

--unlines
unlines' ln = intercalate "\n" ln ++ "\n"

--unwords
unwords' wds = intercalate " " wds

--takeWhile
takeWhile' _ [] = []
takeWhile' f (x:xs) = if f x then x : takeWhile' f xs else []

--dropWhile
dropWhile' _ [] = []
dropWhile' f (x:xs) = if f x then dropWhile' f xs else x : xs


--desperado
desperado a = [1..a]
--concatMap
concatMap' f [] = []
concatMap' f (x:xs) = f x ++ concatMap' f xs

--all
all' f [] = True
all' f (x:xs) = if f x then all' f xs else False

--any
any' f [] = False
any' f (x:xs) = if f x then True else any' f xs

--insert
insert2 a [] = [a]
insert2 a (x:xs) = if a > x then x : insert2 a xs else a : insert2 x xs


--partition
partition' _ [] = ([], [])
partition' f xs = ((filter' f xs), (remove' f xs))

--plus3
plus3 a b c = a + b + c

--zipWith3
zipWith32 f [] _ _ = []
zipWith32 f _ [] _ = []
zipWith32 f _ _ [] = []
zipWith32 f (x:xs) (y:ys) (z:zs) = f x y z : zipWith32 f xs ys zs

--replicate
replicate' 0 xs = []
replicate' a xs = [xs] ++ replicate' (pred a) xs

--Lazy: iterate, repeat, cycle

--iterate
iterate' f a = a : (iterate' f (f a))

--repeat
repeat' a = a : (repeat' a)

--cycle
cycle' xs = xs ++ (cycle' xs)

--Math: isPrime, primesUnder, fibo, fiboList, pascal,pascalAll, fak, fakList
--fak
fak a = a * (fak (pred a))

--bonus track: sequences, permutations
