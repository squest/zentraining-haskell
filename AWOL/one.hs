import Data.List

null' [] = True
null' _ = False

take' _ [] = []
take' 0 _ = []
take' n (x:xs) = x : take' (pred n) xs

drop' _ [] = []
drop' 0 (x:xs) = x:xs
drop' n (x:xs) = drop' (pred n) xs

fst' (a, b) = a

snd' (a, b) = b

map' _ [] = []
map' f (x:xs) = [f x] ++ map' f xs

filter' _ [] = []
filter' f (x:xs) = if f x then x : filter' f xs else filter' f xs

delete' _ [] = []
delete' n (x:xs)
  | n == x = xs
  | otherwise = x : (delete' n xs)

-- nub, foldl, foldl1, zip, zipWith, (!!) -> ganti jadi nth,

any' f [] = False
any' f (x:xs) = if (f x) == True then True else any' f xs

sort' [] = []
sort' (x:xs) = if (any' (\a -> a < x) xs) then sort' (xs ++ [x]) else [x] ++ sort' xs

-- scanl, scanl1,

inits' [] = [[]]
inits' xs = inits' (init' xs) ++ [xs]

tails' [] = [[]]
tails' xs = xs : tails' (tail' xs)

elem' _ [] = False
elem' n (x:xs)
  | x == n = True
  | otherwise = elem' n xs

notElem' _ [] = True
notElem' n (x:xs)
    | x == n = False
    | otherwise = notElem' n xs

reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

head' (x:xs) = x

last' [x] = x
last' (x:xs) = last' xs

length' [] = 0
length' (x:xs) = 1 + length' xs

init' [] = [] --butlast
init' (x:xs)
  | length (x:xs) == 1 = []
  | otherwise = [x] ++ init' xs

tail' [] = [] --rest
tail' (x:xs) = xs

max' a b
  | a > b = a
  | otherwise = b

min' a b
  | a < b = a
  | otherwise = b

maximum' (x:y:xs)
  | xs == [] = if x > y then x else y
  | otherwise = maximum' (y:xs)

maximum'' (x:xs)
  | xs == [] = x
  | otherwise = max' x (maximum'' xs)

minimum' (x:y:xs)
  | xs == [] = if x < y then x else y
  | otherwise = minimum' (y:xs)

-- concat, union, intersect, intersperse, intercalate,

and' [] = True
and' (x:xs) = if x == True then and' xs else False

or' [] = False
or' (x:xs) = if x == True then True else or' xs


-- group, zip3

sum' [] = 0
sum' (x:xs) = x + sum' xs

product' [] = 1
product' (x:xs) = x * product' xs

splitAt' _ [] = []
splitAt' n x = take n x : [drop n x]

-- words, lines, unlines, unwords, takeWhile, dropWhile, concatMap, all,

insert' a [] = [a]
insert' a (x:xs) = if a > x then (x:xs) ++ [a] else a : (x:xs)
-- partition, zipWith3
