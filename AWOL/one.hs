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
map' f (x:xs) = [(f x)] ++ [(map' xs)]

--map, filter,

delete' _ [] = []
delete' n (x:xs)
  | n == x = xs
  | otherwise = x : (delete' n xs)

-- nub, foldl, foldl1, zip, zipWith, (!!) -> ganti jadi nth,


--sort, scanl, scanl1, inits, tails

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

init' [] = []
init' (x:xs) = [x]

tail' [] = []
tail' [x] = [x]
tail' (x:xs) = tail' xs

--tails' [] = []
--tails' (x:xs)
--  | = tail' xs

--init' [] = []
--init' (x:xs) = [x]

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

-- concat, union, intersect, intersperse, intercalate, and, or, group, zip3,

sum' [] = 0
sum' (x:xs) = x + sum' xs

product' [] = 1
product' (x:xs) = x * product' xs

--splitAt' 0 (x:xs) = (x:xs)
--splitAt' n (x:xs)
--  | xs == [] = x
--  | otherwise = 1

--words' "x"
--  | x == " " = []
--  | otherwise = ["x"]

-- words, lines, unlines, unwords, takeWhile, dropWhile, concatMap, all, any, insert, partition, zipWith3
