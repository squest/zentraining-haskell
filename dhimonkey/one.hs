module WorkSheet where

--null, elem, notElem, head, length, reverse, tail, init, last, take, drop,
-- concat, max, min, maximum, minimum, inits, tails, repeat, cycle, union,
-- intersect, fst, snd, itersperse, intercalate, and, or, group, zip, zip3,
-- sort, nub, delete, sum, product, splitAt, words, lines, unlines, unwords,
--subsequences, permutations.

--special (!!) => ganti jadi elke, (++) => ganti jadi cats,

import Data.List

null' [] = True
null' _ = False

elem' x [] = False
elem' a (x:xs)
  | a == x = True
  | otherwise = elem' a xs

notElem' x [] = True
notElem' a (x:xs)
  | a == x = False
  | otherwise = notElem' a xs

head' (x:xs) = (x)

length' [] = 0
length' [x] = 1
length' (x:xs) = 1 + (length' xs)

--reverse' [] = []
--reverse' (x:xs) = [last' (x:xs)] ++ reverse' (init' x:xs)
reverse' [] = []
reverse' (x) = [last' x] ++ reverse' (init' x)

-- reverse' [1..10] = [10] ++ reverse' (init' [1..10])
-- reverse' [1..10] = [10] ++ reverse' ([1..9])
-- reverse' [1..9] = [9] ++ reverse' ([1..8])........
-- reverse' [1] = [1]  ++ reverse' []

tail' (x:xs) = (xs)

init' [x] = []
init' (x:xs) = x : init' xs
init' [] = error "won't work, yo mama so fat!!"

--and' [] = True
--and' (x:xs)
  -- | x == False = False
  -- | otherwise = True && and' xs

last' [x] = x
last' (x:xs) = last' xs
last' [] = error "won't work, yo mama so fat!"

repeat' x = x : repeat' x

take' 0 _ = []
take' a [] = []
take' a (x:xs) = x : take (a-1) xs

drop' 0 (x:xs) = x:xs
drop' a [] = []
drop' a (x:xs) = drop' (a-1) xs

concat' [] = []
concat' [[]] = []
concat' [[a]] = [a]
concat' (x:xs) = x ++ concat' xs

max' a b
  | a > b = a
  | otherwise = a

min' a b
  | a > b = b
  | otherwise = a

maximum' [a] = a
maximum' (x:xs) = max x (maximum xs)

minimum' [a] = a
minimum' (x:xs) = min x (minimum' xs)

tails' [] = [[]]
tails' [a] = [[a],[]]
tails' (x:xs) = (x:xs) : (tails' xs)

inits' [] = [[]]
inits' (x:xs) = inits' [x] ++ (inits' xs)

--union' [] [] = []
--intersect'

fst' (a,b) = a
snd' (a,b) = b

intersperse' _ [] = []
intersperse' a [b,c] = [b,a,c]
intersperse' a (x:xs) = x:a:(intersperse' a xs)

intercalate' a [] = []
intercalate' a [[]] = []
intercalate' a [b] = b
intercalate' (a) (x:xs) = (x ++ a) ++ intercalate' a xs

group' [a] = [[a]]
group' (x:xs) = [x] : group' xs

zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' (xs) (ys)

zip3' [] _ _ = []
zip3' _ [] _ = []
zip3' _ _ [] = []
zip3' (x:xs) (y:ys) (z:zs) = (x,y,z) : zip3' (xs)(ys)(zs)

--sort' [] = []
--sort' (x:y:ys)
  -- | x <= y = (x:y:sort' ys)
  -- | y > x = (y:x:sort' ys)

--nub' [] = []
--nub' [a] = [a]
--nub' (x:y:xs)
  --  | x == y = x:(nub' xs)
  --  | x /=y = x:y:(nub' xs)

deleteAll' _ [] = []
deleteAll' a (x:xs)
  | a == x = deleteAll' x xs
  | a /= x = x : (deleteAll' a xs)

delete' _ [] = []
delete' a (x:xs)
  | a == x = xs
  | a /= x = x : (delete' a xs)

foldl' 

sum' [] = 0
sum' (x:xs) = x + (sum' xs)

product' [] = 1
product' (x:xs) = x * (product' xs)

--splitAt' _ [] = ([],[])
--splitAt' 0 [x] = ([],[x])
--splitAt a (x:xs) = ([x]++(splitAt' (a-1) xs))

--words' "" = []
--words' a = [a]




-- nub [1,1,3,4,4,2]
-- nub [1,3,4,4,2]
-- nub [1,3,4,4,2]

-- delete' a [] = []
-- delete' 0 [a] = [a]
-- delete' a (x:xs)
--   | a == x = delete' a xs
--   | otherwise = x : (delete' a (xs))

--subsequences' [] = [[]]
--subsequences' [a] = [[], [a]]
--subsequences' (x:xs) = [[x]] ++ subsequences' xs

cycle' (x:xs) = x:xs ++ cycle' (x:xs)
cycle' [] = error "won't work, yo mama so fat!"



--  nth' (x:xs) a
  --  | a == 0 = x
  --  | a < 0 = error "won't work, yo mama so fat!"
  --  | a >= last (x:xs) = error "won't work, yo mama so fat!"
