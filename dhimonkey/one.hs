module WorkSheet where

--null, take, drop, fst, snd, map, filter, delete, deleteAll, foldl, foldl1, zip, zipWith, (!!) -> ganti jadi nth,
--sort, scanl, scanl1, elem, notElem, head, length, reverse, last, tail, init, max, min, concat, intersperse, intercalate, and, or, zip3, sum, product, words, lines, unlines, unwords, takeWhile, dropWhile, concatMap, all, any, insert, zipWith3

--nub, sort, minimum, maximum, inits, tails, union, intersect, group, splitAt, partition, replicate, subsequences, permutations

--iterate, repeat, cycle

--isPrime, primesUnder, fibo, fiboList, pascal, pascalAll, fak, fakList



null' [] = True
null' _ = False

take' 0 _ = []
take' a [] = []
take' a (x:xs) = x : take (a-1) xs

drop' 0 (x:xs) = x:xs
drop' a [] = []
drop' a (x:xs) = drop' (a-1) xs

fst' (a,b) = a
snd' (a,b) = b

map' f [] = []
map' f (x:xs) = (f x) : (map' f xs)

filter' f [] = []
filter' f (x:xs)
  | (f x)  = x : filter' f xs
  | otherwise = filter' f xs
-- tanya sabda
-- knp nggak filter' f (x:xs)
  -- | (f x) == True = x : filter' f xs
  -- | otherwise = filter' f xs

deleteAll' _ [] = []
deleteAll' a (x:xs)
  | a == x = deleteAll' x xs
  | a /= x = x : (deleteAll' a xs)

delete' _ [] = []
delete' a (x:xs)
  | a == x = xs
  | a /= x = x : (delete' a xs)

foldli' f a [] = a
foldli' f a (x:xs) = foldli' f (f a x) xs

foldl1' f [a] = a
foldl1' f (x:xs) = (f x (foldl1' f xs))

-- foldl1' (+) [1..4] = (+) 1 ((+) 2 ((+) 3 (4)))

zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' (xs) (ys)

zipWith' f (x:xs) (y:ys) =  (f x y) : (zipWith' f (xs)(ys))

nth' (x:xs) a
  | a == 0 = x
  | otherwise = nth' xs (a-1)

--sort' [] = []
--sort' (x:xs)
-- | x == pred (x = x : sort' xs
-- | otherwise = sort' (xs ++ [x])

scanl' f a [] = [a]
scanl' f a (x:xs) = (a : (scanl' f (f a x) xs))

scanl1' f [] = []
scanl1' f [a] = [a]
scanl1' f (x:xs) = (scanl' f x xs)


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




last' [x] = x
last' (x:xs) = last' xs
last' [] = error "won't work, yo mama so fat!"



max' a b
  | a > b = a
  | otherwise = a

min' a b
  | a > b = b
  | otherwise = a


concat' [] = []
concat' [[]] = []
concat' [[a]] = [a]
concat' (x:xs) = x ++ concat' xs

intersperse' _ [] = []
intersperse' a [b,c] = [b,a,c]
intersperse' a (x:xs) = x:a:(intersperse' a xs)

intercalate' a [] = []
intercalate' a [[]] = []
intercalate' a [b] = b
intercalate' (a) (x:xs) = (x ++ a) ++ intercalate' a xs

and' [] = True
and' (x:xs)
   | x == False = False
   | otherwise = and' xs

or' [] = False
or' (x:xs)
   | x == False = or' xs
   | otherwise = True


zip3' [] _ _ = []
zip3' _ [] _ = []
zip3' _ _ [] = []
zip3' (x:xs) (y:ys) (z:zs) = (x,y,z) : zip3' (xs)(ys)(zs)


sum' [] = 0
sum' (x:xs) = x + (sum' xs)

product' [] = 1
product' (x:xs) = x * (product' xs)

--splitAt' _ [] = ([],[])
--splitAt' 0 [x] = ([],[x])
--splitAt a (x:xs) = ([x]++(splitAt' (a-1) xs))

words' "" = []
words' (x:xs)
  | x == ' ' = words' xs
  | otherwise = x : words' xs

--words' "12 231"


maximum' [a] = a
maximum' (x:xs) = max x (maximum' xs)

minimum' [a] = a
minimum' (x:xs) = min x (minimum' xs)

tails' [] = [[]]
tails' [a] = [[a],[]]
tails' (x:xs) = (x:xs) : (tails' xs)

inits' [] = [[]]
inits' (x:xs) = inits' [x] ++ (inits' xs)



--union' [] [] = []
--intersect'





group' [a] = [[a]]
group' (x:xs) = [x] : group' xs



--sort' [] = []
--sort' (x:y:ys)
  -- | x <= y = (x:y:sort' ys)
  -- | y > x = (y:x:sort' ys)

--nub' [] = []
--nub' [a] = [a]
--nub' (x:y:xs)
  --  | x == y = x:(nub' xs)
  --  | x /=y = x:y:(nub' xs)


--foldl' f 0 _ = 0
--foldl' f a [] = a
--foldl' f a (x:xs) = f a (x:xs)

insert' a (x:xs) = a:(x:xs)





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

repeat' x = x : repeat' x

cycle' (x:xs) = x:xs ++ cycle' (x:xs)
cycle' [] = error "won't work, yo mama so fat!"



--  nth' (x:xs) a
  --  | a == 0 = x
  --  | a < 0 = error "won't work, yo mama so fat!"
  --  | a >= last (x:xs) = error "won't work, yo mama so fat!"
