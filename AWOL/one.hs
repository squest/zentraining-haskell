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


--map, filter, delete, nub, foldl, foldl1, zip, zipWith, (!!) -> ganti jadi nth,
--sort, scanl, scanl1, inits, tails

elem' _ [] = False
elem' n (x:xs)
  | x == n = True
  | otherwise = elem' n xs

notElem' _ [] = True
notElem' n (x:xs)
    | x == n = False
    | otherwise = notElem' n xs

head' (x:xs) = x

length' [] = 0
length' (x:xs) = 1 + length' xs

--reverse' [] = []
--reverse' (x:xs) = reverse' xs (: x)

last' [x] = x
last' (x:xs) = last' xs

tail' [] = []
tail' (x:xs) = xs


--init, max, min, minimum, maximum, concat, union, intersect, intersperse, intercalate, and, or, group, zip3, sum, product, splitAt, words, lines, unlines, unwords, takeWhile, dropWhile, concatMap, all, any, insert, partition, zipWith3
