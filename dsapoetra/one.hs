module One where

import Data.List

mutlak x = if x>0 then x else (- x)

mutlak' x
  | x >= 0 =x
  | otherwise = (- x)

fak x
  |x == 0 = 1
  |otherwise = x *(fak $ x - 1)

fak2 0 = 1
fak2 i = i * (fak2 $ pred i)

isPrime x
  | x < 2 = False
  | x == 2 = True
  | even x = False
  | otherwise = iter 3
  where iter i
          | (i*i) > x = True
          | 0 == rem x i = False
          | otherwise = iter $ i + 2

-- fakIter i = iter 1 1
  -- where iter n res
    -- | n > i = res
-- | otherwise = iter (n+1) (res*n)

-- 1a- Reimplement :

-- do not use any other function except : pred, succ, ++, :, pattern matching, |
--pattern match use only (x:xs)/(x:[]). do not use (x:y:xs)
--24
-- null X, take X, drop X, fst X, snd X,  delete X, elem X, notElem X, head X, length X, reverse X, last X, tail X, init X, max X, min X, and X, or X,  sum X,
-- foldl X,  (!!) -> ganti jadi nth X,insert X, zipWith3, map X,

--22
--  product, words, lines, unlines, unwords, takeWhile, dropWhile, concatMap, all, any,  filter,zip3,
--csort, scanl, scanl1, concat, intersperse, deleteAll, foldl1, zip, zipWith,intercalate,
testcase = [1,2,3,4]

null' [] = True
null'(x:xs) = False


take' i [] = []
take' i (x:xs)
  | i <= 1 = [x]
  | otherwise = x:take' (i-1) xs

drop' i [] = []
drop' i (x:xs)
  | i <= 0 = (x:xs)
  | i == 1 = xs
  | otherwise = drop' (i-1) xs

fst' (x,x2) = x

snd' (x,x2) = x2

head' (x:xs) = x

sum' []=0
sum' (x:xs) = x + (sum' xs)

last' [x] = x
last' (x:xs) = last' (xs)

tail' [x] = []
tail' (x:xs) = xs

init' [x] = []
init' (x:xs) = x:init' xs

and' [] = True
and' [x] = x
--and' (x:xs) = x && (and' xs)
and' (x:xs)
  | x == True = test x
  | x == False = test x
  where test x
          | x == and' xs = x
          | otherwise = False


or' [] = False
or' [x] = x
--or' (x:xs) = x || (and' xs)
or' (x:xs)
  | x == True = True
  | x == False = test x
  where test x
          | head' xs == True = True
          | otherwise = or' xs

length' [] = 0
length' (x:xs) = 1+length' (xs)

reverse' [] = []
reverse' x = last' x:reverse' (init' x) -- everything i used here is predefined by me

delete' i [] = []
delete' i (x:xs)
  | i == x = xs
  | otherwise = test x
  where test x
          | i == (head' xs) = x:(tail' xs)
          | otherwise = x:delete' i xs

--deleteAll i [] =[]
--deleteAll i (x:xs) = deleteAll i (delete' i (x:xs))
-- foldl = function nerima function dan elemen, dan list outputnya adalah elemen

max' x y
  | x > y = x
  | x < y = y
  | otherwise = x

min' x y
  | x > y = y
  | x < y = x
  | otherwise = x

elem' i [] = False
elem' i (x:xs)
  | i == x = True
  | otherwise = elem' i xs

notElem' i [] = True
notElem' i (x:xs)
  | i == x = False
  | otherwise = notElem' i xs

nth [x] 0 = x
nth (x:xs) i
  | i == 0 = x
  | otherwise = nth xs (i-1)

-- Yang ini da
zip' [] [] = []
zip' (x:xs) (y:ys)
  | length' (x:xs) == length' (y:ys) = [(x,y)]:zip' xs ys
  | length' (x:xs) > length' (y:ys) = zip' (take' (length' (y:ys)) x:xs) (y:ys)
  | length' (x:xs) < length' (y:ys) = zip' (x:xs) (take' (length' (x:xs)) y:ys)

foldla' func i [] = i
foldla' func i (x:xs) = foldla' func (func i x) xs

insert' i [] = [i]
insert' i (x:xs) = (x:xs) ++ [i]

map' func [] = []
map' func (x:xs) = (func x):map' func xs
