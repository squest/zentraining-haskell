module One where

import Data.List

square x = x*x

tes =[1,2,3,4]

sqr x = x*x

cube x = x*x*x
cons x xs =x:xs

multiple3 = (3*)

mutlak x = if x>0 then x else (- x)

mutlak' x
  | x >= 0 =x
  | otherwise = (- x)

fak x
  |x == 0 = 1
  |otherwise = x *(fak $ x - 1)

fak2 0 = 1
fak2 i = i * (fak2 $ pred i)

-- isPrime x
  -- | x < 2 = False
  -- | x == 2 = True
  -- | even x = False
  -- | otherwise = iter 3
  -- where iter i
    -- | (i*i) > x = True
    -- | 0 == rem x i = False
    -- | otherwise = iter $ i + 2

-- fakIter i = iter 1 1
  -- where iter n res
    -- | n > i = res
-- | otherwise = iter (n+1) (res*n)






-- 1a- Reimplement :

-- do not use any other function except : pred, succ, ++, :, pattern matching, |
--pattern match use only (x:xs)/(x:[]). do not use (x:y:xs)

-- null X, take, drop, fst X, snd X, map, filter, delete, deleteAll, foldl, foldl1, zip, zipWith, (!!) -> ganti jadi nth,
--csort, scanl, scanl1, elem, notElem, head X, length X, reverse, last X, tail X, init X, max, min, concat, intersperse, intercalate,
-- and X, or X, zip3, sum X,
--product, words, lines, unlines, unwords, takeWhile, dropWhile, concatMap, all, any, insert, zipWith3

testcase = [1,2,3,4]

null' [] = True
null'(x:xs) = False


--take' i [] = []
--take' i (x:xs)
  -- | i <= 1 = [x]
  -- | otherwise = (take' $ (i-1) (xs))

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
init' [x,x2] = [x]
init' (x:xs) = x:init' xs

and' [] = True
and' [x] = x
and' (x:xs) = x && (and' xs)

or' [] = False
or' [x] = x
or' (x:xs) = x || (and' xs)

length' [] = 0
length' [x] = 1
length' (x:xs) = 1+length' (xs)
