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

sum' []=0
sum' (x:xs) = x + (sum' xs)

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

-- null, take, drop, fst, snd, map, filter, delete, deleteAll, foldl, foldl1, zip, zipWith, (!!) -> ganti jadi nth,
--csort, scanl, scanl1, elem, notElem, head, length, reverse, last, tail, init, max, min, concat, intersperse, intercalate, and, or, zip3, sum,
--product, words, lines, unlines, unwords, takeWhile, dropWhile, concatMap, all, any, insert, zipWith3

null' [] = True
null'(x:xs) = False


take' i [] = []
take' i (x:xs)
  | i <= 1 = [x]
  | otherwise = ulang i
  where ulang i
  |
