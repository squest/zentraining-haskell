module Feedback where

import Data.List

-- ISPRIME KW ====>>>>> (Si babi masih ga jalan)
belIsPrime n
  | n < 2 = False
  | n == 2 = True
  | even n = False
  | otherwise = (any (\a -> ((rem n a) == 0)) [3..(sqrt n)])

 -- mending bikin oddPrime aja karena lebih ngaruh

oddPrime n = iter 3
  where iter i
          | i > div n i = True
          | 0 == rem n i = False
          | otherwise = iter $ i+2

-- kalo mau versi elo, bisa gini, tapi ini sebenernya buggy, karena 3 pasti ga masuk

{-
isPrime :: Integral a => a -> Bool
isPrime n
    | n < 2 = False
    | n == 2 = True
    | even n = False
    | otherwise = all (\x -> (rem n x) /= 0) [3..(round (sqrt n))]
-}
-- another one, function yg dimasukin ke takeWhile tuh a trick untuk ngecheck whether dia seukuran sama sqrt n

isPrime' n
  | n < 2 = False
  | n == 2 = True
  | even n = False
  | otherwise = all (\x -> (rem n x) /= 0) $ takeWhile (\x -> x <= div n x) [3,5..]

difference l [] = l
difference [] l = []
difference (x:xs) l
  | elem x l = difference xs l
  | otherwise = x : difference xs l

permutations' [] = [[]]
permutations' xs = iter (length xs) (map (\x -> [x]) xs)
  where iter 1 res = res
        iter n res = iter (n-1) waka
          where waka = concatMap (\x -> map (\k -> x ++ [k]) $ difference xs x) res

nth = (!!)

belHaha n (y:ys) = (map (\a -> [n] ++ a) (belPermutations (y:ys)))

belPermutations [] = [[]]
belPermutations (x:[]) = [[x]]
belPermutations (x:xs) = iter 0 (x:xs)
  where iter i l
          | (length l - i) == 1 = belHaha (nth l i) (delete (nth l i) l)
          | otherwise = (belHaha (nth l i) (delete (nth l i) l)) ++ (iter (succ i) l)






























--save
