module One where

import Data.List

-- NGETES JALAN
squa x = x*x

-- NULL KW
belNull [] = True
belNull (x:xs) = False
-- belNull _ = False (dari Peeta)

-- TAKE KW
belTake i [] = []
belTake 0 _ = []
belTake i (x:xs) = [x] ++ (belTake (pred i) xs)

-- DROP KW
belDrop i [] = []
belDrop 0 l = l
belDrop i (x:xs) = (belDrop (pred i) xs)

-- FST KW
belFst (a, b) = a

-- SND KW
belSnd (a, b) = b

-- MAP KW
belMap f [] = []
belMap f (x:xs) = (f x):(belMap f xs)

-- FILTER KW
belFilter f [] = []
belFilter f (x:xs)
  | True == (f x) = x:(belFilter f xs)
  | otherwise = (belFilter f xs)

-- DELETE KW
belDelete a [] = []
belDelete a (x:xs)
  | a == x = xs
  | otherwise = x:(belDelete a xs)

-- DELETEALL KW
belDeleteAll a [] = []
belDeleteAll a (x:xs)
  | a == x = (belDeleteAll a xs)
  | otherwise = x:(belDeleteAll a xs)

-- FOLDL KW
belFoldl f b [] = b
belFoldl f b (x:xs) = (belFoldl f (f b x) xs)

-- FOLDL1 KW
belFoldl1 f (x:[]) = x
belFoldl1 f (x:xs) = (belFoldl f x xs)

-- ZIP KW
belZip _ [] = []
belZip [] _ = []
belZip (x:xs) (y:ys) = [(x, y)] ++ (belZip xs ys)

-- ZIPWITH KW
belZipWith f _ [] = []
belZipWith f [] _ = []
belZipWith f (x:xs) (y:ys) = [((f x y))] ++ (belZipWith f xs ys)

-- !! KW
nth (x:xs) i
  | i == 0 = x
  | otherwise = nth xs (pred i)

-- SORT KW
belInsert a [] = [a]
belInsert a (x:xs)
  | a < x = a:(x:xs)
  | a >= x = x:(belInsert a xs)

belSort [] = []
belSort (x:xs) = belInsert x (belSort xs)

-- SCANL KW
belScanl f a [] = [a]
belScanl f a (x:xs) = [a] ++ (belScanl f (f a x) xs)

-- SCANL1 KW
belScanl1 f [] = []
belScanl1 f (x:xs) = (belScanl f x xs)

-- ELEM KW
belElem a [] = False
belElem a (x:xs)
  | a == x = True
  | otherwise = belElem a xs

-- NOTELEM KW
belNotElem a [] = not (belElem a [])
belNotElem a (x:xs) = not (belElem a (x:xs))

-- HEAD KW
belHead (x:xs) = x

-- LENGTH KW
belLength [] = 0
belLength (x:xs) = 1 + (belLength xs)

-- REVERSE KW
belReverse [] = []
belReverse (x:xs) = (belReverse xs) ++ [x]

-- LAST KW
belLast (x:xs)
  | xs == [] = x
  | otherwise = belLast xs
