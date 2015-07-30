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

-- DELETE ALL KW
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
