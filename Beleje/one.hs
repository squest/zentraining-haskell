module One where

import Data.List

-- NGETES JALAN
squa x = x*x

---------------------------- 1.A Pup ----------------------------
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

-- TAIL KW
belTail (x:xs) = xs

-- INIT KW
belInit (x:xs)
  | xs == [] = []
  | otherwise = x:(belInit xs)

-- MAX KW
belMax a b
  | a > b = a
  | otherwise = b

-- MIN KW
belMin a b
  | a < b = a
  | otherwise = b

-- CONCAT KW
belConcat [] = []
belConcat (x:xs)
  | xs == [] = x
  | otherwise = x ++ (belConcat xs)

-- INTERSPERSE KW
belIntersperse a [] = []
belIntersperse a (x:xs)
  | xs == [] = [x]
  | otherwise = x:a:(belIntersperse a xs)

-- INTERCALATE KW
belIntercalate _ [[]] = []
belIntercalate l (y:ys)
  | ys == [] = y
  | otherwise = y ++ l ++ (belIntercalate l ys)

-- AND KW
belAnd [] = True
belAnd (x:xs)
  | x == True = (belAnd xs)
  | otherwise = False

-- OR KW
belOr [] = False
belOr (x:xs)
  | x == True = True
  | otherwise = (belOr xs)

-- ZIP3 KW
belZip3 [] _ _ = []
belZip3 _ [] _ = []
belZip3 _ _ [] = []
belZip3 (x:xs) (y:ys) (z:zs) = [(x, y, z)] ++ (belZip3 xs ys zs)

-- SUM KW
belSum [] = 0
belSum (x:xs) = belFoldl1 (+) (x:xs)

-- PRODUCT KW
belProduct [] = 1
belProduct (x:xs) = belFoldl1 (*) (x:xs)

-- WORDS KW
belWords [] = []
belWords (x:xs) = round 0 (x:xs)
  where round i l
          | belNotElem ' ' l = [l]
          | nth l i == ' ' = [belTake i l] ++ (round 0 (belDrop (succ i) l))
          | otherwise = round (succ i) l

-- LINES KW
belLines "" = []
belLines a = [a]

-- UNLINES KW
belUnlines [] = ""
belUnlines (x:xs)
  | x == [] = []
  | otherwise = x ++ "\n" ++ belUnlines xs

-- UNWORDS KW
belUnwords [] = ""
belUnwords l = (intercalate [' '] l)

-- TAKEWHILE KW
belTakeWhile f [] = []
belTakeWhile f (x:xs)
  | (f x) = x:(belTakeWhile f xs)
  | otherwise = []

-- DROPWHILE KW
belDropWhile f [] = []
belDropWhile f (x:xs)
  | (f x) = (belDropWhile f xs)
  | otherwise = (x:xs)

-- CONCATMAP KW
belConcatMap f [] = []
belConcatMap f (x:xs) = f x ++ (belConcatMap f xs)

-- ALL KW
belAll f [] = True
belAll f l = (and (map f l))

-- ANY KW
belAny f [] = False
belAny f l = (or (map f l))

-- INSERT KW
belInsert2 a (x:xs) = belSort a:(x:xs)

-- ZIPWITH3
belZipWith3 f _ _ [] = []
belZipWith3 f _ [] _ = []
belZipWith3 f [] _ _ = []
belZipWith3 f (x:xs) (y:ys) (z:zs) = [f (f x y) z] ++ (belZipWith3 f xs ys zs)

---------------------------- 1.B Puppy ----------------------------
-- NUB KW
belNub [] = []
belNub (x:xs)
  | belElem x xs = x:belNub(belDeleteAll x xs)
  | otherwise = x:belNub xs

-- MINIMUM KW
belMinimum (x:[]) = x
belMinimum (x:xs) = (min x (belMinimum xs))

-- MAXIMUM KW
belMaximum (x:[]) = x
belMaximum (x:xs) = (max x (belMaximum xs))

-- INITS KW
belInits [] = [[]]
belInits (x:xs) = (belInits (belInit (x:xs))) ++ [(x:xs)]

-- TAILS KW
belTails [] = [[]]
belTails (x:xs) = [(x:xs)] ++ (belTails (belTail (x:xs)))

-- UNION KW
belUnion l [] = l
belUnion [] l = l
belUnion l (x:xs)
  | belElem x l = belUnion l xs
  | otherwise = belUnion (l ++ [x]) xs

-- INTERSECT KW
belIntersect l [] = []
belIntersect [] l = []
belIntersect (x:xs) (y:ys)
  | belElem x (y:ys) = x:belIntersect xs (y:ys)
  | otherwise = belIntersect xs (y:ys)

-- GROUP KW
belGroup [] = []
belGroup (x:xs) = [x]:(belGroup xs)

-- SPLITAT KW
belSplitAt n [] = (belTake n [], belDrop n [])
belSplitAt n (x:xs) = (belTake n (x:xs), belDrop n (x:xs))

-- PARTITION KW
belPartition f [] = ([],[])
belPartition f l = (filter f l, filter (\n -> not (f n)) l)

-- REPLICATE KW
belReplicate 0 _ = []
belReplicate n l = [l] ++ belReplicate (pred n) l

-- SUBSEQUENCES KW

-- PERMUTATIONS KW

---------------------------- 2 Pubs ----------------------------
-- ITERATE KW
belIterate f n = n:(belIterate f (f n))

-- REPEAT KW
belRepeat n = n:(belRepeat n)

-- CYCLE KW
belCycle l = l ++ belCycle l
---------------------------- 3 Pops ----------------------------
-- FACTORIAL
belFact 0 = 1
belFact n = n * (belFact (pred n))

-- FACTORIAL LIST
belFactL 0 = [1]
belFactL n = n:(belFactL (pred n))

-- FIBBO
belFib 1 = 1
belFib 2 = 1
belFib n = belFib (n - 2) + belFib (n - 1)

-- FIBBO LIST
belFibL n = map belFib [1..n]

-- PASCAL
belPascal 1 = [1]
belPascal n = zipWith (+) (0:(belPascal (pred n))) (belPascal (pred n) ++ [0])

-- PASCAL LIST
belPascalL n = map belPascal [1..n]

-- PRIME
-- PRIME BELOW
