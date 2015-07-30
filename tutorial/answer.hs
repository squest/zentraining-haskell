module Answer where

import Data.List

null' [] = True
null' _ = False

take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x : take' (pred n) xs

drop' 0 xs = xs
drop' _ [] = []
drop' n (x:xs) = drop' (pred n) xs

fst' (a,b) = a
snd' (a,b) = b

map' f [] = []
map' f (x:xs) = f x : map' f xs

filter' f [] = []
filter' f (x:xs)
  | f x = x : filter' f xs
  | otherwise = filter' f xs

delete' x [] = []
delete' e (x:xs)
  | e == x = xs
  | otherwise = x : delete' e xs

deleteAll x [] = []
deleteAll e (x:xs)
  | e == x = deleteAll e xs
  | otherwise = x: deleteAll e xs

nub' [] = []
nub' (x:xs) = x : (nub' $ deleteAll x xs)

mfoldl f i [] = i
mfoldl f i (x:xs) = mfoldl f (f i x) xs


zipWith' f _ [] = []
zipWith' f [] _ = []
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys

nth (x:xs) 0 = x
nth (x:xs) n = nth xs (pred n)

sort' [] = []
sort' (x:[]) = [x]
sort' (x:xs) = (sort' $ filter' (<= x) $ xs) ++
               [x] ++
               (sort' $ filter (> x ) xs)

last' (x:[]) = x
last' (x:xs) = last' xs

reverse' [] = []
reverse' (x:xs) = (reverse xs) ++ [x]

elem' e [] = False
elem' e (x:xs)
  | e == x = True
  | otherwise = elem' e xs

notElem' e [] = True
notElem' e (x:xs)
  | e == x = False
  | otherwise = notElem' e xs

head' (x:xs) = x

length' [] = 0
length' (x:xs) = 1 + (length' xs)

tail' (x:xs) = xs

init' (x:[]) = []
init' (x:xs) = x : init' xs

max' a b = if a > b then a else b
min' a b = if a > b then b else a

maximum' [x] = x
maximum' (x:xs) = max' x (maximum' xs)

minimum' [x] = x
minimum' (x:xs) = min' x (minimum' xs)

scanl' f i [] = [i]
scanl' f i (x:xs) = i : scanl' f (f i x) xs

scanl1' f (x:xs) = scanl' f x xs

iterate' f i = i : iterate' f (f i)

takeWhile' f [] = []
takeWhile' f (x:xs)
  | f x = x : takeWhile' f xs
  | otherwise = []

inits' [] = [[]]
inits' lst = reverse' $ map reverse' $ tails' $ reverse' lst

tails' [] = [[]]
tails' (x:xs) = (x:xs) : tails' xs

concat' [] = []
concat' (x:xs) = x ++ concat' xs

and' [] = True
and' (x:xs)
  | x = and' xs
  | otherwise = False

or' [] = False
or' (x:xs)
  | x = True
  | otherwise = or' xs

zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

zip3' [] _ _ = []
zip3' _ [] _ = []
zip3' _ _ [] = []
zip3' (x:xs) (y:ys) (z:zs) = (x,y,z) : zip3' xs ys zs

product' [] = 1
product' (x:xs) = x * (product' xs)

words' [] = []
words' lst = iter [] [] lst
  where iter cur res [] = res ++ [cur]
        iter cur res (x:xs)
          | x == ' ' = iter [] (res ++ [cur]) xs
          | otherwise = iter (cur++[x]) res xs

intercalate' a [x] = x
intercalate' a (x:xs) = x ++ a ++ intercalate' a xs

intersperse' a [x] = [x]
intersperse' a (x:xs) = x:a:intersperse' a xs










































-- save
