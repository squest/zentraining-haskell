module Test where

--elem
elem' a [] = False
elem' a (x:xs)
  | x == a = True
  | otherwise = elem' a xs


notElem' a (x:xs)
  | elem' a (x:xs) = False
  | otherwise = True

head' (x:xs) = x

length' [] = 0
length' (x:xs) = 1 + length' xs


reverse' [] = []
reverse' (x:xs) =  reverse' xs ++ [x]

tail' (x:xs) = xs

init' xs = reverse' $ tail' $ reverse' xs

last' [x] = x
last' (x:xs) = last' xs

take' _ [] = []
take' 0 xs = []
take' a (x:xs) = x: (take' (a - 1) xs)

drop' 0 xs = xs
drop' 1 (x:xs) = xs
drop' a (x:xs) = (drop' (a-1) xs)


max' x y
  | x >= y = x
  | x <= y = y


maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

min' x y
  | x <= y = x
  | y <= x = y

minimum' [x] = x
minimum' (x:xs) = min' x (minimum' xs)

repeat' x = x: repeat' x

cycle' (x:xs) = (x:xs) ++ cycle' (x:xs)

buang a b
  | a == b = a:[]
  | a /= b = [a,b] ++ []

fst' (x, y) = x

snd' (x, y) = y

droplast' xs = reverse $ tail $ reverse xs

interspersee x [y] = [y,x]
interspersee x (y:ys) = [y,x] ++ (interspersee x ys)

intersperse' _ (x:[]) = [x]
intersperse' a (x:xs) = [x,a] ++ (intersperse' a xs)