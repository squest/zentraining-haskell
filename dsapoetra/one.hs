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
