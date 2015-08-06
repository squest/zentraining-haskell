module Data where

import Data.List

-- abstraction using data structure => list & lazy

-- algorithms vs data structures

square x = x*x

listSquare n = take n $ map square [1..]

squares = map square [0..]





-- triangular numbers = 1 + 2 + 3

triangles = scanl1 (+) [0..]

-- fibolist, pascal, faktorials, primorials,
