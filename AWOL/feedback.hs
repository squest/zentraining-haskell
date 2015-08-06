module Feedback where

import Data.List

map' f [] = []
map' f (x:xs) = f x:map' f xs


init' [x] = []
init' (x:xs) = x:init' xs

tail' (x:xs) = xs

max' a b
  | a > b = a
  | otherwise = b

maximum' [x] = x
maximum' (x:xs) = max' x (maximum' xs)
