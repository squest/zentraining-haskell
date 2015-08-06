module Ans where

import Data.List

faktorials = 0 : scanl1 (*) [1..]

oddPrime n = all (\x -> 0 /= rem n x) $ takeWhile (\x -> x*x <= n) [3,5..]

primes = 2 : filter oddPrime [3,5..]

primorials = scanl1 (*) primes

fibo = 1:2:zipWith (+) fibo (tail fibo)

pascal = iterate (\x -> zipWith (+) (0:x) (reverse $ 0:x)) [1]

squares = map (\x -> x*x) [1..]

power2 = iterate (*2) 1

triangles = scanl1 (+) [1..]

































-- save
