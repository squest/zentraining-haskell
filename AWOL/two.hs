import Data.List

--primes (infinite list of prime numbers, without using any function as described in the video)


--fibos => [1,2,3,5..]
fibo = 1 : 2 : zipWith (+) fibo (tail fibo)

--primorials

--factorials
factorials x
  | x == 1 = 1
  | otherwise = x * factorials (pred x)

--triangular numbers n(n+1)/2




--pentagonal numbers n(3n - 1) / 2


--pascal => [[1],[1,1],[1,2,1]...]

--powerOf (infinite list of powerOf i)
power a n
  | n == 0 = 1
  | n == 1 = a
  | otherwise = a * power a (pred n)

powerOf x = zipWith power (repeat x) [0..]

--powerOf' x = zipWith (*) [1..] [1..] ++ zipWith (*) [1..] (tail powerOf' x)

pow x = [x] ++ [x * (last (pow x))]

--powerOf' x = foldl1 (*) (take x (repeat 1)) : --foldl1 (*) (take x (repeat 1))


--ex. powerOf 2 = [1,2,4,8,16,32...] power 2 n

--sumPrimes => all possible summation of consecutive primes

--palindromes (starting from 1) => [1,2,3..11,22,33..101,111,121...]
