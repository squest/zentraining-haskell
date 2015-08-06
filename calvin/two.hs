-- primes (infinite list of prime numbers, without using any function as described in the video)
-- fibos => [1,2,3,5..] 
-- primorials
-- factorials
factorials = (scanl1 (*) [1..])

-- triangular numbers
triangulars = (scanl1 (+) [1..])

-- pentagonal numbers
-- pascal => [[1],[1,1],[1,2,1]...]
-- powerOf (infinite list of powerOf i)  ex. powerOf 2 = [1,2,4,8,16,32...]
powerOf n = (scanl (*) 1 (repeat n))

-- sumPrimes => all possible summation of consecutive primes
-- palindromes (starting from 1) => [1,2,3..11,22,33..101,111,121...]
