module One where
import Data.List

square x = x*x
tiger = "knee"
-- 1A. REIMPLEMENT TODDLER STUFFS 

--null'
null' [] = True
null' (x:xs) = False

--take
take' _ [] = []
take' 0 _ = []
take' n (x:xs) = x :take' (n-1) xs

--drop
drop' _ [] = []
drop' 0 (x:xs) = (x:xs)
drop' n (x:xs) = drop (n-1) xs

--fst
fst' (a,b) = a

--snd
snd' (a,b) = b

--map
map' f [] = []
map' f (x:xs) = (f x) : (map' f xs)

--filter
filter' f [] = []
filter' f (x:xs)
    | f x == True = x:(filter' f xs)
    | f x == False = (filter' f xs)

--delete
delete' n [] = []
delete' n (x:xs)
    | n == x = xs
    | n /= x = x:(delete' n xs)

--deleteAll
deleteAll' n [] = []
deleteAll' n (x:xs)
    | n == x = (deleteAll' n xs)
    | n /= x = x:(deleteAll' n xs)

--foldl
fold' f n [] = n
fold' f n (x:xs) = fold' f (f n x) xs

--foldl1
fold1' f [x] = x
fold1' f (x:xs) = f x (fold1' f xs) 

--zip
zip' [] _ = []
zip' _ [] = []
zip' (x1:xs1) (x2:xs2) = [x1,x2]:zip' xs1 xs2
                         
--zipWith
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x1:xs1) (x2:xs2) = (f x1 x2):zipWith' f xs1 xs2

--(!!) -> ganti jadi nth,
nth' (x:xs) 0 = x
nth' (x:xs) n = nth' xs (n-1)

--scanl
scan' f n [] = [n]
scan' f n (x:xs) = n:scan' f (f n x) xs

--scanl1
scan1' f (x:xs) = scan' f x xs

--elem
elem' n [] = False
elem' n (x:xs)
  | n == x = True
  | otherwise = elem' n xs
               
--notElem
notElem' n (x:xs)
  | elem' n (x:xs) == True = False
  | otherwise = True

--head
head' (x:xs) = x

--length
length' [] = 0
length' (x:xs) = 1+length' xs

--reverse
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

--last
last' [x] = x
last' (x:xs) = last xs
  
--tail
tail' (x:xs) = xs

--init
init' [x] = []
init' (x:xs) = x:init' xs

--max
max' x y
  | x<y = y
  | otherwise = x

--min
min' x y
  | x>y = y
  | otherwise = x

--concat
concat' [] = []
concat' (x:xs) = x ++ concat' xs

--intersperse (shitty name)
intersperse' _ [] = []
intersperse' _ [x] = [x]
intersperse' n (x:xs) = x:n:intersperse' n xs

--intercalate
intercalate' _ [] = []
intercalate' _ [[]] = []
intercalate' _ [x] = x
intercalate' (y) (x:xs) = x ++ y ++ intercalate' (y) xs

--and
and' [] = True
and' (x:xs)
  | x == False = False
  | otherwise = and' xs

--or
or' [] = False
or' (x:xs)
  | x == True = True
  | otherwise = and' xs

--zip3
zip3' [] _ _ = []
zip3' _ [] _ = []
zip3' _ _ [] = []
zip3' (x1:x1s) (x2:x2s) (x3:x3s) = (x1,x2,x3):zip3' x1s x2s x3s

--sum
sum' [] = 0
sum' (x:xs) = x+sum' xs

--product
product' [] = 1
product' (x:xs) = x*sum' xs

--words 
words' "" = []
words' (x) 
  | head x == ' ' = words' (tail x)
  | otherwise = takeWhile' (\a -> (a /= ' ')) x: words' (dropWhile' (\a -> (a /= ' ')) x)

--lines
lines' "" = []
lines' (x) = takeWhile' (\a -> (a /= '\n')) x: lines' (tail (dropWhile' (\a -> (a /= '\n')) x))

--unwords
unwords' (x) = intercalate' " " x

--unlines
unlines' [] = ""
unlines' (x) = intercalate' "\n" x ++ "\n"
 
--takeWhile
takeWhile' _ [] = []
takeWhile' f (x:xs)
  | f x == True = x:takeWhile' f xs
  | otherwise = []
                
--dropWhile
dropWhile' _ [] = []
dropWhile' f (x:xs)
  | f x == True = dropWhile' f xs
  | otherwise = x:xs

--concatMap

--all
all' _ [] = True
all' f (x:xs)
  | f x == True = all' f xs
  | otherwise = False

--any
any' _ [] = False
any' f (x:xs)
  | f x == False = any' f xs
  | otherwise = True

--insert
insert' n [] = [n]
insert' n (x:xs)
  | n < x = n:x:xs
  | otherwise = x:insert' n xs
    
--zipWith3
zipWith3' _ [] _ _ = []
zipWith3' _ _ [] _ = []
zipWith3' _ _ _ [] = []
zipWith3' f (x1:x1s) (x2:x2s) (x3:x3s) = (f x1 x2 x3):zipWith3' f x1s x2s x3s

-- 1B. REIMPLEMENT A BIT GROWN TODDLER STUFFS

-- nub
nub' [] = []
nub' (x:xs) = x:nub' (deleteAll' x xs)

-- sort
sort' [] = []
sort' (x) = minimum' x: sort' (delete' (minimum' x) x)

-- minimum
minimum' [x] = x
minimum' (x:xs) = min' x (minimum' xs)

-- maximum
maximum' [x] = x
maximum' (x:xs) = max' x (maximum' xs)

-- inits
inits' [] = [[]]
inits' (x) = (inits' (init' x)) ++ [x] 

-- tails
tails' [] = [[]]
tails' (x) = x:tails' (tail' x)

-- union
union' [] (y) = y
union' (x) [] = x
union' (x) (y:ys)
  | any' (\a -> a==y) x = union' (x) ys
  | otherwise =  union' (x++[y]) ys

-- intersect
-- group
group' [] = []
group' (x:xs) = takeWhile' (\a -> a==x) (x:xs) :group' (dropWhile' (\a -> a==x) (x:xs))

-- splitAt
splitAt' n (x) = (take' n x):(drop' n x):[]

-- partition
-- replicate
replicate' 0 _ = []
replicate' n (x) = x:replicate' (n-1) x

-- 2. LAZIES

-- iterate
iterate' f n = n:iterate' f (f n)

-- repeat
repeat' n = n:repeat' n

-- cycle
cycle' x = x++cycle' x

-- 3. CRYSTAL METH STUFFS

-- isPrime
-- primesUnder
-- fibo
-- fiboList
-- pascal
-- pascalAll
-- fak
fak' 0 = 1
fak' n = n * fak' (n-1)

-- fakList
fakList' [] = []
fakList' x = map fak' x

-- 4. BONUSES 
-- subsequences
-- permutations
