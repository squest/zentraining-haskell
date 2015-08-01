module One where
import Data.List

square x = x*x
tiger = "knee"

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

--sort BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
sort' [] = []
-- sort' (x:xs) = 

--scanl
scan' f n [] = [n]
scan' f n (x:xs) = n:scan' f (f n x) xs

--scanl1 DITANGGUHKANNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN
scan1' f [] = []
scan1' f [x] = [x]
scan1' f (x:xs) = f x (scan1' f xs):scan1' f xs

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

--words AOISDOIAJDOIJASOJDOAISJDOIADIOASPJDASJIPDASJIDJSAPODJASOJPOASJADO
-- words' "" = []
-- words' (x:xs)
--   | x == ' ' =
--   | otherwise = x:

--lines

--unlines
--unwords

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
