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
|f x == True = x:(filter' xs)
|f x == False = filter' xs

--delete
--deleteAll
--foldl
--foldl1
--zip
--zipWith (!!) -> ganti jadi nth, 
--sort
--scanl
--scanl1
--elem
--notElem
--head
--length
--reverse
--last
--tail
--init
--max
--min
--concat
--intersperse
--intercalate
--and
--or
--zip3
--sum
--product
--words
--lines
--unlines
--unwords
--takeWhile
--dropWhile
--concatMap
--all
--any
--insert
--zipWith3
