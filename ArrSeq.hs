module ArrSeq (Arr) where 

import Prelude hiding (length)
import Arr
import Seq
import Par 

instance Seq Arr where
    emptyS = empty
    singletonS x = Arr.fromList [x]
    lengthS = length
    nthS = (!)
    tabulateS = tabulate 
    mapS = myMapS    
    filterS = myFilterS
    appendS = myAppendS
    takeS = myTakeS 
    dropS = myDropS 
    showtS = myShowtS
    showlS = myShowlS
    joinS = flatten
    reduceS = myReduceS
    scanS = myScanS
    fromList = Arr.fromList

getElement :: Arr a -> (a -> b) -> Int -> b
getElement a f n = f (a ! n) 

myMapS f xs = tabulate (getElement xs f) (length xs)

getAppendedElement :: Arr a -> Arr a -> Int -> a
getAppendedElement xs ys n
    | n < lxs = xs ! n
    | otherwise = ys ! (n-lxs)
    where lxs = length xs

myAppendS :: Arr a -> Arr a -> Arr a
myAppendS xs ys = tabulate (getAppendedElement xs ys) (length xs + length ys)

myTakeS :: Arr a -> Int -> Arr a
myTakeS xs n = subArray 0 n xs

myDropS :: Arr a -> Int -> Arr a
myDropS xs n = subArray n (length xs - n) xs

myFilterS :: (a -> Bool) -> Arr a -> Arr a
myFilterS f xs 
    | n == 0 = empty
    | n == 1 = if f (xs ! 0) then xs else empty
    | otherwise = filteredL 
    where 
        n = length xs
        k = n `div` 2
        l = myTakeS xs k 
        r = myDropS xs k
        (filteredL, filteredR) = myFilterS f l ||| myFilterS f r

myShowtS :: Arr a -> TreeView a (Arr a)
myShowtS xs
    | n == 0 = EMPTY
    | n == 1 = ELT (xs ! 0)
    | otherwise = NODE l r
    where
        n = length xs
        k = n `div` 2
        (l, r) = myTakeS xs k ||| myDropS xs (n-k)
  
myShowlS :: Arr a -> ListView a (Arr a)
myShowlS xs
    | n == 0 = NIL
    | otherwise = CONS x xs'
    where
        n = length xs
        x = xs ! 0
        xs' = myDropS xs 1

ceilDivTwo :: Int -> Int
ceilDivTwo n =  n `div` 2 + mod n 2 

inPair :: (a -> a -> a) -> Arr a -> Int -> Int -> a
inPair f xs n k
    | dk == (n - 1) = xs ! (n - 1) 
    | otherwise = f (xs ! dk) (xs ! (dk+1)) 
    where dk = 2*k

contract :: (a -> a -> a) -> Arr a -> Arr a
contract f xs = tabulate (inPair f xs n) (ceilDivTwo n)
    where n = length xs

myReduceS :: (a -> a -> a) -> a -> Arr a -> a
myReduceS f b xs 
    | n == 0 = b
    | n == 1 = b `f` (xs ! 0)
    | n  > 1 = myReduceS f b reduced
    where 
        n = length xs
        reduced = contract f xs 

comparate :: (a -> a -> a) -> a -> Arr a -> Arr a -> Int -> a
comparate f b s s' 0 = b
comparate f b s s' n 
    | even n = s' ! (n `div` 2)
    | otherwise = f (s' ! (n `div` 2)) (s ! (n-1))

myScanS :: (a -> a -> a) -> a -> Arr a -> (Arr a,a)
myScanS f b s 
    | n == 0 = (empty,b)
    | n == 1 = (Arr.fromList [b], b `f` (s ! 0))
    | n > 1 = (s'', t)
    where
        n = length s
        s' = contract f s
        (r,t) = myScanS f b s'
        s'' = tabulate (comparate f b s r) n

