module ListSeq where 

import Seq
import Par 


instance Seq [] where
    emptyS = []
    singletonS x = [x]
    lengthS = length
    nthS = (!!)
    tabulateS f n = myTabulateS f n 0 
    mapS = myMapS
    filterS = myFilterS 
    appendS = (++)
    takeS xs n = take n xs
    dropS xs n = drop n xs
    showtS = myShowtS
    showlS = myShowlS
    joinS = concat 
    reduceS = myReduceS
    scanS = myScanS 
    fromList = id

myTabulateS :: (Int -> a) -> Int -> Int -> [a]
myTabulateS f n k 
    | k == n = []
    | k <  n = y:ys
    where (y, ys) = f k ||| myTabulateS f n (k+1)

myMapS :: (a -> b) -> [a] -> [b]
myMapS f [] = []
myMapS f (x:xs) = y:ys
    where (y, ys) = f x ||| myMapS f xs

myFilterS :: (a -> Bool) -> [a] -> [a]
myFilterS _ [] = []
myFilterS f (x:xs) =
    if y then x:ys
         else ys
    where 
        (y, ys) = f x ||| myFilterS f xs

myShowtS :: [a] -> TreeView a [a]
myShowtS [] = EMPTY
myShowtS [x] = ELT x
myShowtS list@(x:xs) = NODE l r
    where
        n = length list
        k = n `div` 2
        (l, r) = take k xs ||| drop (n-k) xs
        
myShowlS :: [a] -> ListView a [a]
myShowlS [] = NIL
myShowlS (x:xs) = CONS x xs 

contract :: (a -> a -> a) -> [a] -> [a]
contract _ [] = []
contract _ [x] = [x]
contract f (x:y:xs) = l : r
    where
        (l,r) = f x y ||| contract f xs

myReduceS :: (a -> a -> a) -> a -> [a] -> a
myReduceS f b [] = b
myReduceS f b [x] = b `f` x
myReduceS f b xs = myReduceS f b xs'
     where xs' = contract f xs


comparate :: (a -> a -> a) -> a -> [a] -> [a] -> Int -> [a]
comparate f b [] _ _ = []
comparate f b [x] xs'' _ = [head xs'']
comparate f b l@(x:y:xs) r@(x'':xs'') n 
    | even n = x'' : comparate f b l r (n+1)
    | otherwise = l' : r'
    where
        (l',r') = f x'' x ||| comparate f b xs xs'' (n+1)

myScanS :: (a -> a -> a) -> a -> [a] -> ([a],a)
myScanS f b [] = ([],b)
myScanS f b [x] = ([b], f b x)
myScanS f b xs = (comparate f b xs xs'' 0, t)
    where 
        xs' = contract f xs
        (xs'', t) = myScanS f b xs'
