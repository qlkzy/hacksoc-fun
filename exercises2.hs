--
-- Implement a method to sum all of the integers in a given range.
-- For example
-- sum' 1 10 => 55
--
sum'     :: (Integral a) => a -> a -> a
sum' x y = foldr (+) 0 [x..y]

--
-- Implement a method to find the product of all the integers in a given range.
-- For example
-- prod' 5 10 => 55
--
prod'     :: (Integral a) => a -> a -> a
prod' x y =  foldr (*) 1 [x..y]

--
-- Implement a function which determines the length of any list.
-- For example
-- len' [1,2,3] => 3
-- len' [5,6,7] => 3
-- len' [] => 0
--
len'        :: (Integral a) => [t] -> a
len' []     =   0
len' (x:xs) =  1 + (len' xs)

--
-- Find the last element of a list
-- For example
-- last' [1,2,3] => 3
-- last' [3,2,1] => 1
--
last' :: [t] -> t
last' [x]    = x
last' (x:xs) = last' xs


--
-- Turn two lists into a list of tuples.
-- For example
-- zip' [1,2,3] [4,5,6] => [(1,4), (2,5), (3,6)]
--
zip'       :: [a] -> [b] -> [(a,b)]
zip' [] []         = []
zip' (a:as) (b:bs) = (a, b):(zip' as bs)

--
-- Treating lists as mathematical vectors, find the inner product (a.k.a. the 
-- dot product) of two lists
-- For example
-- innerProd [1,2,3] [4,5,6] => 32
-- innerProd [4,5,6] [1,2,3] => 32
-- innerProd [100,0.2,0.1] [0.1,0.2,0.5] => 10.09
--
innerProd               :: (Num a) => [a] -> [a] -> a
innerProd [] []         = 0
innerProd (x:xs) (y:ys) = (x * y) + (innerProd xs ys)

--
-- Find the largest element in a list
-- For Example
-- maximum' [5,4,3,2,1] => 5
-- maximum' [1,9,2,8,3,4] => 9
--
maximum'        :: (Ord a) => [a] -> a
maximum' [x]    = x
maximum' (x:xs) = if x > maxrest then x else maxrest
    where maxrest = maximum' xs

--
-- Find the smallest element in a list
-- For example
-- minimum' [5,4,3,2,1] => 1
-- minimum' [1,9,2,8,3,4] => 1
--
minimum'        :: (Ord a) => [a] -> a
minimum' [x]    = x
minimum' (x:xs) = if x < minrest then x else minrest
    where minrest = minimum' xs

